/*
 * Adapted from code Copyright 2009-2010 NVIDIA Corporation,
 * and code copyright 2009-2012 Intel Corporation
 *
 * Modifications Copyright 2011-2014, Blender Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifdef __QBVH__
#include "geom_qbvh_volume_all.h"
#endif

/* This is a template BVH traversal function for volumes, where
 * various features can be enabled/disabled. This way we can compile optimized
 * versions for each case without new features slowing things down.
 *
 * BVH_INSTANCING: object instancing
 * BVH_MOTION: motion blur rendering
 *
 */

ccl_device uint BVH_FUNCTION_FULL_NAME(BVH)(KernelGlobals *kg,
                                            const Ray *ray,
                                            Intersection *isect_array,
                                            const uint max_hits,
                                            const uint visibility)
{
	/* todo:
	 * - test if pushing distance on the stack helps (for non shadow rays)
	 * - separate version for shadow rays
	 * - likely and unlikely for if() statements
	 * - test restrict attribute for pointers
	 */

	/* traversal stack in CUDA thread-local memory */
	int traversalStack[BVH_STACK_SIZE];
	traversalStack[0] = ENTRYPOINT_SENTINEL;

	/* traversal variables in registers */
	int stackPtr = 0;
	int nodeAddr = kernel_data.bvh.root;

	/* ray parameters in registers */
	const float tmax = ray->t;
	float3 P = ray->P;
	float3 dir = bvh_clamp_direction(ray->D);
	float3 idir = bvh_inverse_direction(dir);
	int object = OBJECT_NONE;
	float isect_t = tmax;

#if BVH_FEATURE(BVH_MOTION)
	Transform ob_itfm;
#endif

#if BVH_FEATURE(BVH_INSTANCING)
	int num_hits_in_instance = 0;
#endif

	uint num_hits = 0;
	isect_array->t = tmax;

#if defined(__KERNEL_SSE2__)
	const shuffle_swap_t shuf_identity = shuffle_swap_identity();
	const shuffle_swap_t shuf_swap = shuffle_swap_swap();
	
	const ssef pn = cast(ssei(0, 0, 0x80000000, 0x80000000));
	ssef Psplat[3], idirsplat[3];
	ssef tnear(0.0f), tfar(isect_t);
	shuffle_swap_t shufflexyz[3];

	Psplat[0] = ssef(P.x);
	Psplat[1] = ssef(P.y);
	Psplat[2] = ssef(P.z);

	ssef tsplat(0.0f, 0.0f, -isect_t, -isect_t);

	gen_idirsplat_swap(pn, shuf_identity, shuf_swap, idir, idirsplat, shufflexyz);
#endif

	IsectPrecalc isect_precalc;
	triangle_intersect_precalc(dir, &isect_precalc);

	/* traversal loop */
	do {
		do {
			/* traverse internal nodes */
			while(nodeAddr >= 0 && nodeAddr != ENTRYPOINT_SENTINEL) {
				int nodeAddrChild1, traverse_mask;
				float dist[2];
				float4 cnodes = kernel_tex_fetch(__bvh_nodes, nodeAddr+0);

#if !defined(__KERNEL_SSE2__)
				traverse_mask = bvh_node_intersect(kg,
				                                   P,
				                                   dir,
				                                   idir,
				                                   isect_t,
				                                   visibility,
				                                   nodeAddr,
				                                   dist);
#else // __KERNEL_SSE2__
				traverse_mask = bvh_node_intersect(kg,
				                                   P,
				                                   dir,
				                                   tnear,
				                                   tfar,
				                                   tsplat,
				                                   Psplat,
				                                   idirsplat,
				                                   shufflexyz,
				                                   visibility,
				                                   nodeAddr,
				                                   dist);
#endif // __KERNEL_SSE2__

				nodeAddr = __float_as_int(cnodes.z);
				nodeAddrChild1 = __float_as_int(cnodes.w);

				if(traverse_mask == 3) {
					/* Both children were intersected, push the farther one. */
					bool closestChild1 = (dist[1] < dist[0]);

					if(closestChild1) {
						int tmp = nodeAddr;
						nodeAddr = nodeAddrChild1;
						nodeAddrChild1 = tmp;
					}

					++stackPtr;
					kernel_assert(stackPtr < BVH_STACK_SIZE);
					traversalStack[stackPtr] = nodeAddrChild1;
				}
				else {
					/* One child was intersected. */
					if(traverse_mask == 2) {
						nodeAddr = nodeAddrChild1;
					}
					else if(traverse_mask == 0) {
						/* Neither child was intersected. */
						nodeAddr = traversalStack[stackPtr];
						--stackPtr;
					}
				}
			}

			/* if node is leaf, fetch triangle list */
			if(nodeAddr < 0) {
				float4 leaf = kernel_tex_fetch(__bvh_leaf_nodes, (-nodeAddr-1));
				int primAddr = __float_as_int(leaf.x);

#if BVH_FEATURE(BVH_INSTANCING)
				if(primAddr >= 0) {
#endif
					const int primAddr2 = __float_as_int(leaf.y);
					const uint type = __float_as_int(leaf.w);
					bool hit;

					/* pop */
					nodeAddr = traversalStack[stackPtr];
					--stackPtr;

					/* primitive intersection */
					switch(type & PRIMITIVE_ALL) {
						case PRIMITIVE_TRIANGLE: {
							/* intersect ray against primitive */
							for(; primAddr < primAddr2; primAddr++) {
								kernel_assert(kernel_tex_fetch(__prim_type, primAddr) == type);
								/* only primitives from volume object */
								uint tri_object = (object == OBJECT_NONE)? kernel_tex_fetch(__prim_object, primAddr): object;
								int object_flag = kernel_tex_fetch(__object_flag, tri_object);
								if((object_flag & SD_OBJECT_HAS_VOLUME) == 0) {
									continue;
								}
								hit = triangle_intersect(kg, &isect_precalc, isect_array, P, visibility, object, primAddr);
								if(hit) {
									/* Move on to next entry in intersections array. */
									isect_array++;
									num_hits++;
#if BVH_FEATURE(BVH_INSTANCING)
									num_hits_in_instance++;
#endif
									isect_array->t = isect_t;
									if(num_hits == max_hits) {
#if BVH_FEATURE(BVH_INSTANCING)
#  if BVH_FEATURE(BVH_MOTION)
										float t_fac = 1.0f / len(transform_direction(&ob_itfm, dir));
#  else
										Transform itfm = object_fetch_transform(kg, object, OBJECT_INVERSE_TRANSFORM);
										float t_fac = 1.0f / len(transform_direction(&itfm, dir));
#  endif
										for(int i = 0; i < num_hits_in_instance; i++) {
											(isect_array-i-1)->t *= t_fac;
										}
#endif  /* BVH_FEATURE(BVH_INSTANCING) */
										return num_hits;
									}
								}
							}
							break;
						}
#if BVH_FEATURE(BVH_MOTION)
						case PRIMITIVE_MOTION_TRIANGLE: {
							/* intersect ray against primitive */
							for(; primAddr < primAddr2; primAddr++) {
								kernel_assert(kernel_tex_fetch(__prim_type, primAddr) == type);
								/* only primitives from volume object */
								uint tri_object = (object == OBJECT_NONE)? kernel_tex_fetch(__prim_object, primAddr): object;
								int object_flag = kernel_tex_fetch(__object_flag, tri_object);
								if((object_flag & SD_OBJECT_HAS_VOLUME) == 0) {
									continue;
								}
								hit = motion_triangle_intersect(kg, isect_array, P, dir, ray->time, visibility, object, primAddr);
								if(hit) {
									/* Move on to next entry in intersections array. */
									isect_array++;
									num_hits++;
#  if BVH_FEATURE(BVH_INSTANCING)
									num_hits_in_instance++;
#  endif
									isect_array->t = isect_t;
									if(num_hits == max_hits) {
#  if BVH_FEATURE(BVH_INSTANCING)
#    if BVH_FEATURE(BVH_MOTION)
										float t_fac = 1.0f / len(transform_direction(&ob_itfm, dir));
#    else
										Transform itfm = object_fetch_transform(kg, object, OBJECT_INVERSE_TRANSFORM);
										float t_fac = 1.0f / len(transform_direction(&itfm, dir));
#    endif
										for(int i = 0; i < num_hits_in_instance; i++) {
											(isect_array-i-1)->t *= t_fac;
										}
#  endif  /* BVH_FEATURE(BVH_INSTANCING) */
										return num_hits;
									}
								}
							}
							break;
						}
#endif  /* BVH_MOTION */
						default: {
							break;
						}
					}
				}
#if BVH_FEATURE(BVH_INSTANCING)
				else {
					/* instance push */
					object = kernel_tex_fetch(__prim_object, -primAddr-1);
					int object_flag = kernel_tex_fetch(__object_flag, object);

					if(object_flag & SD_OBJECT_HAS_VOLUME) {

#  if BVH_FEATURE(BVH_MOTION)
						bvh_instance_motion_push(kg, object, ray, &P, &dir, &idir, &isect_t, &ob_itfm);
#  else
						bvh_instance_push(kg, object, ray, &P, &dir, &idir, &isect_t);
#  endif

						triangle_intersect_precalc(dir, &isect_precalc);
						num_hits_in_instance = 0;
						isect_array->t = isect_t;

#  if defined(__KERNEL_SSE2__)
						Psplat[0] = ssef(P.x);
						Psplat[1] = ssef(P.y);
						Psplat[2] = ssef(P.z);

						tsplat = ssef(0.0f, 0.0f, -isect_t, -isect_t);
						tfar = ssef(isect_t);

						gen_idirsplat_swap(pn, shuf_identity, shuf_swap, idir, idirsplat, shufflexyz);
#  endif

						++stackPtr;
						kernel_assert(stackPtr < BVH_STACK_SIZE);
						traversalStack[stackPtr] = ENTRYPOINT_SENTINEL;

						nodeAddr = kernel_tex_fetch(__object_node, object);
					}
					else {
						/* pop */
						object = OBJECT_NONE;
						nodeAddr = traversalStack[stackPtr];
						--stackPtr;
					}
				}
			}
#endif  /* FEATURE(BVH_INSTANCING) */
		} while(nodeAddr != ENTRYPOINT_SENTINEL);

#if BVH_FEATURE(BVH_INSTANCING)
		if(stackPtr >= 0) {
			kernel_assert(object != OBJECT_NONE);

			if(num_hits_in_instance) {
				float t_fac;
#  if BVH_FEATURE(BVH_MOTION)
				bvh_instance_motion_pop_factor(kg, object, ray, &P, &dir, &idir, &t_fac, &ob_itfm);
#  else
				bvh_instance_pop_factor(kg, object, ray, &P, &dir, &idir, &t_fac);
#  endif
				triangle_intersect_precalc(dir, &isect_precalc);
				/* Scale isect->t to adjust for instancing. */
				for(int i = 0; i < num_hits_in_instance; i++) {
					(isect_array-i-1)->t *= t_fac;
				}
			}
			else {
				float ignore_t = FLT_MAX;
#  if BVH_FEATURE(BVH_MOTION)
				bvh_instance_motion_pop(kg, object, ray, &P, &dir, &idir, &ignore_t, &ob_itfm);
#  else
				bvh_instance_pop(kg, object, ray, &P, &dir, &idir, &ignore_t);
#  endif
				triangle_intersect_precalc(dir, &isect_precalc);
			}

			isect_t = tmax;
			isect_array->t = isect_t;

#  if defined(__KERNEL_SSE2__)
			Psplat[0] = ssef(P.x);
			Psplat[1] = ssef(P.y);
			Psplat[2] = ssef(P.z);

			tsplat = ssef(0.0f, 0.0f, -isect_t, -isect_t);
			tfar = ssef(isect_t);

			gen_idirsplat_swap(pn, shuf_identity, shuf_swap, idir, idirsplat, shufflexyz);
#  endif

			object = OBJECT_NONE;
			nodeAddr = traversalStack[stackPtr];
			--stackPtr;
		}
#endif  /* FEATURE(BVH_MOTION) */
	} while(nodeAddr != ENTRYPOINT_SENTINEL);

	return num_hits;
}

ccl_device_inline uint BVH_FUNCTION_NAME(KernelGlobals *kg,
                                         const Ray *ray,
                                         Intersection *isect_array,
                                         const uint max_hits,
                                         const uint visibility)
{
#ifdef __QBVH__
	if(kernel_data.bvh.use_qbvh) {
		return BVH_FUNCTION_FULL_NAME(QBVH)(kg,
		                                    ray,
		                                    isect_array,
		                                    max_hits,
		                                    visibility);
	}
	else
#endif
	{
		kernel_assert(kernel_data.bvh.use_qbvh == false);
		return BVH_FUNCTION_FULL_NAME(BVH)(kg,
		                                   ray,
		                                   isect_array,
		                                   max_hits,
		                                   visibility);
	}
}

#undef BVH_FUNCTION_NAME
#undef BVH_FUNCTION_FEATURES
