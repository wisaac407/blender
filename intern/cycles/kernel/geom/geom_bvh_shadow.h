/*
 * Adapted from code Copyright 2009-2010 NVIDIA Corporation,
 * and code copyright 2009-2012 Intel Corporation
 *
 * Modifications Copyright 2011-2013, Blender Foundation.
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
#  include "geom_qbvh_shadow.h"
#endif

/* This is a template BVH traversal function, where various features can be
 * enabled/disabled. This way we can compile optimized versions for each case
 * without new features slowing things down.
 *
 * BVH_INSTANCING: object instancing
 * BVH_HAIR: hair curve rendering
 * BVH_MOTION: motion blur rendering
 *
 */

ccl_device bool BVH_FUNCTION_FULL_NAME(BVH)(KernelGlobals *kg,
                                            const Ray *ray,
                                            Intersection *isect_array,
                                            const uint max_hits,
                                            uint *num_hits)
{
	/* todo:
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

	*num_hits = 0;
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
#endif  /* __KERNEL_SSE2__ */

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
				                                   PATH_RAY_SHADOW,
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
				                                   PATH_RAY_SHADOW,
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
					const uint p_type = type & PRIMITIVE_ALL;

					/* pop */
					nodeAddr = traversalStack[stackPtr];
					--stackPtr;

					/* primitive intersection */
					while(primAddr < primAddr2) {
						kernel_assert(kernel_tex_fetch(__prim_type, primAddr) == type);

						bool hit;

						/* todo: specialized intersect functions which don't fill in
						 * isect unless needed and check SD_HAS_TRANSPARENT_SHADOW?
						 * might give a few % performance improvement */

						switch(p_type) {
							case PRIMITIVE_TRIANGLE: {
								hit = triangle_intersect(kg, &isect_precalc, isect_array, P, PATH_RAY_SHADOW, object, primAddr);
								break;
							}
#if BVH_FEATURE(BVH_MOTION)
							case PRIMITIVE_MOTION_TRIANGLE: {
								hit = motion_triangle_intersect(kg, isect_array, P, dir, ray->time, PATH_RAY_SHADOW, object, primAddr);
								break;
							}
#endif
#if BVH_FEATURE(BVH_HAIR)
							case PRIMITIVE_CURVE:
							case PRIMITIVE_MOTION_CURVE: {
								if(kernel_data.curve.curveflags & CURVE_KN_INTERPOLATE) 
									hit = bvh_cardinal_curve_intersect(kg, isect_array, P, dir, PATH_RAY_SHADOW, object, primAddr, ray->time, type, NULL, 0, 0);
								else
									hit = bvh_curve_intersect(kg, isect_array, P, dir, PATH_RAY_SHADOW, object, primAddr, ray->time, type, NULL, 0, 0);
								break;
							}
#endif
							default: {
								hit = false;
								break;
							}
						}

						/* shadow ray early termination */
						if(hit) {
							/* detect if this surface has a shader with transparent shadows */

							/* todo: optimize so primitive visibility flag indicates if
							 * the primitive has a transparent shadow shader? */
							int prim = kernel_tex_fetch(__prim_index, isect_array->prim);
							int shader = 0;

#ifdef __HAIR__
							if(kernel_tex_fetch(__prim_type, isect_array->prim) & PRIMITIVE_ALL_TRIANGLE)
#endif
							{
								shader = kernel_tex_fetch(__tri_shader, prim);
							}
#ifdef __HAIR__
							else {
								float4 str = kernel_tex_fetch(__curves, prim);
								shader = __float_as_int(str.z);
							}
#endif
							int flag = kernel_tex_fetch(__shader_flag, (shader & SHADER_MASK)*2);

							/* if no transparent shadows, all light is blocked */
							if(!(flag & SD_HAS_TRANSPARENT_SHADOW)) {
								return true;
							}
							/* if maximum number of hits reached, block all light */
							else if(*num_hits == max_hits) {
								return true;
							}

							/* move on to next entry in intersections array */
							isect_array++;
							(*num_hits)++;
#if BVH_FEATURE(BVH_INSTANCING)
							num_hits_in_instance++;
#endif

							isect_array->t = isect_t;
						}

						primAddr++;
					}
				}
#if BVH_FEATURE(BVH_INSTANCING)
				else {
					/* instance push */
					object = kernel_tex_fetch(__prim_object, -primAddr-1);

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

				/* scale isect->t to adjust for instancing */
				for(int i = 0; i < num_hits_in_instance; i++)
					(isect_array-i-1)->t *= t_fac;
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
#endif  /* FEATURE(BVH_INSTANCING) */
	} while(nodeAddr != ENTRYPOINT_SENTINEL);

	return false;
}

ccl_device_inline bool BVH_FUNCTION_NAME(KernelGlobals *kg,
                                         const Ray *ray,
                                         Intersection *isect_array,
                                         const uint max_hits,
                                         uint *num_hits)
{
#ifdef __QBVH__
	if(kernel_data.bvh.use_qbvh) {
		return BVH_FUNCTION_FULL_NAME(QBVH)(kg,
		                                    ray,
		                                    isect_array,
		                                    max_hits,
		                                    num_hits);
	}
	else
#endif
	{
		kernel_assert(kernel_data.bvh.use_qbvh == false);
		return BVH_FUNCTION_FULL_NAME(BVH)(kg,
		                                   ray,
		                                   isect_array,
		                                   max_hits,
		                                   num_hits);
	}
}

#undef BVH_FUNCTION_NAME
#undef BVH_FUNCTION_FEATURES
