/*
 * ***** BEGIN GPL LICENSE BLOCK *****
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software  Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Contributor(s): Bastien Montagne
 *
 * ***** END GPL LICENSE BLOCK *****
 *
 */

/** \file blender/modifiers/intern/MOD_setsplitnor.c
 *  \ingroup modifiers
 */

#include <string.h>

#include "MEM_guardedalloc.h"

#include "DNA_object_types.h"
#include "DNA_meshdata_types.h"
#include "DNA_mesh_types.h"

#include "BLI_math.h"
#include "BLI_utildefines.h"
#include "BLI_string.h"

#include "BKE_cdderivedmesh.h"
#include "BKE_mesh.h"
#include "BKE_modifier.h"
#include "BKE_deform.h"
#include "BKE_shrinkwrap.h"       /* For SpaceTransform stuff. */

#include "depsgraph_private.h"

#include "RE_shader_ext.h"

#include "MOD_util.h"


static void initData(ModifierData *md)
{
	SetSplitNormalModifierData *smd = (SetSplitNormalModifierData *) md;

	smd->mode = MOD_SETSPLITNORMAL_ELLIPSOID;
}

static void copyData(ModifierData *md, ModifierData *target)
{
	modifier_copyData_generic(md, target);
}

static CustomDataMask requiredDataMask(Object *UNUSED(ob), ModifierData *md)
{
	SetSplitNormalModifierData *smd = (SetSplitNormalModifierData *)md;
	CustomDataMask dataMask = CD_CUSTOMLOOPNORMAL;

	/* Ask for vertexgroups if we need them. */
	if (smd->defgrp_name[0]) {
		dataMask |= (CD_MASK_MDEFORMVERT);
	}

	return dataMask;
}

static bool dependsOnNormals(ModifierData *UNUSED(md)) {
	return true;
}

static void foreachObjectLink(ModifierData *md, Object *ob, ObjectWalkFunc walk, void *userData)
{
	SetSplitNormalModifierData *smd = (SetSplitNormalModifierData *) md;

	walk(userData, ob, &smd->object_geometry);
	walk(userData, ob, &smd->object_center);
}

static void foreachIDLink(ModifierData *md, Object *ob, IDWalkFunc walk, void *userData)
{
	SetSplitNormalModifierData *smd = (SetSplitNormalModifierData *) md;

	walk(userData, ob, (ID **)&smd->object_geometry);
	walk(userData, ob, (ID **)&smd->object_center);
}

static void updateDepgraph(ModifierData *md, DagForest *forest, struct Scene *UNUSED(scene),
                           Object *UNUSED(ob), DagNode *obNode)
{
	SetSplitNormalModifierData *smd = (SetSplitNormalModifierData *) md;

	if (smd->object_geometry) {
		DagNode *Node = dag_get_node(forest, smd->object_geometry);

		dag_add_relation(forest, Node, obNode, DAG_RL_DATA_DATA | DAG_RL_OB_DATA, "SetSplitNormal Modifier");
	}

	if (smd->object_center) {
		DagNode *Node = dag_get_node(forest, smd->object_center);

		dag_add_relation(forest, Node, obNode, DAG_RL_DATA_DATA | DAG_RL_OB_DATA, "SetSplitNormal Modifier");
	}
}

static void generate_vert_coordinates(DerivedMesh *dm, Object *ob, Object *ob_center, const bool use_bbox_center,
                                      const int num_verts, float (*r_cos)[3], float r_size[3])
{
	float min_co[3] = {FLT_MAX, FLT_MAX, FLT_MAX};
	float max_co[3] = {FLT_MIN, FLT_MIN, FLT_MIN};
	float diff[3];
	bool do_diff = false;
	int i, j;

	dm->getVertCos(dm, r_cos);

	/* Compute min/max's, aka bbox. */
	/* XXX Check we can't get this from object?! */
	i = num_verts;
	while (i--) {
		float *co = r_cos[i];
		j = 3;
		while (j--) {
			if (min_co[j] > co[j]) {
				min_co[j] = co[j];
			}
			if (max_co[j] < co[j]) {
				max_co[j] = co[j];
			}
		}
	}

	/* Set size. */
	sub_v3_v3v3(r_size, max_co, min_co);

	/* Error checks - we do not want one or more of our sizes to be null! */
	if (is_zero_v3(r_size)) {
		r_size[0] = r_size[1] = r_size[2] = 1.0f;
	}
	else if (min_fff(r_size[0], r_size[1], r_size[2]) < FLT_EPSILON) {
		j = 3;
		while (j--) {
			if (r_size[j] < FLT_EPSILON) {
				r_size[j] = FLT_EPSILON;
			}
		}
	}

	if (ob_center) {
		/* Translate our coordinates so that center of obj_center is at (0, 0, 0). */
		float mat[4][4];

		/* Get ob_center coordinates in ob local coordinates. */
		invert_m4_m4(mat, ob_center->obmat);
		mul_m4_m4m4(mat, mat, ob->obmat);
		copy_v3_v3(diff, mat[3]);

		do_diff = true;
	}
	else if (use_bbox_center) {
		/* Translate our coordinates so that center of bounding box is at (0, 0, 0). */

		/* Compute bbox center in local coordinates. */
		add_v3_v3v3(diff, min_co, max_co);
		mul_v3_fl(diff, -0.5f);

		do_diff = true;
	}
	/* Else, no need to change coordinates! */

	if (do_diff) {
		i = num_verts;
		while (i--) {
			add_v3_v3(r_cos[i], diff);
		}
	}
}

static float vertex_weight(MDeformVert *dvert, const int index, const int defgrp_index) {
	return (!dvert || defgrp_index == -1) ? 1.0f :  defvert_find_weight(&dvert[index], defgrp_index);
}

static void setSplitNormalModifier_do(SetSplitNormalModifierData *smd, Object *ob, DerivedMesh *dm)
{
	float (*clnors)[2];
	const int num_verts = dm->getNumVerts(dm);
	const int num_edges = dm->getNumEdges(dm);
	const int num_loops = dm->getNumLoops(dm);
	const int num_polys = dm->getNumPolys(dm);
	MVert *mvert = dm->getVertArray(dm);
	MEdge *medge = dm->getEdgeArray(dm);
	MLoop *mloop = dm->getLoopArray(dm);
	MPoly *mpoly = dm->getPolyArray(dm);

	const bool use_bbox_center = ((smd->flags & MOD_SETSPLITNORMAL_CENTER_BBOX) != 0) && (smd->object_center == NULL);
	const bool use_invert_vgroup = ((smd->flags & MOD_SETSPLITNORMAL_INVERT_VGROUP) != 0);

	int defgrp_index;
	MDeformVert *dvert;

	float (*polynors)[3];
	bool free_polynors = false;

	float (*cos)[3] = MEM_mallocN(sizeof(*cos) * num_verts, __func__);
	float *facs = MEM_mallocN(sizeof(*facs) * num_verts, __func__);

	int i;

	if (!ELEM(smd->mode, MOD_SETSPLITNORMAL_ELLIPSOID, MOD_SETSPLITNORMAL_OBJECT) ||
	    (smd->mode == MOD_SETSPLITNORMAL_OBJECT && !smd->object_geometry))
	{
		return;
	}

	clnors = CustomData_duplicate_referenced_layer(&dm->loopData, CD_CUSTOMLOOPNORMAL, num_loops);
	if (!clnors) {
		DM_add_loop_layer(dm, CD_CUSTOMLOOPNORMAL, CD_CALLOC, NULL);
		clnors = dm->getLoopDataArray(dm, CD_CUSTOMLOOPNORMAL);
	}

	modifier_get_vgroup(ob, dm, smd->defgrp_name, &dvert, &defgrp_index);

	polynors = dm->getPolyDataArray(dm, CD_NORMAL);
	if (!polynors) {
		polynors = MEM_mallocN(sizeof(*polynors) * num_polys, __func__);
		BKE_mesh_calc_normals_poly(mvert, num_verts, mloop, mpoly, num_loops, num_polys, polynors, false);
		free_polynors = true;
	}

	if (smd->mode == MOD_SETSPLITNORMAL_ELLIPSOID) {
		float size[3];

		generate_vert_coordinates(dm, ob, smd->object_center, use_bbox_center, num_verts, cos, size);

		/* size gives us our spheroid coefficients (A, B, C).
		 * Then, we want to find out for each vert its (a, b, c) triple (proportional to (A, B, C) one).
		 *
		 * Ellipsoid basic equation: (x^2/a^2) + (y^2/b^2) + (z^2/c^2) = 1.
		 * Since we want to find (a, b, c) matching this equation and proportional to (A, B, C), we can do:
		 *     m = B / A
		 *     n = C / A
		 * hence:
		 *     (x^2/a^2) + (y^2/b^2) + (z^2/c^2) = 1
		 *  -> b^2*c^2*x^2 + a^2*c^2*y^2 + a^2*b^2*z^2 = a^2*b^2*c^2
		 *     b = ma
		 *     c = na
		 *  -> m^2*a^2*n^2*a^2*x^2 + a^2*n^2*a^2*y^2 + a^2*m^2*a^2*z^2 = a^2*m^2*a^2*n^2*a^2
		 *  -> m^2*n^2*a^4*x^2 + n^2*a^4*y^2 + m^2*a^4*z^2 = m^2*n^2*a^6
		 *  -> a^2 = (m^2*n^2*x^2 + n^2y^2 + m^2z^2) / (m^2*n^2) = x^2 + (y^2 / m^2) + (z^2 / n^2)
		 *  -> b^2 = (m^2*n^2*x^2 + n^2y^2 + m^2z^2) / (n^2)     = (m^2 * x^2) + y^2 + (m^2 * z^2 / n^2)
		 *  -> c^2 = (m^2*n^2*x^2 + n^2y^2 + m^2z^2) / (m^2)     = (n^2 * x^2) + (n^2 * y^2 / m^2) + z^2
		 *
		 * All we have to do now is compute normal of the spheroid at that point:
		 *     n = (x / a^2, y / b^2, z / c^2)
		 * And we are done!
		 */
		{
			const float A = size[0], B = size[1], C = size[2];
			const float m2 = (B * B) / (A * A);
			const float n2 = (C * C) / (A * A);

			/* We reuse cos to now store the ellipsoid-normal of the verts! */
			i = num_verts;
			while (i--) {
				float *co = cos[i];
				facs[i] = use_invert_vgroup ? 1.0f - vertex_weight(dvert, i, defgrp_index) :
				                              vertex_weight(dvert, i, defgrp_index);

				if (facs[i]) {
					const float x2 = co[0] * co[0];
					const float y2 = co[1] * co[1];
					const float z2 = co[2] * co[2];
					const float a2 = x2 + (y2 / m2) + (z2 / n2);
					const float b2 = (m2 * x2) + y2 + (m2 * z2 / n2);
					const float c2 = (n2 * x2) + (n2 * y2 / m2) + z2;
					co[0] /= a2;
					co[1] /= b2;
					co[2] /= c2;
					normalize_v3(co);
				}
				else {
					zero_v3(co);
				}
			}
		}
	}
	else {
		Object *obr = smd->object_geometry;
		DerivedMesh *target_dm = obr->derivedFinal;
		BVHTreeFromMesh treeData = NULL_BVHTreeFromMesh;

		/* Create a bvh-tree of the given target's faces. */
		bvhtree_from_mesh_faces(&treeData, target_dm, 0.0, 2, 6);
		if (treeData.tree != NULL) {
			const int target_num_polys = target_dm->getNumPolys(target_dm);
			BVHTreeNearest nearest = NULL_BVHTreeNearest;

			float (*target_polynors)[3];
			bool free_target_polynors = false;

			SpaceTransform loc2trgt;
			SPACE_TRANSFORM_SETUP(&loc2trgt, ob, obr);

			target_polynors = target_dm->getPolyDataArray(target_dm, CD_NORMAL);
			if (!target_polynors) {
				const int target_num_verts = target_dm->getNumVerts(target_dm);
				const int target_num_loops = target_dm->getNumLoops(target_dm);
				MVert *target_mvert = target_dm->getVertArray(target_dm);
				MLoop *target_mloop = target_dm->getLoopArray(target_dm);
				MPoly *target_mpoly = target_dm->getPolyArray(target_dm);

				target_polynors = MEM_mallocN(sizeof(*target_polynors) * target_num_polys, __func__);
				BKE_mesh_calc_normals_poly(target_mvert, target_num_verts, target_mloop, target_mpoly,
				                           target_num_loops, target_num_polys, target_polynors, false);
				free_target_polynors = true;
			}

			nearest.index = -1;
			dm->getVertCos(dm, cos);

			/* Find the nearest vert/edge/face. */
#ifndef __APPLE__
#pragma omp parallel for default(none) private(i) firstprivate(nearest) \
                         shared(treeData, cos, facs, target_polynors, loc2trgt, dvert, defgrp_index) \
                         schedule(static)
#endif
			for (i = 0; i < num_verts; i++) {
				float tmp_co[3];
				facs[i] = use_invert_vgroup ? 1.0f - vertex_weight(dvert, i, defgrp_index) :
				                              vertex_weight(dvert, i, defgrp_index);

				/* Convert the vertex to tree coordinates. */
				copy_v3_v3(tmp_co, cos[i]);
				space_transform_apply(&loc2trgt, tmp_co);

				/* Use local proximity heuristics (to reduce the nearest search).
				 *
				 * If we already had an hit before, we assume this vertex is going to have a close hit to
				 * that other vertex, so we can initiate the "nearest.dist" with the expected value to that
				 * last hit.
				 * This will lead in prunning of the search tree.
				 */
				nearest.dist_sq = (nearest.index != -1) ? len_squared_v3v3(tmp_co, nearest.co) : FLT_MAX;
				/* Compute and store result. */
				BLI_bvhtree_find_nearest(treeData.tree, tmp_co, &nearest, treeData.nearest_callback, &treeData);

				if (facs[i] && nearest.index != -1) {
					copy_v3_v3(cos[i], target_polynors[nearest.index]);
					/* Bring normal back in own space! */
					space_transform_invert_normal(&loc2trgt, cos[i]);
				}
				else {
					zero_v3(cos[i]);
				}
			}

			free_bvhtree_from_mesh(&treeData);
			if (free_target_polynors) {
				MEM_freeN(target_polynors);
			}
		}
		else {
			memset(cos, 0, sizeof(*cos) * num_verts);
			memset(facs, 0, sizeof(*facs) * num_verts);
		}
	}

	BKE_mesh_normals_loop_custom_from_vertices_set(mvert, cos, facs, num_verts, medge, num_edges, mloop, num_loops,
	                                               mpoly, (const float(*)[3])polynors, num_polys, clnors);

	MEM_freeN(cos);
	MEM_freeN(facs);
	if (free_polynors) {
		MEM_freeN(polynors);
	}
}

static DerivedMesh *applyModifier(ModifierData *md, Object *ob, DerivedMesh *dm, ModifierApplyFlag UNUSED(flag))
{
	setSplitNormalModifier_do((SetSplitNormalModifierData *)md, ob, dm);
	return dm;
}

ModifierTypeInfo modifierType_SetSplitNormal = {
	/* name */              "Set Split Normals",
	/* structName */        "SetSplitNormalModifierData",
	/* structSize */        sizeof(SetSplitNormalModifierData),
	/* type */              eModifierTypeType_Constructive,
	/* flags */             eModifierTypeFlag_AcceptsMesh |
	                        eModifierTypeFlag_AcceptsCVs |
	                        eModifierTypeFlag_SupportsMapping |
	                        eModifierTypeFlag_SupportsEditmode |
	                        eModifierTypeFlag_EnableInEditmode,
	/* copyData */          copyData,
	/* deformVerts */       NULL,
	/* deformMatrices */    NULL,
	/* deformVertsEM */     NULL,
	/* deformMatricesEM */  NULL,
	/* applyModifier */     applyModifier,
	/* applyModifierEM */   NULL,
	/* initData */          initData,
	/* requiredDataMask */  requiredDataMask,
	/* freeData */          NULL,
	/* isDisabled */        NULL,
	/* updateDepgraph */    updateDepgraph,
	/* dependsOnTime */     NULL,
	/* dependsOnNormals */  dependsOnNormals,
	/* foreachObjectLink */ foreachObjectLink,
	/* foreachIDLink */     foreachIDLink,
	/* foreachTexLink */    NULL,
};
