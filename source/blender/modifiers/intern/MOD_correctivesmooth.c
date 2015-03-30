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
* The Original Code is Copyright (C) 2015 by the Blender Foundation.
* All rights reserved.
*
* Contributor(s): Jack Simpson
*
* ***** END GPL LICENSE BLOCK *****
*
*/

#include "DNA_scene_types.h"
#include "DNA_meshdata_types.h"
#include "DNA_object_types.h"

#include "BLI_math.h"
#include "BLI_utildefines.h"

#include "MEM_guardedalloc.h"

#include "BKE_cdderivedmesh.h"
#include "BKE_deform.h"

#include "MOD_modifiertypes.h"
#include "MOD_util.h"

#include "BLI_strict_flags.h"

#define DEBUG_TIME

#include "PIL_time.h"
#ifdef DEBUG_TIME
#  include "PIL_time_utildefines.h"
#endif

/* minor optimization, calculate this inline */
#define USE_TANGENT_CALC_INLINE

static void initData(ModifierData *md)
{
	CorrectiveSmoothModifierData *dmmd = (CorrectiveSmoothModifierData *)md;

	dmmd->flag = MOD_CORRECTIVESMOOTH_PIN_BOUNDARY;
	dmmd->positions = NULL;
	dmmd->positions_num = 0;
	dmmd->positions_delta_cache = NULL;
	dmmd->lambda = 0.5f;
	dmmd->repeat = 5;
	dmmd->defgrp_name[0] = '\0';
}


static void copyData(ModifierData *md, ModifierData *target)
{
	CorrectiveSmoothModifierData *dmmd = (CorrectiveSmoothModifierData *)md;
	CorrectiveSmoothModifierData *t_dmmd = (CorrectiveSmoothModifierData *)target;

	modifier_copyData_generic(md, target);

	if (dmmd->positions) {
		t_dmmd->positions = MEM_dupallocN(dmmd->positions);
	}
}


static void freeBind(CorrectiveSmoothModifierData *dmmd)
{
	MEM_SAFE_FREE(dmmd->positions);
	MEM_SAFE_FREE(dmmd->positions_delta_cache);

	dmmd->positions_num = 0;
}


static void freeData(ModifierData *md)
{
	CorrectiveSmoothModifierData *dmmd = (CorrectiveSmoothModifierData *)md;
	freeBind(dmmd);
}


static CustomDataMask requiredDataMask(Object *UNUSED(ob), ModifierData *md)
{
	CorrectiveSmoothModifierData *dmmd = (CorrectiveSmoothModifierData *)md;
	CustomDataMask dataMask = 0;
	/* ask for vertex groups if we need them */
	if (dmmd->defgrp_name[0]) {
		dataMask |= CD_MASK_MDEFORMVERT;
	}
	return dataMask;
}


/* check individual weights for changes and cache values */
static void dm_get_weights(
        MDeformVert *dvert, const int defgrp_index,
        const unsigned int numVerts, const bool use_invert_vgroup,
        float *smooth_weights)
{
	unsigned int i;

	for (i = 0; i < numVerts; i++, dvert++) {
		const float w = defvert_find_weight(dvert, defgrp_index);

		if (use_invert_vgroup == false) {
			smooth_weights[i] = w;
		}
		else {
			smooth_weights[i] = 1.0f - w;
		}
	}
}


static void dm_get_boundaries(DerivedMesh *dm, float *smooth_weights)
{
	const MPoly *mpoly = dm->getPolyArray(dm);
	const MLoop *mloop = dm->getLoopArray(dm);
	const MEdge *medge = dm->getEdgeArray(dm);
	unsigned int mpoly_num, medge_num, i;
	unsigned short *boundaries;

	mpoly_num = (unsigned int)dm->getNumPolys(dm);
	medge_num = (unsigned int)dm->getNumEdges(dm);

	boundaries = MEM_callocN(medge_num * sizeof(*boundaries), "delta mush boundary data");

	/* count the number of adjacent faces */
	for (i = 0; i < mpoly_num; i++) {
		const MPoly *p = &mpoly[i];
		const int totloop = p->totloop;
		int j;
		for (j = 0; j < totloop; j++) {
			boundaries[mloop[p->loopstart + j].e]++;
		}
	}

	for (i = 0; i < medge_num; i++) {
		if (boundaries[i] == 1) {
			smooth_weights[medge[i].v1] = 0.0f;
			smooth_weights[medge[i].v2] = 0.0f;
		}
	}

	MEM_freeN(boundaries);
}


/* -------------------------------------------------------------------- */
/* Edge-Length Weighted Smoothing
 */

static void smooth_iter__simple(
        CorrectiveSmoothModifierData *dmmd, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts,
        const float *smooth_weights,
        unsigned int iterations)
{
	const float lambda = dmmd->lambda;
	unsigned int i;

	const unsigned int numEdges = (unsigned int)dm->getNumEdges(dm);
	const MEdge *edges = dm->getEdgeArray(dm);
	float *vertex_edge_count_div;

	struct SmoothingData {
		float delta[3];
	} *smooth_data = MEM_callocN((size_t)numVerts * sizeof(*smooth_data), __func__);

	vertex_edge_count_div = MEM_callocN((size_t)numVerts * sizeof(float), __func__);

	/* calculate as floats to avoid int->float conversion in #smooth_iter */
	for (i = 0; i < numEdges; i++) {
		vertex_edge_count_div[edges[i].v1] += 1.0f;
		vertex_edge_count_div[edges[i].v2] += 1.0f;
	}
	for (i = 0; i < numVerts; i++) {
		vertex_edge_count_div[i] = vertex_edge_count_div[i] ? (1.0f / vertex_edge_count_div[i]) : 1.0f;
	}

	/* -------------------------------------------------------------------- */
	/* Main Loop */

	while (iterations--) {
		for (i = 0; i < numEdges; i++) {
			struct SmoothingData *sd_v1;
			struct SmoothingData *sd_v2;
			float edge_dir[3];
			float co1[3];
			float co2[3];

			sub_v3_v3v3(edge_dir, vertexCos[edges[i].v1], vertexCos[edges[i].v2]);

			interp_v3_v3v3(co1, vertexCos[edges[i].v1], vertexCos[edges[i].v2], lambda);
			interp_v3_v3v3(co2, vertexCos[edges[i].v2], vertexCos[edges[i].v1], lambda);

			sd_v1 = &smooth_data[edges[i].v1];
			sd_v2 = &smooth_data[edges[i].v2];

			add_v3_v3(sd_v1->delta, co1);
			add_v3_v3(sd_v2->delta, co2);
		}

		if (smooth_weights == NULL) {
			/* fast-path */
			for (i = 0; i < numVerts; i++) {
				struct SmoothingData *sd = &smooth_data[i];
				mul_v3_v3fl(vertexCos[i], sd->delta, vertex_edge_count_div[i]);
				/* zero for the next iteration (saves memset on entire array) */
				memset(sd, 0, sizeof(*sd));
			}
		}
		else {

			for (i = 0; i < numVerts; i++) {
				struct SmoothingData *sd = &smooth_data[i];

				float lambda_w = lambda * smooth_weights[i];
				mul_v3_fl(sd->delta, vertex_edge_count_div[i]);
				interp_v3_v3v3(vertexCos[i], vertexCos[i], sd->delta, lambda_w);

				memset(sd, 0, sizeof(*sd));
			}
		}
	}

	MEM_freeN(vertex_edge_count_div);
	MEM_freeN(smooth_data);
}


/* -------------------------------------------------------------------- */
/* Simple Weighting
 *
 * (average of surrounding verts)
 */

static void smooth_iter__length_weight(
        CorrectiveSmoothModifierData *dmmd, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts,
        const float *smooth_weights,
        unsigned int iterations)
{
	const float eps = FLT_EPSILON * 10.0f;
	const unsigned int numEdges = (unsigned int)dm->getNumEdges(dm);
	const float lambda = dmmd->lambda;
	const MEdge *edges = dm->getEdgeArray(dm);
	float *vertex_edge_count;
	unsigned int i;

	struct SmoothingData {
		float delta[3];
		float edge_lengths;
	} *smooth_data = MEM_callocN((size_t)numVerts * sizeof(*smooth_data), __func__);


	/* calculate as floats to avoid int->float conversion in #smooth_iter */
	vertex_edge_count = MEM_callocN((size_t)numVerts * sizeof(float), __func__);
	for (i = 0; i < numEdges; i++) {
		vertex_edge_count[edges[i].v1] += 1.0f;
		vertex_edge_count[edges[i].v2] += 1.0f;
	}


	/* -------------------------------------------------------------------- */
	/* Main Loop */

	while (iterations--) {
		for (i = 0; i < numEdges; i++) {
			struct SmoothingData *sd_v1;
			struct SmoothingData *sd_v2;
			float edge_dir[3];
			float edge_dist;

			sub_v3_v3v3(edge_dir, vertexCos[edges[i].v2], vertexCos[edges[i].v1]);
			edge_dist = len_v3(edge_dir);

			/* weight by distance */
			mul_v3_fl(edge_dir, edge_dist);


			sd_v1 = &smooth_data[edges[i].v1];
			sd_v2 = &smooth_data[edges[i].v2];

			add_v3_v3(sd_v1->delta, edge_dir);
			sub_v3_v3(sd_v2->delta, edge_dir);

			sd_v1->edge_lengths += edge_dist;
			sd_v2->edge_lengths += edge_dist;
		}

		if (smooth_weights == NULL) {
			/* fast-path */
			for (i = 0; i < numVerts; i++) {
				struct SmoothingData *sd = &smooth_data[i];
				/* divide by sum of all neighbour distances (weighted) and amount of neighbours, (mean average) */
				const float div = sd->edge_lengths * vertex_edge_count[i];
				if (div > eps) {
#if 0
					/* first calculate the new location */
					mul_v3_fl(sd->delta, 1.0f / div);
					/* then interpolate */
					madd_v3_v3fl(vertexCos[i], sd->delta, lambda);
#else
					/* do this in one step */
					madd_v3_v3fl(vertexCos[i], sd->delta, lambda / div);
#endif
				}
				/* zero for the next iteration (saves memset on entire array) */
				memset(sd, 0, sizeof(*sd));
			}
		}
		else {
			for (i = 0; i < numVerts; i++) {
				struct SmoothingData *sd = &smooth_data[i];
				const float div = sd->edge_lengths * vertex_edge_count[i];
				if (div > eps) {
					const float lambda_w = lambda * smooth_weights[i];
					madd_v3_v3fl(vertexCos[i], sd->delta, lambda_w / div);
				}

				memset(sd, 0, sizeof(*sd));
			}
		}
	}

	MEM_freeN(vertex_edge_count);
	MEM_freeN(smooth_data);
}


static void smooth_iter(
        CorrectiveSmoothModifierData *dmmd, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts,
        const float *smooth_weights,
        unsigned int iterations)
{
	switch (dmmd->smooth_type) {
		case MOD_CORRECTIVESMOOTH_SMOOTH_EDGE_WEIGHT:
			smooth_iter__length_weight(dmmd, dm, vertexCos, numVerts, smooth_weights, iterations);
			break;

		/* case MOD_CORRECTIVESMOOTH_SMOOTH_SIMPLE: */
		default:
			smooth_iter__simple(dmmd, dm, vertexCos, numVerts, smooth_weights, iterations);
			break;
	}
}

static void smooth_verts(
        CorrectiveSmoothModifierData *dmmd, DerivedMesh *dm,
        MDeformVert *dvert, const int defgrp_index,
        float(*vertexCos)[3], unsigned int numVerts)
{
	float *smooth_weights = NULL;

	if (dvert || (dmmd->flag & MOD_CORRECTIVESMOOTH_PIN_BOUNDARY)) {

		smooth_weights = MEM_mallocN(sizeof(float) * numVerts, "delta mush weight cache");

		if (dvert) {
			dm_get_weights(
			        dvert, defgrp_index,
			        numVerts, (dmmd->flag & MOD_CORRECTIVESMOOTH_INVERT_VGROUP) != 0,
			        smooth_weights);
		}
		else {
			fill_vn_fl(smooth_weights, (int)numVerts, 1.0f);
		}

		if (dmmd->flag & MOD_CORRECTIVESMOOTH_PIN_BOUNDARY) {
			dm_get_boundaries(dm, smooth_weights);
		}
	}

	smooth_iter(dmmd, dm, vertexCos, numVerts, smooth_weights, (unsigned int)dmmd->repeat);

	if (smooth_weights) {
		MEM_freeN(smooth_weights);
	}
}

/**
 * finalize after accumulation.
 */
static void calc_tangent_ortho(float ts[3][3])
{
	float v_tan_a[3], v_tan_b[3];
	float t_vec_a[3], t_vec_b[3];

	normalize_v3(ts[2]);

	copy_v3_v3(v_tan_a, ts[0]);
	copy_v3_v3(v_tan_b, ts[1]);

	cross_v3_v3v3(ts[1], ts[2], v_tan_a);
	mul_v3_fl(ts[1], dot_v3v3(ts[1], v_tan_b) < 0.0f ? -1.0f : 1.0f);

	/* orthognalise tangent */
	mul_v3_v3fl(t_vec_a, ts[2], dot_v3v3(ts[2], v_tan_a));
	sub_v3_v3v3(ts[0], v_tan_a, t_vec_a);

	/* orthognalise bitangent */
	mul_v3_v3fl(t_vec_a, ts[2], dot_v3v3(ts[2], ts[1]));
	mul_v3_v3fl(t_vec_b, ts[0], dot_v3v3(ts[0], ts[1]) / dot_v3v3(v_tan_a, v_tan_a));
	sub_v3_v3(ts[1], t_vec_a);
	sub_v3_v3(ts[1], t_vec_b);

	normalize_v3(ts[0]);
	normalize_v3(ts[1]);
}

/**
 * accumulate edge-vectors from all polys.
 */
static void calc_tangent_loop_accum(
        const float v_dir_prev[3],
        const float v_dir_next[3],
        float r_tspace[3][3])
{
	add_v3_v3v3(r_tspace[1], v_dir_prev, v_dir_next);

	if (compare_v3v3(v_dir_prev, v_dir_next, FLT_EPSILON * 10.0f) == false) {
		const float weight = fabsf(acosf(dot_v3v3(v_dir_next, v_dir_prev)));
		float nor[3];

		cross_v3_v3v3(nor, v_dir_prev, v_dir_next);
		normalize_v3(nor);

		cross_v3_v3v3(r_tspace[0], r_tspace[1], nor);

		mul_v3_fl(nor, weight);
		/* accumulate weighted normals */
		add_v3_v3(r_tspace[2], nor);
	}
}


static void calc_tangent_spaces(
        DerivedMesh *dm, float(*vertexCos)[3],
        float (*r_tangent_spaces)[3][3])
{
	const unsigned int mpoly_num = (unsigned int)dm->getNumPolys(dm);
#ifndef USE_TANGENT_CALC_INLINE
	const unsigned int mvert_num = (unsigned int)dm->getNumVerts(dm);
#endif
	const MPoly *mpoly = dm->getPolyArray(dm);
	const MLoop *mloop = dm->getLoopArray(dm);
	unsigned int i;

	for (i = 0; i < mpoly_num; i++) {
		const MPoly *mp = &mpoly[i];
		const MLoop *l_next = &mloop[mp->loopstart];
		const MLoop *l_term = l_next + mp->totloop;
		const MLoop *l_prev = l_term - 2;
		const MLoop *l_curr = l_term - 1;

		/* loop directions */
		float v_dir_prev[3], v_dir_next[3];

		/* needed entering the loop */
		sub_v3_v3v3(v_dir_prev, vertexCos[l_prev->v], vertexCos[l_curr->v]);
		normalize_v3(v_dir_prev);

		for (;
		     l_next != l_term;
		     l_prev = l_curr, l_curr = l_next, l_next++)
		{
			float (*ts)[3] = r_tangent_spaces[l_curr->v];

			/* re-use the previous value */
#if 0
			sub_v3_v3v3(v_dir_prev, vertexCos[l_prev->v], vertexCos[l_curr->v]);
			normalize_v3(v_dir_prev);
#endif
			sub_v3_v3v3(v_dir_next, vertexCos[l_curr->v], vertexCos[l_next->v]);
			normalize_v3(v_dir_next);

			calc_tangent_loop_accum(v_dir_prev, v_dir_next, ts);

			copy_v3_v3(v_dir_prev, v_dir_next);
		}
	}

	/* do inline */
#ifndef USE_TANGENT_CALC_INLINE
	for (i = 0; i < mvert_num; i++) {
		float (*ts)[3] = r_tangent_spaces[i];
		calc_tangent_ortho(ts);
	}
#endif
}


static void calc_deltas(
        CorrectiveSmoothModifierData *dmmd, DerivedMesh *dm,
        MDeformVert *dvert, const int defgrp_index,
        float(*vertexCos)[3], unsigned int numVerts)
{
	float(*smooth_vertex_cos)[3] = MEM_dupallocN(vertexCos);
	float(*tangent_spaces)[3][3];
	unsigned int i;

	tangent_spaces = MEM_callocN((size_t)(numVerts) * sizeof(float[3][3]), "delta mush tangents");
	dmmd->positions_num = numVerts;
	/* allocate deltas if they have not yet been allocated, otheriwse we will just write over them */
	if (!dmmd->positions_delta_cache) {
		dmmd->positions_delta_cache = MEM_mallocN((size_t)numVerts * sizeof(float[3]), "delta mush deltas");
	}

	smooth_verts(dmmd, dm, dvert, defgrp_index, smooth_vertex_cos, numVerts);

	calc_tangent_spaces(dm, smooth_vertex_cos, tangent_spaces);

	for (i = 0; i < numVerts; i++) {
		float imat[3][3], delta[3];

#ifdef USE_TANGENT_CALC_INLINE
		calc_tangent_ortho(tangent_spaces[i]);
#endif

		sub_v3_v3v3(delta, vertexCos[i], smooth_vertex_cos[i]);
		if (UNLIKELY(!invert_m3_m3(imat, tangent_spaces[i]))) {
			transpose_m3_m3(imat, tangent_spaces[i]);
		}
		mul_v3_m3v3(dmmd->positions_delta_cache[i], imat, delta);
	}

	MEM_freeN(smooth_vertex_cos);
	MEM_freeN(tangent_spaces);
}


static void correctivesmooth_modifier_do(
        ModifierData *md, Object *ob, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts)
{
	CorrectiveSmoothModifierData *dmmd = (CorrectiveSmoothModifierData *)md;
	const bool use_bind = (dmmd->flag & MOD_CORRECTIVESMOOTH_BIND) != 0;
	const bool use_only_smooth = (dmmd->flag & MOD_CORRECTIVESMOOTH_ONLY_SMOOTH)  != 0;
	MDeformVert *dvert = NULL;
	int defgrp_index;

	modifier_get_vgroup(ob, dm, dmmd->defgrp_name, &dvert, &defgrp_index);

	if (UNLIKELY(use_bind == false)) {
		freeBind(dmmd);
		smooth_verts(dmmd, dm, dvert, defgrp_index, vertexCos, numVerts);
		return;
	}

	/* if rest positions not are defined, set them (only run during bind) */
	if (!dmmd->positions) {
		dmmd->positions = MEM_dupallocN(vertexCos);
		dmmd->positions_num = numVerts;
		if (!dmmd->positions) {
			return;
		}
	}

	/* If the number of verts has changed, the bind is invalid, so we do nothing */
	if (dmmd->positions_num != numVerts) {
		modifier_setError(md, "Verts changed from %d to %d", dmmd->positions_num, numVerts);
		MEM_SAFE_FREE(dmmd->positions_delta_cache);
		return;
	}

	/* check to see if our deltas are still valid */
	if (!dmmd->positions_delta_cache) {
		calc_deltas(dmmd, dm, dvert, defgrp_index, dmmd->positions, numVerts);
	}

	/* this could be a check, but at this point it _must_ be valid */
	BLI_assert(dmmd->positions_num == numVerts && dmmd->positions_delta_cache);


#ifdef DEBUG_TIME
	TIMEIT_START(corrective_smooth);
#endif

	/* do the actual delta mush */
	smooth_verts(dmmd, dm, dvert, defgrp_index, vertexCos, numVerts);

	if (!use_only_smooth) {
		unsigned int i;

		float(*tangent_spaces)[3][3];

		/* calloc, since values are accumulated */
		tangent_spaces = MEM_callocN((size_t)(numVerts) * sizeof(float[3][3]), "delta mush tangents");

		calc_tangent_spaces(dm, vertexCos, tangent_spaces);

		for (i = 0; i < numVerts; i++) {
			float delta[3];

#ifdef USE_TANGENT_CALC_INLINE
			calc_tangent_ortho(tangent_spaces[i]);
#endif

			mul_v3_m3v3(delta, tangent_spaces[i], dmmd->positions_delta_cache[i]);
			add_v3_v3(vertexCos[i], delta);
		}

		MEM_freeN(tangent_spaces);
	}

#ifdef DEBUG_TIME
	TIMEIT_END(corrective_smooth);
#endif
}


static void deformVerts(
        ModifierData *md, Object *ob, DerivedMesh *derivedData,
        float(*vertexCos)[3], int numVerts, ModifierApplyFlag UNUSED(flag))
{
	DerivedMesh *dm = get_dm(ob, NULL, derivedData, NULL, false, false);

	correctivesmooth_modifier_do(md, ob, dm, vertexCos, (unsigned int)numVerts);

	if (dm != derivedData) {
		dm->release(dm);
	}
}


static void deformVertsEM(
        ModifierData *md, Object *ob, struct BMEditMesh *editData,
        DerivedMesh *derivedData, float(*vertexCos)[3], int numVerts)
{
	DerivedMesh *dm = get_dm(ob, editData, derivedData, NULL, false, false);

	correctivesmooth_modifier_do(md, ob, dm, vertexCos, (unsigned int)numVerts);

	if (dm != derivedData) {
		dm->release(dm);
	}
}


ModifierTypeInfo modifierType_CorrectiveSmooth = {
		/* name */              "CorrectiveSmooth",
		/* structName */        "CorrectiveSmoothModifierData",
		/* structSize */        sizeof(CorrectiveSmoothModifierData),
		/* type */              eModifierTypeType_OnlyDeform,
		/* flags */             eModifierTypeFlag_AcceptsMesh |
		                        eModifierTypeFlag_SupportsEditmode,

		/* copyData */          copyData,
		/* deformVerts */       deformVerts,
		/* deformMatrices */    NULL,
		/* deformVertsEM */     deformVertsEM,
		/* deformMatricesEM */  NULL,
		/* applyModifier */     NULL,
		/* applyModifierEM */   NULL,
		/* initData */          initData,
		/* requiredDataMask */  requiredDataMask,
		/* freeData */          freeData,
		/* isDisabled */        NULL,
		/* updateDepgraph */    NULL,
		/* dependsOnTime */     NULL,
		/* dependsOnNormals */  NULL,
		/* foreachObjectLink */ NULL,
		/* foreachIDLink */     NULL,
		/* foreachTexLink */    NULL,
};
