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

static void initData(ModifierData *md)
{
	DeltaMushModifierData *dmmd = (DeltaMushModifierData *)md;

	dmmd->dm_flags = MOD_DELTAMUSH_PINBOUNDS;
	dmmd->boundverts = 0;
	dmmd->deltas = NULL;
	dmmd->positions = NULL;
	dmmd->smooth_weights = NULL;
	dmmd->lambda = 0.5f;
	dmmd->repeat = 5;
	dmmd->defgrp_name[0] = '\0';
}


static void copyData(ModifierData *md, ModifierData *target)
{
	DeltaMushModifierData *dmmd = (DeltaMushModifierData *)md;
	DeltaMushModifierData *t_dmmd = (DeltaMushModifierData *)target;
	modifier_copyData_generic(md, target);
	if (dmmd->deltas) {
		t_dmmd->deltas = MEM_dupallocN(dmmd->deltas);
	}
	if (dmmd->smooth_weights) {
		t_dmmd->smooth_weights = MEM_dupallocN(dmmd->smooth_weights);
	}
	if (dmmd->positions) {
		t_dmmd->positions = MEM_dupallocN(dmmd->positions);
	}
}


static void freeBind(DeltaMushModifierData * dmmd)
{
	if (dmmd->deltas) {
		MEM_freeN(dmmd->deltas);
		dmmd->deltas = NULL;
	}
	if (dmmd->smooth_weights) {
		MEM_freeN(dmmd->smooth_weights);
		dmmd->smooth_weights = NULL;
	}
	if (dmmd->positions) {
		MEM_freeN(dmmd->positions);
		dmmd->positions = NULL;
	}
	dmmd->boundverts = 0;
}


static void freeData(ModifierData *md)
{
	DeltaMushModifierData *dmmd = (DeltaMushModifierData *) md;
	freeBind(dmmd);
}


static CustomDataMask requiredDataMask(Object *UNUSED(ob), ModifierData *md)
{
	DeltaMushModifierData *dmmd = (DeltaMushModifierData *) md;
	CustomDataMask dataMask = 0;
	/* ask for vertex groups if we need them */
	if (dmmd->defgrp_name[0]) {
		dataMask |= CD_MASK_MDEFORMVERT;
	}
	return dataMask;
}


/* check individual weights for changes and cache values */
static void update_weights(Object *ob, DeltaMushModifierData *dmmd, DerivedMesh *dm)
{
	MDeformVert *dvert = NULL;
	float w;
	int defgrp_index, i;
	int numVerts = dm->getNumVerts(dm);

	modifier_get_vgroup(ob, dm, dmmd->defgrp_name, &dvert, &defgrp_index);
	if (!dmmd->smooth_weights && dvert) {
		dmmd->smooth_weights = MEM_callocN(sizeof(float) * (size_t) numVerts, "delta mush weight cache");
	}
	else if (dmmd->smooth_weights && !dvert) {
		MEM_freeN(dmmd->smooth_weights);
		dmmd->smooth_weights = NULL;
	}
	if (dmmd->smooth_weights && dvert) {
		for (i = 0; i < numVerts; i++, dvert++) {
			w = defvert_find_weight(dvert, defgrp_index);
			if (dmmd->smooth_weights[i] != w) {
				dmmd->smooth_weights[i] = w;
				if (dmmd->deltas) {
					MEM_freeN(dmmd->deltas);
					dmmd->deltas = NULL;
				}
			}
		}
	}
}


static void find_boundaries(DerivedMesh *dm, short *adjacent_counts)
{
	MPoly *polys = dm->getPolyArray(dm);
	MLoop *loops = dm->getLoopArray(dm);
	MEdge *edges = dm->getEdgeArray(dm);
	int numFaces, numLoops, numEdges, i, j;
	numEdges = dm->getNumEdges(dm);
	numFaces = dm->getNumPolys(dm);
	/* count the number of adjacent faces */
	for (i = 0; i < numFaces; i++) {
		MPoly *p = &polys[i];
		numLoops = p->totloop;
		for (j = 0; j < numLoops; j++) {
			adjacent_counts[loops[p->loopstart + j].v]++;
		}
	}
	/* subtract one from the count for each connected edge - if th count ends up as zero, edge is not a boundary */
	/* this may also consider some other non-manifold edges as boundaries */
	for (i = 0; i < numEdges; i++) {
		adjacent_counts[edges[i].v1]--;
		adjacent_counts[edges[i].v2]--;
	}
}


typedef struct SmoothingData {
	float delta[3];
	float edge_lengths;
	float edge_count;
} SmoothingData;


static void smooth_iter(
        DeltaMushModifierData *dmmd, DerivedMesh *dm, float(*vertexCos)[3], unsigned int numVerts,
        short *boundaries, SmoothingData *smooth_data)
{
	const unsigned int numEdges = (unsigned int)dm->getNumEdges(dm);
	const MEdge *edges = dm->getEdgeArray(dm);
	unsigned int i;

	for (i = 0; i < numEdges; i++) {
		float edge_dir[3];
		float dist;

		sub_v3_v3v3(edge_dir, vertexCos[edges[i].v2], vertexCos[edges[i].v1]);
		dist = len_v3(edge_dir);

		mul_v3_fl(edge_dir, dist);
		add_v3_v3(smooth_data[edges[i].v1].delta, edge_dir);
		smooth_data[edges[i].v1].edge_lengths += dist;
		smooth_data[edges[i].v1].edge_count += 1.0f;

		sub_v3_v3(smooth_data[edges[i].v2].delta, edge_dir);
		smooth_data[edges[i].v2].edge_lengths += dist;
		smooth_data[edges[i].v2].edge_count += 1.0f;
	}

	for (i = 0; i < numVerts; i++) {
		if (smooth_data[i].edge_lengths * smooth_data[i].edge_count > FLT_EPSILON * 10.0f) {
			float w = 1.0f;
			if (dmmd->smooth_weights) {
				w = dmmd->smooth_weights[i];
			}
			if (boundaries) {
				w =  w * (boundaries[i] != 0 ? 0.0f : 1.0f);
			}
			mul_v3_fl(smooth_data[i].delta, w * dmmd->lambda /
			          (smooth_data[i].edge_lengths * smooth_data[i].edge_count));
			add_v3_v3(vertexCos[i], smooth_data[i].delta);
		}
	}
}


static void smooth_verts(
        DeltaMushModifierData *dmmd, DerivedMesh *derivedData,
        float(*vertexCos)[3], unsigned int numVerts)
{
	SmoothingData *smooth_data;
	short *boundaries = NULL;
	int i;

	if (dmmd->dm_flags & MOD_DELTAMUSH_PINBOUNDS) {
		boundaries = MEM_callocN((size_t)numVerts * sizeof(short), "delta mush boundary data");
		find_boundaries(derivedData, boundaries);
	}

	smooth_data = MEM_mallocN((size_t)numVerts * sizeof(SmoothingData), "delta mush smoothing data");
	for (i = 0; i < dmmd->repeat; i++) {
		memset(smooth_data, 0, (size_t)numVerts * sizeof(SmoothingData));
		smooth_iter(dmmd, derivedData, vertexCos, numVerts, boundaries, smooth_data);
	}
	MEM_freeN(smooth_data);

	if (boundaries) {
		MEM_freeN(boundaries);
	}
}


static void calc_loop_axis(
        const MLoop *l_prev,
        const MLoop *l_curr,
        const MLoop *l_next,
        float(*vertexCos)[3],
        float r_tspace[3][3])
{
	float v_prev[3];
	float v_next[3];

	sub_v3_v3v3(v_prev, vertexCos[l_prev->v], vertexCos[l_curr->v]);
	sub_v3_v3v3(v_next, vertexCos[l_curr->v], vertexCos[l_next->v]);

	normalize_v3(v_prev);
	normalize_v3(v_next);
	add_v3_v3v3(r_tspace[1], v_prev, v_next);

	if (compare_v3v3(v_prev, v_next, FLT_EPSILON * 10.0f) == false) {
		float nor[3];
		float weight = fabsf(acosf(dot_v3v3(v_next, v_prev)));

		cross_v3_v3v3(nor, v_prev, v_next);
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
	const unsigned int mvert_num = (unsigned int)dm->getNumVerts(dm);
	const MPoly *mpoly = dm->getPolyArray(dm);
	const MLoop *mloop = dm->getLoopArray(dm);
	unsigned int i;

	for (i = 0; i < mpoly_num; i++) {
		const MPoly *mp = &mpoly[i];
		const unsigned int l_term = (unsigned int)(mp->loopstart + mp->totloop);
		unsigned int l_prev, l_curr, l_next;

		for (l_prev = l_term - 2, l_curr = l_term - 1, l_next = (unsigned int)mp->loopstart;
		     l_next < l_term;
		     l_prev = l_curr, l_curr = l_next, l_next++)
		{
			float (*ts)[3] = r_tangent_spaces[mloop[l_curr].v];
			calc_loop_axis(&mloop[l_prev], &mloop[l_curr], &mloop[l_next], vertexCos, ts);
		}
	}

	for (i = 0; i < mvert_num; i++) {
		float (*ts)[3] = r_tangent_spaces[i];
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
}


static void calc_deltas(
        DeltaMushModifierData *dmmd, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts)
{
	float(*smooth_vertex_cos)[3] = MEM_dupallocN(vertexCos);
	float(*tangent_spaces)[3][3];
	unsigned int i;

	tangent_spaces = MEM_callocN((size_t)(numVerts) * sizeof(float[3][3]), "delta mush tangents");
	dmmd->boundverts = numVerts;
	/* allocate deltas if they have not yet been allocated, otheriwse we will just write over them */
	if (!dmmd->deltas) {
		dmmd->deltas = MEM_mallocN((size_t)(numVerts * 3) * sizeof(float), "delta mush deltas");
	}

	smooth_verts(dmmd, dm, smooth_vertex_cos, numVerts);

	calc_tangent_spaces(dm, smooth_vertex_cos, tangent_spaces);

	for (i = 0; i < numVerts; i++) {
		float imat[3][3], delta[3];

		sub_v3_v3v3(delta, vertexCos[i], smooth_vertex_cos[i]);
		if (UNLIKELY(!invert_m3_m3(imat, tangent_spaces[i]))) {
			transpose_m3_m3(imat, tangent_spaces[i]);
		}
		mul_v3_m3v3(dmmd->deltas[i], imat, delta);
	}

	MEM_freeN(smooth_vertex_cos);
	MEM_freeN(tangent_spaces);
}


static void deltamushmodifier_do(
        ModifierData *md, Object *ob, DerivedMesh *dm,
        float(*vertexCos)[3], unsigned int numVerts)
{
	DeltaMushModifierData *dmmd = (DeltaMushModifierData *)md;
	const bool use_bind = (dmmd->dm_flags & MOD_DELTAMUSH_BIND) != 0;
	const bool use_only_smooth = (dmmd->dm_flags & MOD_DELTAMUSH_SHOWSMOOTH)  != 0;

	if (UNLIKELY(use_bind == false)) {
		freeBind(dmmd);
		update_weights(ob, dmmd, dm);
		smooth_verts(dmmd, dm, vertexCos, numVerts);
		MEM_SAFE_FREE(dmmd->smooth_weights);
		return;
	}

	/* if rest positions not are defined, set them */
	if (!dmmd->positions) {
		dmmd->positions = MEM_dupallocN(vertexCos);
		dmmd->boundverts = numVerts;
		if (!dmmd->positions) {
			return;
		}
	}

	/* If the number of verts has changed, the bind is invalid, so we do nothing */
	if (dmmd->boundverts != numVerts) {
		modifier_setError(md, "Verts changed from %d to %d", dmmd->boundverts, numVerts);
		return;
	}

	/* check to see if our deltas are still valid */
	update_weights(ob, dmmd, dm);
	if (!dmmd->deltas) {
		calc_deltas(dmmd, dm, dmmd->positions, numVerts);
	}

	/* this could be a check, but at this point it _must_ be valid */
	BLI_assert(dmmd->boundverts == numVerts && dmmd->deltas);


	/* do the actual delta mush */
	smooth_verts(dmmd, dm, vertexCos, numVerts);

	if (!use_only_smooth) {
		unsigned int i;

		float(*tangent_spaces)[3][3];

		/* calloc, since values are accumulated */
		tangent_spaces = MEM_callocN((size_t)(numVerts) * sizeof(float[3][3]), "delta mush tangents");

		calc_tangent_spaces(dm, vertexCos, tangent_spaces);

		for (i = 0; i < numVerts; i++) {
			float delta[3];
			mul_v3_m3v3(delta, tangent_spaces[i], dmmd->deltas[i]);
			add_v3_v3(vertexCos[i], delta);
		}

		MEM_freeN(tangent_spaces);
	}
}


static void deformVerts(
        ModifierData *md, Object *ob, DerivedMesh *derivedData,
        float(*vertexCos)[3], int numVerts, ModifierApplyFlag UNUSED(flag))
{
	DerivedMesh *dm = get_dm(ob, NULL, derivedData, NULL, false, false);

	deltamushmodifier_do(md, ob, dm, vertexCos, (unsigned int)numVerts);

	if (dm != derivedData) {
		dm->release(dm);
	}
}


static void deformVertsEM(
        ModifierData *md, Object *ob, struct BMEditMesh *editData,
        DerivedMesh *derivedData, float(*vertexCos)[3], int numVerts)
{
	DerivedMesh *dm = get_dm(ob, editData, derivedData, NULL, false, false);

	deltamushmodifier_do(md, ob, dm, vertexCos, (unsigned int)numVerts);

	if (dm != derivedData) {
		dm->release(dm);
	}
}


ModifierTypeInfo modifierType_DeltaMush = {
		/* name */              "DeltaMush",
		/* structName */        "DeltaMushModifierData",
		/* structSize */        sizeof(DeltaMushModifierData),
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
