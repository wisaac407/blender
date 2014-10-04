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
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Contributor(s): Blender Foundation
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/blenkernel/intern/mesh_mapping.c
 *  \ingroup bke
 *
 * Functions for accessing mesh connectivity data.
 * eg: polys connected to verts, UV's connected to verts.
 */

#include "MEM_guardedalloc.h"

#include "DNA_meshdata_types.h"

#include "BLI_utildefines.h"
#include "BLI_math.h"

#include "BKE_bvhutils.h"
#include "BKE_customdata.h"
#include "BKE_DerivedMesh.h"
#include "BKE_mesh.h"
#include "BKE_mesh_mapping.h"

#include "BLI_strict_flags.h"


/* -------------------------------------------------------------------- */

/** \name Mesh Connectivity Mapping
 * \{ */


/* ngon version wip, based on BM_uv_vert_map_create */
/* this replaces the non bmesh function (in trunk) which takes MTFace's, if we ever need it back we could
 * but for now this replaces it because its unused. */

UvVertMap *BKE_mesh_uv_vert_map_create(struct MPoly *mpoly, struct MLoop *mloop, struct MLoopUV *mloopuv,
                                       unsigned int totpoly, unsigned int totvert, int selected, float *limit)
{
	UvVertMap *vmap;
	UvMapVert *buf;
	MPoly *mp;
	unsigned int a;
	int i, totuv, nverts;

	totuv = 0;

	/* generate UvMapVert array */
	mp = mpoly;
	for (a = 0; a < totpoly; a++, mp++)
		if (!selected || (!(mp->flag & ME_HIDE) && (mp->flag & ME_FACE_SEL)))
			totuv += mp->totloop;

	if (totuv == 0)
		return NULL;

	vmap = (UvVertMap *)MEM_callocN(sizeof(*vmap), "UvVertMap");
	if (!vmap)
		return NULL;

	vmap->vert = (UvMapVert **)MEM_callocN(sizeof(*vmap->vert) * totvert, "UvMapVert*");
	buf = vmap->buf = (UvMapVert *)MEM_callocN(sizeof(*vmap->buf) * (size_t)totuv, "UvMapVert");

	if (!vmap->vert || !vmap->buf) {
		BKE_mesh_uv_vert_map_free(vmap);
		return NULL;
	}

	mp = mpoly;
	for (a = 0; a < totpoly; a++, mp++) {
		if (!selected || (!(mp->flag & ME_HIDE) && (mp->flag & ME_FACE_SEL))) {
			nverts = mp->totloop;

			for (i = 0; i < nverts; i++) {
				buf->tfindex = (unsigned char)i;
				buf->f = a;
				buf->separate = 0;
				buf->next = vmap->vert[mloop[mp->loopstart + i].v];
				vmap->vert[mloop[mp->loopstart + i].v] = buf;
				buf++;
			}
		}
	}

	/* sort individual uvs for each vert */
	for (a = 0; a < totvert; a++) {
		UvMapVert *newvlist = NULL, *vlist = vmap->vert[a];
		UvMapVert *iterv, *v, *lastv, *next;
		float *uv, *uv2, uvdiff[2];

		while (vlist) {
			v = vlist;
			vlist = vlist->next;
			v->next = newvlist;
			newvlist = v;

			uv = mloopuv[mpoly[v->f].loopstart + v->tfindex].uv;
			lastv = NULL;
			iterv = vlist;

			while (iterv) {
				next = iterv->next;

				uv2 = mloopuv[mpoly[iterv->f].loopstart + iterv->tfindex].uv;
				sub_v2_v2v2(uvdiff, uv2, uv);


				if (fabsf(uv[0] - uv2[0]) < limit[0] && fabsf(uv[1] - uv2[1]) < limit[1]) {
					if (lastv) lastv->next = next;
					else vlist = next;
					iterv->next = newvlist;
					newvlist = iterv;
				}
				else
					lastv = iterv;

				iterv = next;
			}

			newvlist->separate = 1;
		}

		vmap->vert[a] = newvlist;
	}

	return vmap;
}

UvMapVert *BKE_mesh_uv_vert_map_get_vert(UvVertMap *vmap, unsigned int v)
{
	return vmap->vert[v];
}

void BKE_mesh_uv_vert_map_free(UvVertMap *vmap)
{
	if (vmap) {
		if (vmap->vert) MEM_freeN(vmap->vert);
		if (vmap->buf) MEM_freeN(vmap->buf);
		MEM_freeN(vmap);
	}
}

/* Generates a map where the key is the vertex and the value is a list
 * of polys or loops that use that vertex as a corner. The lists are allocated
 * from one memory pool. */
static void bke_mesh_vert_poly_loop_map_create(MeshElemMap **r_map, int **r_mem,
                                               const MPoly *mpoly, const MLoop *mloop,
                                               int totvert, int totpoly, int totloop, const bool do_loops)
{
	MeshElemMap *map = MEM_callocN(sizeof(MeshElemMap) * (size_t)totvert, __func__);
	int *indices, *index_iter;
	int i, j;

	indices = index_iter = MEM_mallocN(sizeof(int) * (size_t)totloop, __func__);

	/* Count number of polys for each vertex */
	for (i = 0; i < totpoly; i++) {
		const MPoly *p = &mpoly[i];

		for (j = 0; j < p->totloop; j++)
			map[mloop[p->loopstart + j].v].count++;
	}

	/* Assign indices mem */
	for (i = 0; i < totvert; i++) {
		map[i].indices = index_iter;
		index_iter += map[i].count;

		/* Reset 'count' for use as index in last loop */
		map[i].count = 0;
	}

	/* Find the users */
	for (i = 0; i < totpoly; i++) {
		const MPoly *p = &mpoly[i];

		for (j = 0; j < p->totloop; j++) {
			unsigned int v = mloop[p->loopstart + j].v;

			map[v].indices[map[v].count] = do_loops ? j + p->totloop : i;
			map[v].count++;
		}
	}

	*r_map = map;
	*r_mem = indices;
}

/**
 * Generates a map where the key is the vertex and the value is a list of polys that use that vertex as a corner.
 * The lists are allocated from one memory pool.
 */
void BKE_mesh_vert_poly_map_create(MeshElemMap **r_map, int **r_mem,
                                   const MPoly *mpoly, const MLoop *mloop,
                                   int totvert, int totpoly, int totloop)
{
	bke_mesh_vert_poly_loop_map_create(r_map, r_mem, mpoly, mloop, totvert, totpoly, totloop, false);
}

/**
 * Generates a map where the key is the vertex and the value is a list of loops that use that vertex as a corner.
 * The lists are allocated from one memory pool.
 */
void BKE_mesh_vert_loop_map_create(MeshElemMap **r_map, int **r_mem,
                                   const MPoly *mpoly, const MLoop *mloop,
                                   int totvert, int totpoly, int totloop)
{
	bke_mesh_vert_poly_loop_map_create(r_map, r_mem, mpoly, mloop, totvert, totpoly, totloop, true);
}

/* Generates a map where the key is the vertex and the value is a list
 * of edges that use that vertex as an endpoint. The lists are allocated
 * from one memory pool. */
void BKE_mesh_vert_edge_map_create(MeshElemMap **r_map, int **r_mem,
                                   const MEdge *medge, int totvert, int totedge)
{
	MeshElemMap *map = MEM_callocN(sizeof(MeshElemMap) * (size_t)totvert, "vert-edge map");
	int *indices = MEM_mallocN(sizeof(int[2]) * (size_t)totedge, "vert-edge map mem");
	int *i_pt = indices;

	int i;

	/* Count number of edges for each vertex */
	for (i = 0; i < totedge; i++) {
		map[medge[i].v1].count++;
		map[medge[i].v2].count++;
	}

	/* Assign indices mem */
	for (i = 0; i < totvert; i++) {
		map[i].indices = i_pt;
		i_pt += map[i].count;

		/* Reset 'count' for use as index in last loop */
		map[i].count = 0;
	}

	/* Find the users */
	for (i = 0; i < totedge; i++) {
		const unsigned int v[2] = {medge[i].v1, medge[i].v2};

		map[v[0]].indices[map[v[0]].count] = i;
		map[v[1]].indices[map[v[1]].count] = i;

		map[v[0]].count++;
		map[v[1]].count++;
	}

	*r_map = map;
	*r_mem = indices;
}

void BKE_mesh_edge_poly_map_create(MeshElemMap **r_map, int **r_mem,
                                   const MEdge *UNUSED(medge), const int totedge,
                                   const MPoly *mpoly, const int totpoly,
                                   const MLoop *mloop, const int totloop)
{
	MeshElemMap *map = MEM_callocN(sizeof(MeshElemMap) * (size_t)totedge, "edge-poly map");
	int *indices = MEM_mallocN(sizeof(int) * (size_t)totloop, "edge-poly map mem");
	int *index_step;
	const MPoly *mp;
	int i;

	/* count face users */
	for (i = 0, mp = mpoly; i < totpoly; mp++, i++) {
		const MLoop *ml;
		int j = mp->totloop;
		for (ml = &mloop[mp->loopstart]; j--; ml++) {
			map[ml->e].count++;
		}
	}

	/* create offsets */
	index_step = indices;
	for (i = 0; i < totedge; i++) {
		map[i].indices = index_step;
		index_step += map[i].count;

		/* re-count, using this as an index below */
		map[i].count = 0;

	}

	/* assign poly-edge users */
	for (i = 0, mp = mpoly; i < totpoly; mp++, i++) {
		const MLoop *ml;
		int j = mp->totloop;
		for (ml = &mloop[mp->loopstart]; j--; ml++) {
			MeshElemMap *map_ele = &map[ml->e];
			map_ele->indices[map_ele->count++] = i;
		}
	}

	*r_map = map;
	*r_mem = indices;
}

/**
 * This function creates a map so the source-data (vert/edge/loop/poly)
 * can loop over the destination data (using the destination arrays origindex).
 *
 * This has the advantage that it can operate on any data-types.
 *
 * \param totsource  The total number of elements the that \a final_origindex points to.
 * \param totfinal  The size of \a final_origindex
 * \param final_origindex  The size of the final array.
 *
 * \note ``totsource`` could be ``totpoly``,
 *       ``totfinal`` could be ``tottessface`` and ``final_origindex`` its ORIGINDEX customdata.
 *       This would allow an MPoly to loop over its tessfaces.
 */
void BKE_mesh_origindex_map_create(MeshElemMap **r_map, int **r_mem,
                                   const int totsource,
                                   const int *final_origindex, const int totfinal)
{
	MeshElemMap *map = MEM_callocN(sizeof(MeshElemMap) * (size_t)totsource, "poly-tessface map");
	int *indices = MEM_mallocN(sizeof(int) * (size_t)totfinal, "poly-tessface map mem");
	int *index_step;
	int i;

	/* count face users */
	for (i = 0; i < totfinal; i++) {
		if (final_origindex[i] != ORIGINDEX_NONE) {
			BLI_assert(final_origindex[i] < totsource);
			map[final_origindex[i]].count++;
		}
	}

	/* create offsets */
	index_step = indices;
	for (i = 0; i < totsource; i++) {
		map[i].indices = index_step;
		index_step += map[i].count;

		/* re-count, using this as an index below */
		map[i].count = 0;
	}

	/* assign poly-tessface users */
	for (i = 0; i < totfinal; i++) {
		if (final_origindex[i] != ORIGINDEX_NONE) {
			MeshElemMap *map_ele = &map[final_origindex[i]];
			map_ele->indices[map_ele->count++] = i;
		}
	}

	*r_map = map;
	*r_mem = indices;
}

/** \} */



/* -------------------------------------------------------------------- */

/** \name Mesh Smooth Groups
 * \{ */

/**
 * Calculate smooth groups from sharp edges.
 *
 * \param r_totgroup The total number of groups, 1 or more.
 * \return Polygon aligned array of group index values (bitflags if use_bitflags is true), starting at 1.
 */
int *BKE_mesh_calc_smoothgroups(const MEdge *medge, const int totedge,
                                const MPoly *mpoly, const int totpoly,
                                const MLoop *mloop, const int totloop,
                                int *r_totgroup, const bool use_bitflags)
{
	int *poly_groups;
	int *poly_stack;

	int poly_prev = 0;
	const int temp_poly_group_id = 3;  /* Placeholder value. */
	const int poly_group_id_overflowed = 5;  /* Group we could not find any available bit, will be reset to 0 at end */
	int tot_group = 0;
	bool group_id_overflow = false;

	/* map vars */
	MeshElemMap *edge_poly_map;
	int *edge_poly_mem;

	if (totpoly == 0) {
		*r_totgroup = 0;
		return NULL;
	}

	BKE_mesh_edge_poly_map_create(&edge_poly_map, &edge_poly_mem,
	                              medge, totedge,
	                              mpoly, totpoly,
	                              mloop, totloop);

	poly_groups = MEM_callocN(sizeof(int) * (size_t)totpoly, __func__);
	poly_stack  = MEM_mallocN(sizeof(int) * (size_t)totpoly, __func__);

	while (true) {
		int poly;
		int bit_poly_group_mask = 0;
		int poly_group_id;
		int ps_curr_idx = 0, ps_end_idx = 0;  /* stack indices */

		for (poly = poly_prev; poly < totpoly; poly++) {
			if (poly_groups[poly] == 0) {
				break;
			}
		}

		if (poly == totpoly) {
			/* all done */
			break;
		}

		poly_group_id = use_bitflags ? temp_poly_group_id : ++tot_group;

		/* start searching from here next time */
		poly_prev = poly + 1;

		poly_groups[poly] = poly_group_id;
		poly_stack[ps_end_idx++] = poly;

		while (ps_curr_idx != ps_end_idx) {
			const MPoly *mp;
			const MLoop *ml;
			int j;

			poly = poly_stack[ps_curr_idx++];
			BLI_assert(poly_groups[poly] == poly_group_id);

			mp = &mpoly[poly];
			for (ml = &mloop[mp->loopstart], j = mp->totloop; j--; ml++) {
				/* loop over poly users */
				const MeshElemMap *map_ele = &edge_poly_map[ml->e];
				const int *p = map_ele->indices;
				int i = map_ele->count;
				if (!(medge[ml->e].flag & ME_SHARP)) {
					for (; i--; p++) {
						/* if we meet other non initialized its a bug */
						BLI_assert(ELEM(poly_groups[*p], 0, poly_group_id));

						if (poly_groups[*p] == 0) {
							poly_groups[*p] = poly_group_id;
							poly_stack[ps_end_idx++] = *p;
						}
					}
				}
				else if (use_bitflags) {
					/* Find contiguous smooth groups already assigned, these are the values we can't reuse! */
					for (; i--; p++) {
						int bit = poly_groups[*p];
						if (!ELEM(bit, 0, poly_group_id, poly_group_id_overflowed) &&
						    !(bit_poly_group_mask & bit))
						{
							bit_poly_group_mask |= bit;
						}
					}
				}
			}
		}
		/* And now, we have all our poly from current group in poly_stack (from 0 to (ps_end_idx - 1)), as well as
		 * all smoothgroups bits we can't use in bit_poly_group_mask.
		 */
		if (use_bitflags) {
			int i, *p, gid_bit = 0;
			poly_group_id = 1;

			/* Find first bit available! */
			for (; (poly_group_id & bit_poly_group_mask) && (gid_bit < 32); gid_bit++) {
				poly_group_id <<= 1;  /* will 'overflow' on last possible iteration. */
			}
			if (UNLIKELY(gid_bit > 31)) {
				/* All bits used in contiguous smooth groups, we can't do much!
				 * Note: this is *very* unlikely - theoretically, four groups are enough, I don't think we can reach
				 *       this goal with such a simple algo, but I don't think either we'll never need all 32 groups!
				 */
				printf("Warning, could not find an available id for current smooth group, faces will me marked "
				       "as out of any smooth group...\n");
				poly_group_id = poly_group_id_overflowed; /* Can't use 0, will have to set them to this value later. */
				group_id_overflow = true;
			}
			if (gid_bit > tot_group) {
				tot_group = gid_bit;
			}
			/* And assign the final smooth group id to that poly group! */
			for (i = ps_end_idx, p = poly_stack; i--; p++) {
				poly_groups[*p] = poly_group_id;
			}
		}
	}

	if (UNLIKELY(group_id_overflow)) {
		int i = totpoly, *gid = poly_groups;
		for (; i--; gid++) {
			if (*gid == poly_group_id_overflowed) {
				*gid = 0;
			}
		}
	}

	MEM_freeN(edge_poly_map);
	MEM_freeN(edge_poly_mem);
	MEM_freeN(poly_stack);

	*r_totgroup = tot_group + 1;

	return poly_groups;
}
/** \} */

/* -------------------------------------------------------------------- */

/** \name Mesh to mesh mapping
 * \{ */

/**
 * Calculate smooth groups from sharp edges.
 *
 * \param r_totgroup The total number of groups, 1 or more.
 * \return Polygon aligned array of group index values (bitflags if use_bitflags is true), starting at 1.
 */

void BKE_mesh2mesh_mapping_free(Mesh2MeshMapping *map)
{
	/* For now, we use mere MEM_mallocN, later we'll probably switch to memarena! */
	int i = map->nbr_items;

	if (!i) {
		return;
	}

	while (i--) {
		Mesh2MeshMappingItem *it = &map->items[i];
		if (it->nbr_sources) {
			MEM_freeN(it->indices_src);
			MEM_freeN(it->weights_src);
		}
	}

	MEM_freeN(map->items);

	map->nbr_items = 0;
	map->items = NULL;
	map->mem = NULL;
}

static void bke_mesh2mesh_mapping_item_define(
        Mesh2MeshMappingItem *mapit, const float hit_distance, const int island,
        const int nbr_sources, const int *indices_src, const float *weights_src)
{
	if (nbr_sources) {
		mapit->nbr_sources = nbr_sources;
		mapit->indices_src = MEM_mallocN(sizeof(*mapit->indices_src) * (size_t)nbr_sources, __func__);
		memcpy(mapit->indices_src, indices_src, sizeof(*mapit->indices_src) * (size_t)nbr_sources);
		mapit->weights_src = MEM_mallocN(sizeof(*mapit->weights_src) * (size_t)nbr_sources, __func__);
		memcpy(mapit->weights_src, weights_src, sizeof(*mapit->weights_src) * (size_t)nbr_sources);
	}
	else {
		mapit->nbr_sources = 0;
		mapit->indices_src = NULL;
		mapit->weights_src = NULL;
	}
	mapit->hit_distance = hit_distance;
	mapit->island = island;
}

void BKE_mesh2mesh_mapping_islands_create(Mesh2MeshMappingIslands *r_islands, const int num_loops)
{
	r_islands->loops_to_islands_idx = MEM_mallocN(sizeof(*r_islands->loops_to_islands_idx) * (size_t)num_loops, __func__);
	r_islands->islands = NULL;
	r_islands->nbr_islands = 0;
	r_islands->mem = NULL;
}

void BKE_mesh2mesh_mapping_islands_add_island(Mesh2MeshMappingIslands *r_islands,
                                              const int num_loops, int *loop_indices,
                                              const int num_polys, int *poly_indices)
{
	Mesh2MeshMappingIslandItem *islands;
	const int curr_island_idx = r_islands->nbr_islands++;
	const size_t curr_num_islands = (size_t)r_islands->nbr_islands;
	int i = num_loops;

	r_islands->nbr_loops = num_loops;
	while (i--) {
		r_islands->loops_to_islands_idx[loop_indices[i]] = curr_island_idx;
	}

	/* XXX TODO UGLY!!! Quick code, to be done better. */
	islands = MEM_mallocN(sizeof(*islands) * curr_num_islands, __func__);
	if (curr_island_idx) {
		memcpy(islands, r_islands->islands, curr_num_islands - 1);
		MEM_freeN(r_islands->islands);
	}
	r_islands->islands = islands;

	islands = &islands[curr_island_idx];
	islands->nbr_polys = num_polys;
	islands->polys_idx = MEM_mallocN(sizeof(*islands->polys_idx) * (size_t)num_polys, __func__);
	memcpy(islands->polys_idx, poly_indices, sizeof(*islands->polys_idx) * (size_t)num_polys);
}

static void bke_mesh2mesh_mapping_islands_free(Mesh2MeshMappingIslands *islands)
{
	/* For now, we use mere MEM_mallocN, later we'll probably switch to memarena! */
	int i = islands->nbr_islands;

	if (!i) {
		return;
	}

	while (i--) {
		Mesh2MeshMappingIslandItem *it = &islands->islands[i];
		if (it->nbr_polys) {
			MEM_freeN(it->polys_idx);
		}
	}

	MEM_freeN(islands->islands);
	MEM_freeN(islands->loops_to_islands_idx);

	islands->nbr_loops = 0;
	islands->loops_to_islands_idx = NULL;
	islands->nbr_islands = 0;
	islands->islands = NULL;
	islands->mem = NULL;
}

static int bke_mesh2mesh_mapping_get_interp_poly_data(
        const MPoly *mp, MLoop *mloops, const float (*vcos_src)[3], const float point[3],
        size_t *buff_size, float (**vcos)[3], int **indices, float **weights, const bool do_weights,
        int *r_closest_v, int *r_closest_l)
{
	MLoop *ml;
	float (*vco)[3];
	float ref_dist_sq = FLT_MAX;
	int *idx;
	const int nbr_sources = mp->totloop;
	int i;

	if ((size_t)nbr_sources > *buff_size) {
		*buff_size = (size_t)nbr_sources;
		*vcos = MEM_reallocN(*vcos, sizeof(**vcos) * *buff_size);
		*indices = MEM_reallocN(*indices, sizeof(**indices) * *buff_size);
		if (do_weights) {
			*weights = MEM_reallocN(*weights, sizeof(**weights) * *buff_size);
		}
	}

	for (i = 0, ml = &mloops[mp->loopstart], vco = *vcos, idx = *indices; i < nbr_sources; i++, ml++, vco++, idx++) {
		*idx = (int)ml->v;
		copy_v3_v3(*vco, vcos_src[ml->v]);
		if (r_closest_v || r_closest_l) {
			/* Find closest vert/loop in this case. */
			const float dist_sq = len_squared_v3v3(point, *vco);
			if (dist_sq < ref_dist_sq) {
				ref_dist_sq = dist_sq;
				if (r_closest_v) {
					*r_closest_v = (int)ml->v;
				}
				if (r_closest_l) {
					*r_closest_l = (int)mp->loopstart + i;
				}
			}
		}
	}

	if (do_weights) {
		interp_weights_poly_v3(*weights, *vcos, nbr_sources, point);
	}

	return nbr_sources;
}

static float bke_mesh2mesh_bvhtree_query_nearest(
        BVHTreeFromMesh *treedata, BVHTreeNearest *nearest, const SpaceTransform *space_transform,
        float co[3], const float max_dist_sq)
{
	/* Convert the vertex to tree coordinates, if needed. */
	if (space_transform) {
		BLI_space_transform_apply(space_transform, co);
	}

	/* Use local proximity heuristics (to reduce the nearest search). */
	if (nearest->index != -1) {
		nearest->dist_sq = min_ff(len_squared_v3v3(co, nearest->co), max_dist_sq);
	}
	else {
		nearest->dist_sq = max_dist_sq;
	}
	/* Compute and store result. If invalid (-1 idx), keep FLT_MAX dist. */
	BLI_bvhtree_find_nearest(treedata->tree, co, nearest, treedata->nearest_callback, treedata);
	return sqrtf(nearest->dist_sq);
}

static float bke_mesh2mesh_bvhtree_query_raycast(
        BVHTreeFromMesh *treedata, BVHTreeRayHit *rayhit, const SpaceTransform *space_transform,
        float co[3], float no[3], const float radius, const float max_dist_sq)
{
	/* Convert the vertex to tree coordinates, if needed. */
	if (space_transform) {
		BLI_space_transform_apply(space_transform, co);
		BLI_space_transform_apply_normal(space_transform, no);
	}

	rayhit->index = -1;
	BLI_bvhtree_ray_cast(treedata->tree, co, no, radius, rayhit, treedata->raycast_callback, treedata);

#if 0  /* Stupid in fact!? */
	/* If no ray hit along vertex normal, try in the other direction! */
	if (rayhit->index < 0 || (rayhit->dist * rayhit->dist) > max_dist_sq) {
		negate_v3(no);
		BLI_bvhtree_ray_cast(treedata->tree, co, no, radius, rayhit, treedata->raycast_callback, treedata);
	}
#endif
	if (rayhit->index >= 0 && (rayhit->dist * rayhit->dist) > max_dist_sq) {
		rayhit->index = -1;
	}

	return rayhit->dist;
}

void BKE_dm2mesh_mapping_verts_compute(
        const int mode, const SpaceTransform *space_transform, const float max_dist,
        const MVert *verts_dst, const int numverts_dst, DerivedMesh *dm_src,
        Mesh2MeshMapping *r_map)
{
	const float full_weight = 1.0f;
	const float max_dist_sq = max_dist * max_dist;
	int i;

	BLI_assert(mode & M2MMAP_MODE_VERT);

	r_map->items = MEM_mallocN(sizeof(*r_map->items) * (size_t)numverts_dst, __func__);
	r_map->nbr_items = numverts_dst;

	if (mode == M2MMAP_MODE_TOPOLOGY) {
		BLI_assert(numverts_dst == dm_src->getNumVerts(dm_src));
		for (i = 0; i < numverts_dst; i++) {
			bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 1, &i, &full_weight);
		}
	}
	else {
		BVHTreeFromMesh treedata = {NULL};
		BVHTreeNearest nearest = {0};
		BVHTreeRayHit rayhit = {0};
		float hitdist;

		if (mode == M2MMAP_MODE_VERT_NEAREST) {
			bvhtree_from_mesh_verts(&treedata, dm_src, 0.0, 2, 6);
			nearest.index = -1;

			for (i = 0; i < numverts_dst; i++) {
				float tmp_co[3];

				copy_v3_v3(tmp_co, verts_dst[i].co);

				hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
				                                              tmp_co, max_dist_sq);

				if (nearest.index >= 0) {
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &nearest.index, &full_weight);
				}
				else {
					/* No source for this dest vertex! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}
		}
		else if (ELEM(mode, M2MMAP_MODE_VERT_EDGE_NEAREST, M2MMAP_MODE_VERT_EDGEINTERP_NEAREST)) {
			MEdge *edges_src = dm_src->getEdgeArray(dm_src);
			float (*vcos_src)[3] = MEM_mallocN(sizeof(*vcos_src) * (size_t)dm_src->getNumVerts(dm_src), __func__);
			dm_src->getVertCos(dm_src, vcos_src);

			bvhtree_from_mesh_edges(&treedata, dm_src, 0.0, 2, 6);
			nearest.index = -1;

			for (i = 0; i < numverts_dst; i++) {
				float tmp_co[3];

				copy_v3_v3(tmp_co, verts_dst[i].co);

				hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
				                                              tmp_co, max_dist_sq);

				if (nearest.index >= 0) {
					MEdge *me = &edges_src[nearest.index];
					float (*v1cos)[3] = &vcos_src[me->v1];
					float (*v2cos)[3] = &vcos_src[me->v2];

					if (mode == M2MMAP_MODE_VERT_EDGE_NEAREST) {
						const float dist_v1 = len_squared_v3v3(tmp_co, *v1cos);
						const float dist_v2 = len_squared_v3v3(tmp_co, *v2cos);
						const int index = (int)((dist_v1 > dist_v2) ? me->v2 : me->v1);
						bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &index, &full_weight);
					}
					else if (mode == M2MMAP_MODE_VERT_EDGEINTERP_NEAREST) {
						int indices[2];
						float weights[2];

						indices[0] = (int)me->v1;
						indices[1] = (int)me->v2;

						/* Weight is inverse of point factor here... */
						weights[0] = line_point_factor_v3(tmp_co, *v2cos, *v1cos);
						CLAMP(weights[0], 0.0f, 1.0f);
						weights[1] = 1.0f - weights[0];

						bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 2, indices, weights);
					}
				}
				else {
					/* No source for this dest vertex! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}

			MEM_freeN(vcos_src);
		}
		else if (ELEM(mode, M2MMAP_MODE_VERT_POLY_NEAREST, M2MMAP_MODE_VERT_POLYINTERP_NEAREST,
		                    M2MMAP_MODE_VERT_POLYINTERP_VNORPROJ))
		{
			MPoly *polys_src = dm_src->getPolyArray(dm_src);
			MLoop *loops_src = dm_src->getLoopArray(dm_src);
			float (*vcos_src)[3] = MEM_mallocN(sizeof(*vcos_src) * (size_t)dm_src->getNumVerts(dm_src), __func__);
			int *orig_poly_idx_src;

			size_t tmp_buff_size = 32;  /* Will be enough in 99% of cases. */
			float (*vcos)[3] = MEM_mallocN(sizeof(*vcos) * tmp_buff_size, __func__);
			int *indices = MEM_mallocN(sizeof(*indices) * tmp_buff_size, __func__);
			float *weights = MEM_mallocN(sizeof(*weights) * tmp_buff_size, __func__);

			dm_src->getVertCos(dm_src, vcos_src);
			bvhtree_from_mesh_faces(&treedata, dm_src, 0.0, 2, 6);
			/* bvhtree here uses tesselated faces... */
			orig_poly_idx_src = dm_src->getTessFaceDataArray(dm_src, CD_ORIGINDEX);

			if (mode == M2MMAP_MODE_VERT_POLYINTERP_VNORPROJ) {
				for (i = 0; i < numverts_dst; i++) {
					float tmp_co[3], tmp_no[3];
					const float radius = 1.0e-6f;

					copy_v3_v3(tmp_co, verts_dst[i].co);
					normal_short_to_float_v3(tmp_no, verts_dst[i].no);

					hitdist = bke_mesh2mesh_bvhtree_query_raycast(&treedata, &rayhit, space_transform,
					                                              tmp_co, tmp_no, radius, max_dist_sq);

					if (rayhit.index >= 0 && hitdist <= max_dist) {
						MPoly *mp_src = &polys_src[orig_poly_idx_src[rayhit.index]];
						const int nbr_sources = bke_mesh2mesh_mapping_get_interp_poly_data(
						                                mp_src, loops_src, (const float (*)[3])vcos_src, rayhit.co,
						                                &tmp_buff_size, &vcos, &indices, &weights, true, NULL, NULL);

						bke_mesh2mesh_mapping_item_define(&r_map->items[i], rayhit.dist, 0,
						                                  nbr_sources, indices, weights);
					}
					else {
						/* No source for this dest vertex! */
						bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
					}
				}
			}
			else {
				nearest.index = -1;

				for (i = 0; i < numverts_dst; i++) {
					float tmp_co[3];

					/* Convert the vertex to tree coordinates. */
					copy_v3_v3(tmp_co, verts_dst[i].co);

					hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
					                                              tmp_co, max_dist_sq);

					if (nearest.index >= 0) {
						MPoly *mp = &polys_src[orig_poly_idx_src[nearest.index]];

						if (mode == M2MMAP_MODE_VERT_POLY_NEAREST) {
							int index;
							bke_mesh2mesh_mapping_get_interp_poly_data(
							                                mp, loops_src, (const float (*)[3])vcos_src, nearest.co,
							                                &tmp_buff_size, &vcos, &indices, &weights, false,
							                                &index, NULL);

							bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &index, &full_weight);
						}
						else if (mode == M2MMAP_MODE_VERT_POLYINTERP_NEAREST) {
							const int nbr_sources = bke_mesh2mesh_mapping_get_interp_poly_data(
							                                mp, loops_src, (const float (*)[3])vcos_src, nearest.co,
							                                &tmp_buff_size, &vcos, &indices, &weights, true,
							                                NULL, NULL);

							bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0,
							                                  nbr_sources, indices, weights);
						}
					}
					else {
						/* No source for this dest vertex! */
						bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
					}
				}
			}

			MEM_freeN(vcos_src);
			MEM_freeN(vcos);
			MEM_freeN(indices);
			MEM_freeN(weights);
		}
		else {
			printf("WARNING! Unsupported mesh-to-mesh vertex mapping mode (%d)!\n", mode);
			memset(r_map->items, 0, sizeof(*r_map->items) * (size_t)numverts_dst);
		}

		free_bvhtree_from_mesh(&treedata);
	}
}

/* TODO: all those 'nearest' edge computations could be hugely enhanced, not top priority though. */
void BKE_dm2mesh_mapping_edges_compute(
        const int mode, const SpaceTransform *space_transform, const float max_dist,
        const MVert *verts_dst, const int numverts_dst, const MEdge *edges_dst, const int numedges_dst,
        DerivedMesh *dm_src, Mesh2MeshMapping *r_map)
{
	const float full_weight = 1.0f;
	const float max_dist_sq = max_dist * max_dist;
	int i;

	BLI_assert(mode & M2MMAP_MODE_EDGE);

	r_map->items = MEM_mallocN(sizeof(*r_map->items) * (size_t)numedges_dst, __func__);
	r_map->nbr_items = numedges_dst;

	if (mode == M2MMAP_MODE_TOPOLOGY) {
		BLI_assert(numedges_dst == dm_src->getNumEdges(dm_src));
		for (i = 0; i < numedges_dst; i++) {
			bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 1, &i, &full_weight);
		}
	}
	else {
		BVHTreeFromMesh treedata = {NULL};
		BVHTreeNearest nearest = {0};
		float hitdist;

		if (mode == M2MMAP_MODE_EDGE_VERT_NEAREST) {
			const int numverts_src = dm_src->getNumVerts(dm_src);
			const int numedges_src = dm_src->getNumEdges(dm_src);
			MEdge *edges_src = dm_src->getEdgeArray(dm_src);
			float (*vcos_src)[3] = MEM_mallocN(sizeof(*vcos_src) * (size_t)dm_src->getNumVerts(dm_src), __func__);

			MeshElemMap *vert2edge_src_map;
			int *vert2edge_src_map_mem;

			/* Note we store an integer index in second element (first one is hitdist). */
			float (*v_dst2src_map)[2] = MEM_mallocN(sizeof(*v_dst2src_map) * (size_t)numverts_dst, __func__);
			fill_vn_fl((float *)v_dst2src_map, numverts_dst * 2, -1.0f);

			BKE_mesh_vert_edge_map_create(&vert2edge_src_map, &vert2edge_src_map_mem,
			                              edges_src, numverts_src, numedges_src);

			dm_src->getVertCos(dm_src, vcos_src);

			bvhtree_from_mesh_verts(&treedata, dm_src, 0.0, 2, 6);
			nearest.index = -1;

			for (i = 0; i < numedges_dst; i++) {
				const MEdge *e_dst = &edges_dst[i];
				float best_totdist = FLT_MAX;
				int best_eidx_src = -1;
				int j = 2;

				while (j--) {
					const unsigned int vidx_dst = j ? e_dst->v1 : e_dst->v2;

					/* Compute closest verts only once! */
					if (v_dst2src_map[vidx_dst][0] < 0.0f) {
						float tmp_co[3];

						copy_v3_v3(tmp_co, verts_dst[vidx_dst].co);
						hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
						                                              tmp_co, max_dist_sq);

						if (nearest.index >= 0) {
							v_dst2src_map[vidx_dst][0] = hitdist;
							v_dst2src_map[vidx_dst][1] = (float)nearest.index;
						}
						else {
							/* No source for this dest vert! */
							v_dst2src_map[vidx_dst][0] = FLT_MAX;
						}
					}
				}

				/* Now, check all source edges of closest sources vertices, and select the one giving the smallest
				 * total verts-to-verts distance. */
				for (j = 2; j--;) {
					const unsigned int vidx_dst = j ? e_dst->v1 : e_dst->v2;
					const float first_dist = v_dst2src_map[vidx_dst][0];
					const int vidx_src = (int)v_dst2src_map[vidx_dst][1];
					int *eidx_src, k;

					if (vidx_src < 0) {
						continue;
					}

					eidx_src = vert2edge_src_map[vidx_src].indices;
					k = vert2edge_src_map[vidx_src].count;

					for (; k--; eidx_src++) {
						MEdge *e_src = &edges_src[*eidx_src];
						const float *other_co_src = vcos_src[BKE_mesh_edge_other_vert(e_src, vidx_src)];
						const float *other_co_dst = verts_dst[BKE_mesh_edge_other_vert(e_dst, (int)vidx_dst)].co;
						const float totdist = first_dist + len_v3v3(other_co_src, other_co_dst);

						if (totdist < best_totdist) {
							best_totdist = totdist;
							best_eidx_src = *eidx_src;
						}
					}
				}

				if (best_eidx_src >= 0) {
					const float *co1_src = vcos_src[edges_src[best_eidx_src].v1];
					const float *co2_src = vcos_src[edges_src[best_eidx_src].v2];
					const float *co1_dst = verts_dst[e_dst->v1].co;
					const float *co2_dst = verts_dst[e_dst->v2].co;
					float co_src[3], co_dst[3];

					/* TODO: would need an isect_seg_seg_v3(), actually! */
					const int isect_type = isect_line_line_v3(co1_src, co2_src, co1_dst, co2_dst, co_src, co_dst);
					if (isect_type != 0) {
						const float fac_src = line_point_factor_v3(co_src, co1_src, co2_src);
						const float fac_dst = line_point_factor_v3(co_dst, co1_dst, co2_dst);
						if (fac_src < 0.0f) {
							copy_v3_v3(co_src, co1_src);
						}
						else if (fac_src > 1.0f) {
							copy_v3_v3(co_src, co2_src);
						}
						if (fac_dst < 0.0f) {
							copy_v3_v3(co_dst, co1_dst);
						}
						else if (fac_dst > 1.0f) {
							copy_v3_v3(co_dst, co2_dst);
						}
					}
					hitdist = len_v3v3(co_dst, co_src);
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &best_eidx_src, &full_weight);
				}
				else {
					/* No source for this dest edge! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}

			MEM_freeN(vcos_src);
			MEM_freeN(v_dst2src_map);
			MEM_freeN(vert2edge_src_map);
			MEM_freeN(vert2edge_src_map_mem);
		}
		else if (mode == M2MMAP_MODE_EDGE_NEAREST) {
			bvhtree_from_mesh_edges(&treedata, dm_src, 0.0, 2, 6);
			nearest.index = -1;

			for (i = 0; i < numedges_dst; i++) {
				float tmp_co[3];

				interp_v3_v3v3(tmp_co, verts_dst[edges_dst[i].v1].co, verts_dst[edges_dst[i].v2].co, 0.5f);

				hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
				                                              tmp_co, max_dist_sq);

				if (nearest.index >= 0) {
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &nearest.index, &full_weight);
				}
				else {
					/* No source for this dest edge! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}
		}
		else if (mode == M2MMAP_MODE_EDGE_POLY_NEAREST) {
			MEdge *edges_src = dm_src->getEdgeArray(dm_src);
			MPoly *polys_src = dm_src->getPolyArray(dm_src);
			MLoop *loops_src = dm_src->getLoopArray(dm_src);
			float (*vcos_src)[3] = MEM_mallocN(sizeof(*vcos_src) * (size_t)dm_src->getNumVerts(dm_src), __func__);
			int *orig_poly_idx_src;

			dm_src->getVertCos(dm_src, vcos_src);
			bvhtree_from_mesh_faces(&treedata, dm_src, 0.0, 2, 6);
			/* bvhtree here uses tesselated faces... */
			orig_poly_idx_src = dm_src->getTessFaceDataArray(dm_src, CD_ORIGINDEX);

			for (i = 0; i < numedges_dst; i++) {
				float tmp_co[3];

				interp_v3_v3v3(tmp_co, verts_dst[edges_dst[i].v1].co, verts_dst[edges_dst[i].v2].co, 0.5f);

				hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
				                                              tmp_co, max_dist_sq);

				if (nearest.index >= 0) {
					MPoly *mp_src = &polys_src[orig_poly_idx_src[nearest.index]];
					MLoop *ml_src = &loops_src[mp_src->loopstart];
					int nloops = mp_src->totloop;
					float best_dist_sq = FLT_MAX;
					int best_eidx_src = -1;

					for (; nloops--; ml_src++) {
						MEdge *me_src = &edges_src[ml_src->e];
						float *co1_src = vcos_src[me_src->v1];
						float *co2_src = vcos_src[me_src->v2];
						float co_src[3];
						float dist_sq;

						interp_v3_v3v3(co_src, co1_src, co2_src, 0.5f);
						dist_sq = len_squared_v3v3(tmp_co, co_src);
						if (dist_sq < best_dist_sq) {
							best_dist_sq = dist_sq;
							best_eidx_src = (int)ml_src->e;
						}
					}
					if (best_eidx_src >= 0) {
						bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1, &best_eidx_src, &full_weight);
					}
				}
				else {
					/* No source for this dest edge! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}

			MEM_freeN(vcos_src);
		}
		else {
			printf("WARNING! Unsupported mesh-to-mesh edge mapping mode (%d)!\n", mode);
			memset(r_map->items, 0, sizeof(*r_map->items) * (size_t)numedges_dst);
		}

		free_bvhtree_from_mesh(&treedata);
	}
}

void BKE_dm2mesh_mapping_polys_compute(
        const int mode, const SpaceTransform *space_transform, const float max_dist,
        MVert *verts_dst, const int numverts_dst, MPoly *polys_dst, const int numpolys_dst,
        MLoop *loops_dst, const int numloops_dst, CustomData *pdata_dst, DerivedMesh *dm_src,
        Mesh2MeshMapping *r_map)
{
	const float full_weight = 1.0f;
	const float max_dist_sq = max_dist * max_dist;
	float (*poly_nors_dst)[3] = NULL;
	int i;

	BLI_assert(mode & M2MMAP_MODE_POLY);

	if (mode & (M2MMAP_USE_NORMAL | M2MMAP_USE_NORPROJ)) {
		/* Cache poly nors into a temp CDLayer. */
		poly_nors_dst = CustomData_get_layer(pdata_dst, CD_NORMAL);
		if (!poly_nors_dst) {
			poly_nors_dst = CustomData_add_layer(pdata_dst, CD_NORMAL, CD_CALLOC, NULL, numpolys_dst);
			CustomData_set_layer_flag(pdata_dst, CD_NORMAL, CD_FLAG_TEMPORARY);
			BKE_mesh_calc_normals_poly(verts_dst, numverts_dst, loops_dst, polys_dst, numloops_dst, numpolys_dst,
			                           poly_nors_dst, true);
		}
	}

	r_map->items = MEM_mallocN(sizeof(*r_map->items) * (size_t)numpolys_dst, __func__);
	r_map->nbr_items = numpolys_dst;

	if (mode == M2MMAP_MODE_TOPOLOGY) {
		BLI_assert(numpolys_dst == dm_src->getNumPolys(dm_src));
		for (i = 0; i < numpolys_dst; i++) {
			bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 1, &i, &full_weight);
		}
	}
	else {
		BVHTreeFromMesh treedata = {NULL};
		BVHTreeNearest nearest = {0};
		BVHTreeRayHit rayhit = {0};
		float hitdist;

		int *orig_poly_idx_src;

		bvhtree_from_mesh_faces(&treedata, dm_src, 0.0, 2, 6);
		/* bvhtree here uses tesselated faces... */
		orig_poly_idx_src = dm_src->getTessFaceDataArray(dm_src, CD_ORIGINDEX);

		if (mode == M2MMAP_MODE_POLY_NEAREST) {
			nearest.index = -1;

			for (i = 0; i < numpolys_dst; i++) {
				MPoly *mp = &polys_dst[i];
				float tmp_co[3];

				BKE_mesh_calc_poly_center(mp, &loops_dst[mp->loopstart], verts_dst, tmp_co);

				hitdist = bke_mesh2mesh_bvhtree_query_nearest(&treedata, &nearest, space_transform,
				                                              tmp_co, max_dist_sq);

				if (nearest.index >= 0) {
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist, 0, 1,
					                                  &orig_poly_idx_src[nearest.index], &full_weight);
				}
				else {
					/* No source for this dest poly! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}
		}
		else if (mode == M2MMAP_MODE_POLY_NOR) {
			BLI_assert(poly_nors_dst);

			for (i = 0; i < numpolys_dst; i++) {
				MPoly *mp = &polys_dst[i];
				float tmp_co[3], tmp_no[3];
				const float radius = 1.0e-6f;

				BKE_mesh_calc_poly_center(mp, &loops_dst[mp->loopstart], verts_dst, tmp_co);
				copy_v3_v3(tmp_no, poly_nors_dst[i]);

				hitdist = bke_mesh2mesh_bvhtree_query_raycast(&treedata, &rayhit, space_transform,
				                                              tmp_co, tmp_no, radius, max_dist_sq);

				if (rayhit.index >= 0 && hitdist <= max_dist) {
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], rayhit.dist, 0, 1,
					                                  &orig_poly_idx_src[rayhit.index], &full_weight);
				}
				else {
					/* No source for this dest vertex! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}
		}
		else if (mode == M2MMAP_MODE_POLY_POLYINTERP_PNORPROJ) {
			const size_t numpolys_src = (size_t)dm_src->getNumPolys(dm_src);

			/* Here it's simpler to just allocate for all polys :/ */
			int *indices = MEM_mallocN(sizeof(*indices) * numpolys_src, __func__);
			float *weights = MEM_mallocN(sizeof(*weights) * numpolys_src, __func__);

			size_t tmp_poly_size = 32;  /* Will be enough in 99% of cases. */
			float (*poly_vcos_2d)[2] = MEM_mallocN(sizeof(*poly_vcos_2d) * tmp_poly_size, __func__);

			for (i = 0; i < numpolys_dst; i++) {
				/* For each dst poly, we sample some rays from it (2D grid in pnor space)
				 * and use their hits to interpolate from source polys. */
				MPoly *mp = &polys_dst[i];
				float tmp_co[3], tmp_no[3];
				const float radius = 1.0e-6f;

				const int grid_size = 10;
				const float zvec[3] = {0.0f, 0.0f, 1.0f};
				float pcent_dst[3];
				float to_pnor_2d_mat[3][3], from_pnor_2d_mat[3][3];
				float poly_dst_2d_min_x, poly_dst_2d_min_y, poly_dst_2d_max_x, poly_dst_2d_max_y, poly_dst_2d_z;
				float grid_step_x, grid_step_y;

				float totweights = 0.0f;
				float hitdist_accum = 0.0f;
				int nbr_sources = 0;
				int j, k;

				copy_v3_v3(tmp_no, poly_nors_dst[i]);
				fill_vn_fl(weights, (int)numpolys_src, 0.0f);

				if ((size_t)mp->totloop > tmp_poly_size) {
					tmp_poly_size = (size_t)mp->totloop;
					poly_vcos_2d = MEM_reallocN(poly_vcos_2d, sizeof(*poly_vcos_2d) * tmp_poly_size);
				}

				rotation_between_vecs_to_mat3(to_pnor_2d_mat, poly_nors_dst[i], zvec);
				invert_m3_m3(from_pnor_2d_mat, to_pnor_2d_mat);

				BKE_mesh_calc_poly_center(mp, &loops_dst[mp->loopstart], verts_dst, pcent_dst);

				mul_m3_v3(to_pnor_2d_mat, tmp_co);
				poly_dst_2d_z = tmp_co[2];
				printf("%f\n", tmp_co[2]);

				poly_dst_2d_min_x = poly_dst_2d_min_y = FLT_MAX;
				poly_dst_2d_max_x = poly_dst_2d_max_y = -FLT_MAX;

				for (j = 0; j < mp->totloop; j++) {
					MLoop *ml = &loops_dst[j + mp->loopstart];
					copy_v3_v3(tmp_co, verts_dst[ml->v].co);
					mul_m3_v3(to_pnor_2d_mat, tmp_co);
					copy_v2_v2(poly_vcos_2d[j], tmp_co);
					printf("%f\n", tmp_co[2]);
					if (tmp_co[0] > poly_dst_2d_max_x) poly_dst_2d_max_x = tmp_co[0];
					if (tmp_co[0] < poly_dst_2d_min_x) poly_dst_2d_min_x = tmp_co[0];
					if (tmp_co[1] > poly_dst_2d_max_y) poly_dst_2d_max_y = tmp_co[1];
					if (tmp_co[1] < poly_dst_2d_min_y) poly_dst_2d_min_y = tmp_co[1];
				}

				grid_step_x = (poly_dst_2d_max_x - poly_dst_2d_min_x) / (float)grid_size;
				grid_step_y = (poly_dst_2d_max_y - poly_dst_2d_min_y) / (float)grid_size;

				for (j = 0; j < grid_size; j++) {
					for (k = 0; k < grid_size; k++) {
						tmp_co[0] = poly_dst_2d_min_x + grid_step_x * (float)j;
						tmp_co[1] = poly_dst_2d_min_y + grid_step_y * (float)k;

						if (!isect_point_poly_v2(tmp_co, (const float (*)[2])poly_vcos_2d,
						                         (unsigned int)mp->totloop, false))
						{
							continue;
						}

						tmp_co[2] = poly_dst_2d_z;
						mul_m3_v3(from_pnor_2d_mat, tmp_co);

						/* At this point, tmp_co is a point on our poly surface, in mesh_dst space! */

						/* XXX This transform same tmp_no several times into src space, check whether optimizing this
						 *     is worth it! */
						hitdist = bke_mesh2mesh_bvhtree_query_raycast(&treedata, &rayhit, space_transform,
						                                              tmp_co, tmp_no, radius, max_dist_sq);

						if (rayhit.index >= 0 && hitdist <= max_dist) {
							weights[orig_poly_idx_src[rayhit.index]] += 1.0f;
							totweights += 1.0f;
							hitdist_accum += hitdist;
						}
					}
				}
				if (nbr_sources > 0) {
					for (j = 0; j < (int)numpolys_src; j++) {
						if (!weights[j]) {
							continue;
						}
						/* Note: nbr_sources is always <= j! */
						weights[nbr_sources] = weights[j] / totweights;
						indices[nbr_sources] = j;
						nbr_sources++;
					}
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], hitdist_accum / totweights, 0,
					                                  nbr_sources, indices, weights);
				}
				else {
					/* No source for this dest poly! */
					bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 0, NULL, NULL);
				}
			}

			MEM_freeN(poly_vcos_2d);
			MEM_freeN(indices);
			MEM_freeN(weights);
		}
		else {
			printf("WARNING! Unsupported mesh-to-mesh poly mapping mode (%d)!\n", mode);
			memset(r_map->items, 0, sizeof(*r_map->items) * (size_t)numpolys_dst);
		}

		free_bvhtree_from_mesh(&treedata);
	}
}


void BKE_dm2mesh_mapping_loops_compute(
        const int mode, const SpaceTransform *space_transform, const float max_dist,
        MVert *verts_dst, const int numverts_dst, MEdge *edges_dst, const int numedges_dst,
        MPoly *polys_dst, const int numpolys_dst, MLoop *loops_dst, const int numloops_dst,
        CustomData *pdata_dst, CustomData *ldata_dst, const float split_angle_dst,
        DerivedMesh *dm_src, loop_island_compute gen_islands_src, Mesh2MeshMapping *r_map)
{
	const float full_weight = 1.0f;
	const float max_dist_sq = max_dist * max_dist;

	int i;

	BLI_assert(mode & M2MMAP_MODE_LOOP);

	r_map->items = MEM_mallocN(sizeof(*r_map->items) * (size_t)numloops_dst, __func__);
	r_map->nbr_items = numloops_dst;

	if (mode == M2MMAP_MODE_TOPOLOGY) {
		/* In topology mapping, we assume meshes are identical, islands included! */
		BLI_assert(numloops_dst == dm_src->getNumLoops(dm_src));
		for (i = 0; i < numloops_dst; i++) {
			bke_mesh2mesh_mapping_item_define(&r_map->items[i], FLT_MAX, 0, 1, &i, &full_weight);
		}
	}
	else {
		BVHTreeFromMesh *treedata = NULL;
		BVHTreeNearest nearest = {0};
		BVHTreeRayHit rayhit = {0};
		int num_trees = 0;
		float hitdist;

		const bool use_from_vert = (mode & M2MMAP_USE_VERT);

		Mesh2MeshMappingIslands islands;
		bool use_islands = false;

		float (*poly_nors_src)[3] = NULL;
		float (*loop_nors_src)[3] = NULL;
		float (*poly_nors_dst)[3] = NULL;
		float (*loop_nors_dst)[3] = NULL;

		MeshElemMap *vert_to_polyloop_map_src = NULL;
		int *vert_to_polyloop_map_src_buff = NULL;

		bool verts_allocated_src;
		MVert *verts_src = DM_get_vert_array(dm_src, &verts_allocated_src);
		const int num_verts_src = dm_src->getNumVerts(dm_src);
		float (*vcos_src)[3] = NULL;
		bool loops_allocated_src;
		MLoop *loops_src = DM_get_loop_array(dm_src, &loops_allocated_src);
		const int num_loops_src = dm_src->getNumLoops(dm_src);
		bool polys_allocated_src;
		MPoly *polys_src = DM_get_poly_array(dm_src, &polys_allocated_src);
		const int num_polys_src = dm_src->getNumPolys(dm_src);
		bool faces_allocated_src;
		MFace *faces_src = NULL;
		int num_faces_src;

		int *orig_poly_idx_src = NULL;

		size_t buff_size_interp = 32;  /* Will be enough in 99% of cases. */
		float (*vcos_interp)[3] = NULL;
		int *indices_interp = NULL;
		float *weights_interp = NULL;

		int tidx, pidx_dst, lidx_dst, plidx_dst, pidx_src, lidx_src;

		if (!use_from_vert) {
			vcos_src = MEM_mallocN(sizeof(*vcos_src) * (size_t)num_verts_src, __func__);
			dm_src->getVertCos(dm_src, vcos_src);

			vcos_interp = MEM_mallocN(sizeof(*vcos_interp) * buff_size_interp, __func__);
			indices_interp = MEM_mallocN(sizeof(*indices_interp) * buff_size_interp, __func__);
			weights_interp = MEM_mallocN(sizeof(*weights_interp) * buff_size_interp, __func__);
		}

		{
			const bool need_lnors_src = (mode & M2MMAP_USE_LOOP) && (mode & M2MMAP_USE_NORMAL);
			const bool need_lnors_dst = need_lnors_src || (mode & M2MMAP_USE_NORPROJ);
			const bool need_pnors_src = need_lnors_src || ((mode & M2MMAP_USE_POLY) && (mode & M2MMAP_USE_NORMAL));
			const bool need_pnors_dst = need_lnors_dst || need_pnors_src;

			if (need_pnors_dst) {
				/* Cache poly nors into a temp CDLayer. */
				poly_nors_dst = CustomData_get_layer(pdata_dst, CD_NORMAL);
				if (!poly_nors_dst) {
					poly_nors_dst = CustomData_add_layer(pdata_dst, CD_NORMAL, CD_CALLOC, NULL, numpolys_dst);
					CustomData_set_layer_flag(pdata_dst, CD_NORMAL, CD_FLAG_TEMPORARY);
					BKE_mesh_calc_normals_poly(verts_dst, numverts_dst, loops_dst, polys_dst,
					                           numloops_dst, numpolys_dst, poly_nors_dst, true);
				}
			}
			if (need_lnors_dst) {
				/* Cache poly nors into a temp CDLayer. */
				loop_nors_dst = CustomData_get_layer(ldata_dst, CD_NORMAL);
				if (!loop_nors_dst) {
					loop_nors_dst = CustomData_add_layer(ldata_dst, CD_NORMAL, CD_CALLOC, NULL, numloops_dst);
					CustomData_set_layer_flag(ldata_dst, CD_NORMAL, CD_FLAG_TEMPORARY);
					BKE_mesh_normals_loop_split(verts_dst, numverts_dst, edges_dst, numedges_dst,
					                            loops_dst, loop_nors_dst, numloops_dst,
					                            polys_dst, poly_nors_dst, numpolys_dst, split_angle_dst);
				}
			}
			if (need_pnors_src || need_lnors_src) {
				/* Simpler for now, calcNormals never stores pnors :( */
				dm_src->calcLoopNormals(dm_src, /* TODO */ (float)M_PI);

				if (need_pnors_src) {
					poly_nors_src = dm_src->getPolyDataArray(dm_src, CD_NORMAL);
				}
				if (need_lnors_src) {
					loop_nors_src = dm_src->getLoopDataArray(dm_src, CD_NORMAL);
				}
			}
		}

		if (use_from_vert) {
			if (mode & M2MMAP_USE_LOOP) {
				BKE_mesh_vert_loop_map_create(&vert_to_polyloop_map_src, &vert_to_polyloop_map_src_buff,
				                              polys_src, loops_src, num_verts_src, num_polys_src, num_loops_src);
			}
			else if (mode & M2MMAP_USE_POLY) {
				BKE_mesh_vert_poly_map_create(&vert_to_polyloop_map_src, &vert_to_polyloop_map_src_buff,
				                              polys_src, loops_src, num_verts_src, num_polys_src, num_loops_src);
			}
		}

		/* Island makes things slightly more complex here.
		 * Basically, we:
		 *     * Make one treedata for each island's elements.
		 *     * Check all loops of a same dest poly against all treedata.
		 *     * Choose the island's elements giving the best results.
		 */

		/* First, generate the islands, if possible. */
		if (gen_islands_src) {
			use_islands = gen_islands_src(dm_src, &islands);
			num_trees = use_islands ? islands.nbr_islands : 1;
			treedata = MEM_mallocN(sizeof(*treedata) * (size_t)num_trees, __func__);
		}
		else {
			num_trees = 1;
			treedata = MEM_mallocN(sizeof(*treedata), __func__);
		}

		/* Build our BVHtrees, either from verts or tessfaces. */
		if (use_from_vert) {
			if (use_islands) {
				bool *verts_active = MEM_mallocN(sizeof(*verts_active) * (size_t)num_verts_src, __func__);

				for (tidx = 0; tidx < num_trees; tidx++) {
					Mesh2MeshMappingIslandItem *isld = &islands.islands[tidx];
					if (isld) {
						int num_verts_active = 0;
						memset(verts_active, 0, sizeof(*verts_active) * (size_t)num_verts_src);
						for (i = 0; i < isld->nbr_polys; i++) {
							MPoly *mp_src = &polys_src[isld->polys_idx[i]];
							for (lidx_src = mp_src->loopstart; lidx_src < mp_src->loopstart + mp_src->totloop; lidx_src++) {
								verts_active[loops_src[lidx_src].v] = true;
								num_verts_active++;
							}
						}
						/* verts 'ownership' is transfered to treedata here, which will handle its freeing. */
						bvhtree_from_mesh_verts_ex(&treedata[tidx], verts_src, num_verts_src, verts_allocated_src,
						                           verts_active, num_verts_active, 0.0, 2, 6);
						if (verts_allocated_src) {
							verts_allocated_src = false;  /* Only 'give' our verts once, to first tree! */
						}
					}
				}

				MEM_freeN(verts_active);
			}
			else {
				for (tidx = 0; tidx < num_trees; tidx++) {
					bvhtree_from_mesh_verts(&treedata[tidx], dm_src, 0.0, 2, 6);
				}
			}
		}
		else {  /* We use polygons. */
			if (use_islands) {
				/* bvhtree here uses tesselated faces... */
				const unsigned int dirty_tess_flag = dm_src->dirty & DM_DIRTY_TESS_CDLAYERS;
				bool *faces_active;

				/* We do not care about tessellated data here, only geometry itself is important. */
				if (dirty_tess_flag) {
					dm_src->dirty &= ~dirty_tess_flag;
				}
				DM_ensure_tessface(dm_src);
				if (dirty_tess_flag) {
					dm_src->dirty |= dirty_tess_flag;
				}
				faces_src = DM_get_tessface_array(dm_src, &faces_allocated_src);
				num_faces_src = dm_src->getNumTessFaces(dm_src);
				orig_poly_idx_src = dm_src->getTessFaceDataArray(dm_src, CD_ORIGINDEX);
				faces_active = MEM_mallocN(sizeof(*faces_active) * (size_t)num_faces_src, __func__);

				for (tidx = 0; tidx < num_trees; tidx++) {
					Mesh2MeshMappingIslandItem *isld = gen_islands_src ? &islands.islands[tidx] : NULL;
					if (isld) {
						int num_faces_active = 0;
						memset(faces_active, 0, sizeof(*faces_active) * (size_t)num_faces_src);
						for (i = 0; i < num_faces_src; i++) {
							MPoly *mp_src = &polys_src[orig_poly_idx_src[i]];
							if (islands.loops_to_islands_idx[mp_src->loopstart] == tidx) {
								faces_active[i] = true;
								num_faces_active++;
							}
						}
						/* verts 'ownership' is transfered to treedata here, which will handle its freeing. */
						bvhtree_from_mesh_faces_ex(&treedata[tidx], verts_src, verts_allocated_src,
						                           faces_src, num_faces_src, faces_allocated_src,
						                           faces_active, num_faces_active, 0.0, 2, 6);
						if (verts_allocated_src) {
							verts_allocated_src = false;  /* Only 'give' our verts once, to first tree! */
						}
						if (faces_allocated_src) {
							faces_allocated_src = false;  /* Only 'give' our faces once, to first tree! */
						}
					}
				}

				MEM_freeN(faces_active);
			}
			else {
				for (tidx = 0; tidx < num_trees; tidx++) {
					bvhtree_from_mesh_faces(&treedata[tidx], dm_src, 0.0, 2, 6);
				}
				orig_poly_idx_src = dm_src->getTessFaceDataArray(dm_src, CD_ORIGINDEX);
			}
		}

		{
		/* facs: [0] is actual factor; [1] is hitdist; [2] is loop/poly src idx,
		 *       [3-5] are hit coordinates (if interpolated) */
		float (**facs)[6] = MEM_mallocN(sizeof(*facs) * (size_t)num_trees, __func__);
		size_t facs_buff_size = 32;
		for (tidx = 0; tidx < num_trees; tidx++) {
			facs[tidx] = MEM_mallocN(sizeof(**facs) * facs_buff_size, __func__);
		}

		/* And check each dest poly! */
		for (pidx_dst = 0; pidx_dst < numpolys_dst; pidx_dst++) {
			MPoly *mp_dst = &polys_dst[pidx_dst];
			float (*pnor_dst)[3] = &poly_nors_dst[pidx_dst];

			if ((size_t)mp_dst->totloop > facs_buff_size) {
				facs_buff_size = (size_t)mp_dst->totloop;
				for (tidx = 0; tidx < num_trees; tidx++) {
					facs[tidx] = MEM_reallocN(facs[tidx], sizeof(**facs) * facs_buff_size);
				}
			}

			for (tidx = 0; tidx < num_trees; tidx++) {
				BVHTreeFromMesh *tdata = &treedata[tidx];
				MLoop *ml_dst = &loops_dst[mp_dst->loopstart];

				for (plidx_dst = 0; plidx_dst < mp_dst->totloop; plidx_dst++, ml_dst++) {
					if (use_from_vert) {
						float tmp_co[3];

						copy_v3_v3(tmp_co, verts_dst[ml_dst->v].co);
						nearest.index = -1;

						hitdist = bke_mesh2mesh_bvhtree_query_nearest(tdata, &nearest, space_transform,
						                                              tmp_co, max_dist_sq);

						if (nearest.index >= 0) {
							float (*nor_dst)[3];
							float (*nors_src)[3];
							float best_nor_dot = -2.0f;
							int best_idx_src = -1;

							if (mode == M2MMAP_MODE_LOOP_NEAREST_LOOPNOR) {
								nor_dst = &loop_nors_dst[plidx_dst + mp_dst->loopstart];
								nors_src = loop_nors_src;
							}
							else {  /* if (mode == M2MMAP_MODE_LOOP_NEAREST_POLYNOR) { */
								nor_dst = pnor_dst;
								nors_src = poly_nors_src;
							}

							for (i = vert_to_polyloop_map_src[nearest.index].count; i--;) {
								const int idx_src = vert_to_polyloop_map_src[nearest.index].indices[i];
								const float dot = dot_v3v3(nors_src[idx_src], *nor_dst);
								if (dot > best_nor_dot) {
									best_nor_dot = dot;
									best_idx_src = idx_src;
								}
							}
							facs[tidx][plidx_dst][0] = (hitdist != 0.0f) ? (1.0f / hitdist * best_nor_dot) : FLT_MAX;
							facs[tidx][plidx_dst][1] = hitdist;
							facs[tidx][plidx_dst][2] = (float)best_idx_src;
						}
						else {
							/* No source for this dest loop! */
							facs[tidx][plidx_dst][0] = 0.0f;
							facs[tidx][plidx_dst][1] = FLT_MAX;
							facs[tidx][plidx_dst][2] = (float)-1;
						}
					}
					else if (mode & M2MMAP_USE_NORPROJ) {
						float tmp_co[3], tmp_no[3];
						const float radius = 1.0e-6f;

						copy_v3_v3(tmp_co, verts_dst[ml_dst->v].co);
						copy_v3_v3(tmp_no, loop_nors_dst[plidx_dst + mp_dst->loopstart]);

						hitdist = bke_mesh2mesh_bvhtree_query_raycast(treedata, &rayhit, space_transform,
						                                              tmp_co, tmp_no, radius, max_dist_sq);

						if (rayhit.index >= 0 && hitdist <= max_dist) {
							facs[tidx][plidx_dst][0] = (hitdist != 0.0f) ? (1.0f / hitdist) : FLT_MAX;
							facs[tidx][plidx_dst][1] = hitdist;
							facs[tidx][plidx_dst][2] = (float)orig_poly_idx_src[rayhit.index];
							copy_v3_v3(&facs[tidx][plidx_dst][3], rayhit.co);
						}
						else {
							/* No source for this dest loop! */
							facs[tidx][plidx_dst][0] = 0.0f;
							facs[tidx][plidx_dst][1] = FLT_MAX;
							facs[tidx][plidx_dst][2] = (float)-1;
						}
					}
					else {  /* Nearest poly either to use all its loops/verts or just closest one. */
						float tmp_co[3];

						/* Convert the vertex to tree coordinates. */
						copy_v3_v3(tmp_co, verts_dst[ml_dst->v].co);
						nearest.index = -1;

						hitdist = bke_mesh2mesh_bvhtree_query_nearest(treedata, &nearest, space_transform,
						                                              tmp_co, max_dist_sq);

						if (nearest.index >= 0) {
							facs[tidx][plidx_dst][0] = (hitdist != 0.0f) ? (1.0f / hitdist) : FLT_MAX;
							facs[tidx][plidx_dst][1] = hitdist;
							facs[tidx][plidx_dst][2] = (float)orig_poly_idx_src[rayhit.index];
							copy_v3_v3(&facs[tidx][plidx_dst][3], nearest.co);
						}
						else {
							/* No source for this dest loop! */
							facs[tidx][plidx_dst][0] = 0.0f;
							facs[tidx][plidx_dst][1] = FLT_MAX;
							facs[tidx][plidx_dst][2] = (float)-1;
						}
					}
				}
			}

			/* And now, find best island to use! */
			{
				float best_island_val = 0.0f;
				int best_island_idx = -1;

				for (tidx = 0; tidx < num_trees; tidx++) {
					float island_val = 0.0f;

					for (plidx_dst = 0; plidx_dst < mp_dst->totloop; plidx_dst++) {
						island_val += facs[tidx][plidx_dst][0];
					}
					island_val /= (float)mp_dst->totloop;

					if (island_val > best_island_val) {
						best_island_val = island_val;
						best_island_idx = tidx;
					}
				}

				for (plidx_dst = 0; plidx_dst < mp_dst->totloop; plidx_dst++) {
					lidx_dst = plidx_dst + mp_dst->loopstart;
					if (best_island_idx < 0) {
						/* No source for any loops of our dest poly in any source islands. */
						bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst], FLT_MAX, 0, 0,
						                                  NULL, NULL);
					}
					else if (use_from_vert) {
						/* Indices stored in facs are those of loops, one per dest loop. */
						lidx_src = (int)facs[tidx][plidx_dst][2];
						if (lidx_src >= 0) {
							bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst],
							                                  facs[best_island_idx][plidx_dst][1],
							                                  best_island_idx, 1, &lidx_src, &full_weight);
						}
						else {
							/* No source for this loop in this island. */
							/* TODO: would probably be better to get a source at all cost in best island anyway? */
							bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst], FLT_MAX, best_island_idx, 0,
							                                  NULL, NULL);
						}
					}
					else {
						/* Else, we use source poly, indices stored in facs are those of polygons. */
						pidx_src = (int)facs[tidx][plidx_dst][2];
						if (pidx_src >= 0) {
							MPoly *mp_src = &polys_src[pidx_src];
							float *hit_co = &facs[tidx][plidx_dst][3];
							int best_loop_idx_src;

							if (mode == M2MMAP_MODE_LOOP_POLY_NEAREST) {
								bke_mesh2mesh_mapping_get_interp_poly_data(
								        mp_src, loops_src, (const float (*)[3])vcos_src, hit_co,
								        &buff_size_interp, &vcos_interp, &indices_interp,
								        &weights_interp, false, NULL, &best_loop_idx_src);

								bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst],
								                                  facs[best_island_idx][plidx_dst][1], best_island_idx,
								                                  1, &best_loop_idx_src, &full_weight);
							}
							else {
								const int nbr_sources = bke_mesh2mesh_mapping_get_interp_poly_data(
								                                mp_src, loops_src, (const float (*)[3])vcos_src, hit_co,
								                                &buff_size_interp, &vcos_interp, &indices_interp,
								                                &weights_interp, true, NULL, NULL);

								bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst],
								                                  facs[best_island_idx][plidx_dst][1], best_island_idx,
								                                  nbr_sources, indices_interp, weights_interp);
							}
						}
						else {
							/* No source for this loop in this island. */
							/* TODO: would probably be better to get a source at all cost in best island anyway? */
							bke_mesh2mesh_mapping_item_define(&r_map->items[lidx_dst], FLT_MAX, best_island_idx, 0,
							                                  NULL, NULL);
						}
					}
				}
			}
		}

		for (tidx = 0; tidx < num_trees; tidx++) {
			MEM_freeN(facs[tidx]);
		}
		MEM_freeN(facs);
		}

		if (verts_allocated_src) {
			MEM_freeN(verts_src);
		}
		if (vcos_src) {
			MEM_freeN(vcos_src);
		}
		if (loops_allocated_src) {
			MEM_freeN(loops_src);
		}
		if (polys_allocated_src) {
			MEM_freeN(polys_src);
		}
		if (faces_allocated_src) {
			MEM_freeN(faces_src);
		}
		if (vert_to_polyloop_map_src_buff) {
			MEM_freeN(vert_to_polyloop_map_src_buff);
		}
		if (vcos_interp) {
			MEM_freeN(vcos_interp);
		}
		if (indices_interp) {
			MEM_freeN(indices_interp);
		}
		if (weights_interp) {
			MEM_freeN(weights_interp);
		}
		bke_mesh2mesh_mapping_islands_free(&islands);
		MEM_freeN(treedata);
	}
}

/** \} */
