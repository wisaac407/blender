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
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/bmesh/tools/bmesh_intersect.c
 *  \ingroup bmesh
 *
 * Cut meshes along intersections.
 *
 * Boolean-like modeling operation (without calculating inside/outside).
 *
 * Supported:
 * - Concave faces.
 * - Non-planar faces.
 * - Custom-data (UV's etc).
 *
 * Unsupported:
 * - Intersecting between different meshes.
 * - No support for holes (cutting a hole into a single face).
 */

#include "MEM_guardedalloc.h"

#include "BLI_math.h"
#include "BLI_utildefines.h"
#include "BLI_memarena.h"
#include "BLI_alloca.h"
#include "BLI_sort_utils.h"

#include "BLI_linklist_stack.h"
#include "BLI_stackdefines.h"
#ifndef NDEBUG
#  include "BLI_array_utils.h"
#endif

#include "BLI_kdopbvh.h"
#include "BLI_buffer.h"

#include "bmesh.h"
#include "bmesh_intersect.h"  /* own include */

#include "tools/bmesh_edgesplit.h"

#include "BLI_strict_flags.h"

/*
 * Some of these depend on each other:
 */

/* splice verts into existing edges */
#define USE_SPLICE
/* split faces by intersecting edges */
#define USE_NET
/* split resulting edges */
#define USE_SEPARATE
/* remove verts created by intersecting triangles */
#define USE_DISSOLVE
/* detect isolated holes and fill them */
#define USE_HOLE_FILL

#ifdef USE_HOLE_FILL
#  include "BLI_kdtree.h"
#  include "BLI_sort.h"
#endif

/* strict asserts that may fail in practice (handy for debugging cases which should succeed) */
// #define USE_PARANOID
/* use accelerated overlap check */
#define USE_BVH

#define USE_BOOLEAN_RAYCAST_DRAW
// #define USE_BOOLEAN_RAYCAST_OVERLAP  /* use bvhtree overlap otherwise raycast */

#ifdef USE_BOOLEAN_RAYCAST_DRAW
/* add these locally when using these functions for testing */
extern void bl_debug_draw_quad_clear(void);
extern void bl_debug_draw_quad_add(const float v0[3], const float v1[3], const float v2[3], const float v3[3]);
extern void bl_debug_draw_edge_add(const float v0[3], const float v1[3]);
extern void bl_debug_color_set(const unsigned int col);
#endif

#ifdef USE_HOLE_FILL

static int edge_xmin_cmp_fn(const void *p1, const void *p2, void *pdir)
{
	const float *f_dir = pdir;
	const BMEdge *e1 = *(BMEdge **)p1;
	const BMEdge *e2 = *(BMEdge **)p2;
	const float f1 = min_ff(dot_v3v3(f_dir, e1->v1->co), dot_v3v3(f_dir, e1->v2->co));
	const float f2 = min_ff(dot_v3v3(f_dir, e2->v1->co), dot_v3v3(f_dir, e2->v2->co));

	if (f1 < f2) return -1;
	if (f1 > f2) return  1;
	else         return  0;
}

static int kdtree_find_v_in_range_cb(void *user_data, int index, const float co[3], float dist_sq)
{
	UNUSED_VARS(co, dist_sq);
	const int *group_vert_range = user_data;
	return (index >= group_vert_range[0] && index < group_vert_range[1]);
}

static int kdtree_find_v_prev_range_cb(void *user_data, int index, const float co[3], float dist_sq)
{
	UNUSED_VARS(co, dist_sq);
	const int *group_vert_range = user_data;
	return (index < group_vert_range[0]);
}

/**
 * For when the edge-net has holes in it-this connects them.
 *
 * \warning This is wip test version of the functionality
 * (to test how having holes works with booleans).
 */
static bool  bm_face_split_edgenet_prepare_holes(
        BMesh *bm,
        BMFace *f, BMEdge **edge_net_init, const unsigned int edge_net_init_len,
        BMEdge ***r_edge_net_new, unsigned int *r_edge_net_new_len)
{
	const unsigned int edge_arr_len = (unsigned int)edge_net_init_len + (unsigned int)f->len;
	BMEdge **edge_arr = BLI_array_alloca(edge_arr, edge_arr_len);
	bool ok = false;

	memcpy(edge_arr, edge_net_init, sizeof(*edge_arr) * (size_t)edge_net_init_len);

	/* _must_ clear on exit */
#define VERT_NOT_IN_KDTREE BM_ELEM_INTERNAL_TAG
#define EDGE_NOT_IN_STACK  BM_ELEM_INTERNAL_TAG
	/* XXX this is wrong, we need some better way to know if an edge is in the net */
#define EDGE_IS_NET  BM_ELEM_SEAM

	{
		unsigned int i = edge_net_init_len;
		BMLoop *l_iter, *l_first;
		l_iter = l_first = BM_FACE_FIRST_LOOP(f);
		do {
			edge_arr[i++] = l_iter->e;
		} while ((l_iter = l_iter->next) != l_first);
		BLI_assert(i == edge_arr_len);
	}

	/* we don't care what axis we sort along, just that its from one side to another (across the face)
	 * so the first group is always the outer-most */
	float f_ortho_dir[3];
	ortho_v3_v3(f_ortho_dir, f->no);
	BLI_qsort_r(edge_arr, edge_arr_len, sizeof(BMEdge *), edge_xmin_cmp_fn, f_ortho_dir);

	for (unsigned int i = 0; i < edge_arr_len; i++) {
		BM_elem_flag_enable(edge_arr[i], EDGE_IS_NET);
		BM_elem_flag_enable(edge_arr[i], EDGE_NOT_IN_STACK);
		BM_elem_flag_enable(edge_arr[i]->v1, VERT_NOT_IN_KDTREE);
		BM_elem_flag_enable(edge_arr[i]->v2, VERT_NOT_IN_KDTREE);
	}

	LinkNode *groups = NULL;
	unsigned int groups_len = 0;
	{
		unsigned int edge_index = 0;
		unsigned int edge_in_group_tot = 0;

		BLI_SMALLSTACK_DECLARE(estack, BMEdge *);

		while (true) {
			LinkNode *g = NULL;
			unsigned int g_len = 0;

			/* list of groups */
			BLI_SMALLSTACK_PUSH(estack, edge_arr[edge_index]);
			BM_elem_flag_disable(edge_arr[edge_index], EDGE_NOT_IN_STACK);

			BMEdge *e;
			while ((e = BLI_SMALLSTACK_POP(estack))) {
				BLI_linklist_prepend_alloca(&g, e);
				g_len++;

				edge_in_group_tot++;

				for (int i = 0; i < 2; i++) {
					BMVert *v_iter = (&e->v1)[i];
					BMEdge *e_iter = v_iter->e;
					do {
						if ((e_iter != e) &&
						    (BM_elem_flag_test(e_iter, EDGE_NOT_IN_STACK)))
						{
							BLI_SMALLSTACK_PUSH(estack, e_iter);
							BM_elem_flag_disable(e_iter, EDGE_NOT_IN_STACK);
						}
					} while ((e_iter = BM_DISK_EDGE_NEXT(e_iter, v_iter)) != v_iter->e);
				}
			}

			struct {
				LinkNode node;
				unsigned int edge_len, vert_len;
			} *group_base = alloca(sizeof(*group_base));
			group_base->edge_len = g_len;
			group_base->vert_len = 0;
			BLI_linklist_prepend_nlink(&groups, g, (LinkNode *)group_base);

			groups_len++;

			if (edge_in_group_tot == edge_arr_len) {
				break;
			}

			/* skip edges in the stack */
			while (BM_elem_flag_test(edge_arr[edge_index], EDGE_NOT_IN_STACK) == false) {
				edge_index++;
			}
		}
	}

	/* single group - no holes */
	if (groups_len == 1) {
		goto finally;
	}

	struct {
		LinkNode node;
		unsigned int edge_len, vert_len;
	} *group_base, **groups_arr = BLI_array_alloca(groups_arr, groups_len);
	KDTree *tree = BLI_kdtree_new(edge_arr_len * 2);
	BMVert **verts_arr = BLI_array_alloca(verts_arr, edge_arr_len * 2);
	unsigned int verts_len = 0;

	{
		group_base = (void *)groups;
		for (unsigned int i = 0; i < groups_len; i++, group_base = (void *)group_base->node.next) {
			/* fill the kdtree */


			LinkNode *g = group_base->node.link;
			do {
				BMEdge *e = g->link;
				BLI_assert(e->head.htype == BM_EDGE);

				for (int j = 0; j < 2; j++) {
					BMVert *v_iter = (&e->v1)[j];
					BLI_assert(v_iter->head.htype == BM_VERT);
					if (BM_elem_flag_test(v_iter, VERT_NOT_IN_KDTREE)) {
						BM_elem_flag_disable(v_iter, VERT_NOT_IN_KDTREE);
						BLI_kdtree_insert(tree, (int)verts_len, v_iter->co);
						verts_arr[verts_len] = v_iter;

						verts_len++;
						group_base->vert_len++;
					}
				}
			} while ((g = g->next));

			groups_arr[i] = group_base;
		}
	}

	BLI_kdtree_balance(tree);

	const unsigned int edge_net_new_len = (unsigned int)edge_net_init_len + ((groups_len - 1) * 2);

	BMEdge **edge_net_new = MEM_mallocN(sizeof(*edge_net_new) * edge_net_new_len, __func__);

	memcpy(edge_net_new, edge_net_init, sizeof(*edge_net_new) * (size_t)edge_net_init_len);

	{
		const unsigned int refine = 4;
		unsigned int edge_net_new_index = edge_net_init_len;
		/* start-end of the verts in the current group */
		unsigned int group_vert_range[2] = {0, groups_arr[0]->vert_len};
		for (unsigned int group_index = 1; group_index < groups_len; group_index++) {
			BMVert *v_start;
			/* left most vert of left most edge */
			{
				BMEdge *e = groups_arr[group_index]->node.link;
				v_start = (dot_v3v3(f_ortho_dir, e->v1->co) <
				           dot_v3v3(f_ortho_dir, e->v2->co)) ? e->v1 : e->v2;
			}

			/* the range of verts this group uses in 'verts_arr' (not uncluding the last index) */
			group_vert_range[0]  = group_vert_range[1];
			group_vert_range[1] += groups_arr[group_index]->vert_len;

			BMVert *vpair_best[2] = {v_start, NULL};
			for (unsigned int i = 0; i < refine; i++) {
				unsigned int src_side, dst_side;

				if (i % 2) {
					src_side = 1;
					dst_side = 0;
				}
				else {
					src_side = 0;
					dst_side = 1;
				}

				const int index = BLI_kdtree_find_nearest_cb(
				        tree, vpair_best[src_side]->co,
				        src_side ? kdtree_find_v_prev_range_cb : kdtree_find_v_in_range_cb, group_vert_range,
				        NULL);

				BLI_assert(index >= 0);
				BLI_assert(index < (int)verts_len);
				vpair_best[dst_side] = verts_arr[index];
			}

			BMVert *vpair_other[2] = {NULL, NULL};

			/* define a plane to make sure we don't create bow-tie */
			float v_best_plane[4];
			{
				float dir[3];
				sub_v3_v3v3(dir, vpair_best[1]->co, vpair_best[0]->co);
				cross_v3_v3v3(v_best_plane, f->no, dir);
				v_best_plane[3] = -dot_v3v3(vpair_best[0]->co, v_best_plane);
			}

			/* find closest pair */
			{
				BMVert *v_a = vpair_best[0];
				BMEdge *e_a = v_a->e;
				float vpair_other_best_length = -1.0f;
				do {
					if (BM_elem_flag_test(e_a, EDGE_IS_NET)) {
						BMVert *v_a_other = BM_edge_other_vert(e_a, v_a);
						const float v_a_other_plane_side =
						        dist_signed_squared_to_plane_v3(v_a_other->co, v_best_plane);


						BMVert *v_b = vpair_best[1];
						BMEdge *e_b = v_b->e;
						do {
							if (BM_elem_flag_test(e_b, EDGE_IS_NET)) {
								BMVert *v_b_other = BM_edge_other_vert(e_b, v_b);
								const float v_b_other_plane_side =
								        dist_signed_squared_to_plane_v3(v_b_other->co, v_best_plane);

								/* avoid bow-tie */
								if ((v_a_other_plane_side < 0.0f) == (v_b_other_plane_side < 0.0f)) {
									const float vpair_other_test_length =
									        len_squared_v3v3(v_a_other->co, v_b_other->co);

									if ((vpair_other_best_length == -1.0f) ||
									    (vpair_other_test_length < vpair_other_best_length))
									{
										vpair_other[0] = v_a_other;
										vpair_other[1] = v_b_other;
										vpair_other_best_length = vpair_other_test_length;
									}
								}
							}
						} while ((e_b = BM_DISK_EDGE_NEXT(e_b, v_b)) != v_b->e);
					}
				} while ((e_a = BM_DISK_EDGE_NEXT(e_a, v_a)) != v_a->e);
			}

			edge_net_new[edge_net_new_index] = BM_edge_create(bm, UNPACK2(vpair_best), NULL, 0);
			BM_elem_flag_enable(edge_net_new[edge_net_new_index], BM_ELEM_TAG);
			edge_net_new_index++;

			edge_net_new[edge_net_new_index] = BM_edge_create(bm, UNPACK2(vpair_other), NULL, 0);
			BM_elem_flag_enable(edge_net_new[edge_net_new_index], BM_ELEM_TAG);
			edge_net_new_index++;
		}
		BLI_assert(edge_net_new_len == edge_net_new_index);
	}


	BLI_kdtree_free(tree);

	*r_edge_net_new = edge_net_new;
	*r_edge_net_new_len = edge_net_new_len;
	ok = true;

finally:
	for (unsigned int i = 0; i < edge_arr_len; i++) {
		BM_elem_flag_disable(edge_arr[i], EDGE_IS_NET);
		BM_elem_flag_disable(edge_arr[i], EDGE_NOT_IN_STACK);
		BM_elem_flag_disable(edge_arr[i]->v1, VERT_NOT_IN_KDTREE);
		BM_elem_flag_disable(edge_arr[i]->v2, VERT_NOT_IN_KDTREE);
	}

#undef VERT_NOT_IN_KDTREE
#undef EDGE_NOT_IN_STACK
#undef EDGE_IS_NET

	return ok;
}
#endif  /* USE_HOLE_FILL */


enum {
	BOOLEAN_NONE = -1,
	/* aligned with BooleanModifierOp */
	BOOLEAN_ISECT = 0,
	BOOLEAN_UNION = 1,
	BOOLEAN_DIFFERENCE = 2,
};


static void tri_v3_scale(
        float v1[3], float v2[3], float v3[3],
        const float t)
{
	float p[3];

	mid_v3_v3v3v3(p, v1, v2, v3);

	interp_v3_v3v3(v1, p, v1, t);
	interp_v3_v3v3(v2, p, v2, t);
	interp_v3_v3v3(v3, p, v3, t);
}

#ifdef USE_DISSOLVE
/* other edge when a vert only has 2 edges */
static BMEdge *bm_vert_other_edge(BMVert *v, BMEdge *e)
{
	BLI_assert(BM_vert_is_edge_pair(v));
	BLI_assert(BM_vert_in_edge(e, v));

	if (v->e != e) {
		return v->e;
	}
	else {
		return BM_DISK_EDGE_NEXT(v->e, v);
	}
}
#endif

enum ISectType {
	IX_NONE = -1,
	IX_EDGE_TRI_EDGE0,
	IX_EDGE_TRI_EDGE1,
	IX_EDGE_TRI_EDGE2,
	IX_EDGE_TRI,
	IX_TOT,
};

struct ISectEpsilon {
	float eps, eps_sq;
	float eps2x, eps2x_sq;
	float eps_margin, eps_margin_sq;
};

struct ISectState {
	BMesh *bm;
	GHash *edgetri_cache;  /* int[4]: BMVert */
	GHash *edge_verts;  /* BMEdge: LinkList(of verts), new and original edges */
	GHash *face_edges;  /* BMFace-index: LinkList(of edges), only original faces */
	GSet  *wire_edges;  /* BMEdge  (could use tags instead) */
	LinkNode *vert_dissolve;  /* BMVert's */

	MemArena *mem_arena;

	struct ISectEpsilon epsilon;
};

/**
 * Store as value in GHash so we can get list-length without counting every time.
 * Also means we don't need to update the GHash value each time.
 */
struct LinkBase {
	LinkNode    *list;
	unsigned int list_len;
};

static bool ghash_insert_link(
        GHash *gh, void *key, void *val, bool use_test,
        MemArena *mem_arena)
{
	struct LinkBase *ls_base;
	LinkNode *ls;

	ls_base = BLI_ghash_lookup(gh, key);

	if (ls_base) {
		if (use_test && (BLI_linklist_index(ls_base->list, key) != -1)) {
			return false;
		}
	}
	else {
		ls_base = BLI_memarena_alloc(mem_arena, sizeof(*ls_base));
		ls_base->list     = NULL;
		ls_base->list_len = 0;
		BLI_ghash_insert(gh, key, ls_base);
	}

	ls = BLI_memarena_alloc(mem_arena, sizeof(*ls));
	ls->next = ls_base->list;
	ls->link = val;
	ls_base->list = ls;
	ls_base->list_len += 1;

	return true;
}

struct vert_sort_t {
	float val;
	BMVert *v;
};

#ifdef USE_SPLICE
static void edge_verts_sort(const float co[3], struct LinkBase *v_ls_base)
{
	/* not optimal but list will be typically < 5 */
	unsigned int i;
	struct vert_sort_t *vert_sort = BLI_array_alloca(vert_sort, v_ls_base->list_len);
	LinkNode *node;

	BLI_assert(v_ls_base->list_len > 1);

	for (i = 0, node = v_ls_base->list; i < v_ls_base->list_len; i++, node = node->next) {
		BMVert *v = node->link;
		BLI_assert(v->head.htype == BM_VERT);
		vert_sort[i].val = len_squared_v3v3(co, v->co);
		vert_sort[i].v   = v;
	}

	qsort(vert_sort, v_ls_base->list_len, sizeof(*vert_sort), BLI_sortutil_cmp_float);

	for (i = 0, node = v_ls_base->list; i < v_ls_base->list_len; i++, node = node->next) {
		node->link = vert_sort[i].v;
	}
}
#endif

static void edge_verts_add(
        struct ISectState *s,
        BMEdge *e,
        BMVert *v,
        const bool use_test
        )
{
	BLI_assert(e->head.htype == BM_EDGE);
	BLI_assert(v->head.htype == BM_VERT);
	ghash_insert_link(s->edge_verts, (void *)e, v, use_test, s->mem_arena);
}

static void face_edges_add(
        struct ISectState *s,
        const int f_index,
        BMEdge *e,
        const bool use_test)
{
	void *f_index_key = SET_INT_IN_POINTER(f_index);
	BLI_assert(e->head.htype == BM_EDGE);
	BLI_assert(BM_edge_in_face(e, s->bm->ftable[f_index]) == false);
	BLI_assert(BM_elem_index_get(s->bm->ftable[f_index]) == f_index);

	ghash_insert_link(s->face_edges, f_index_key, e, use_test, s->mem_arena);
}

#ifdef USE_NET
static void face_edges_split(
        BMesh *bm,
        BMFace *f,
        struct LinkBase *e_ls_base)
{
	unsigned int i;
	unsigned int edge_arr_len = e_ls_base->list_len;
	BMEdge **edge_arr = BLI_array_alloca(edge_arr, edge_arr_len);
#ifdef USE_HOLE_FILL
	bool edge_arr_free = false;
#endif
	LinkNode *node;
	BLI_assert(f->head.htype == BM_FACE);

	for (i = 0, node = e_ls_base->list; i < e_ls_base->list_len; i++, node = node->next) {
		edge_arr[i] = node->link;
	}
	BLI_assert(node == NULL);

#ifdef USE_DUMP
	printf("# %s: %d %u\n", __func__, BM_elem_index_get(f), e_ls_base->list_len);
#endif

#ifdef USE_HOLE_FILL
	{
		unsigned int edge_arr_holes_len;
		BMEdge **edge_arr_holes;
		if (bm_face_split_edgenet_prepare_holes(
		        bm, f,
		        edge_arr, edge_arr_len,
		        &edge_arr_holes, &edge_arr_holes_len))
		{
			edge_arr_len = edge_arr_holes_len;
			edge_arr = edge_arr_holes;
			edge_arr_free = true;
		}
	}
#endif

	BM_face_split_edgenet(bm, f, edge_arr, (int)edge_arr_len, NULL, NULL);

#ifdef USE_HOLE_FILL
	if (edge_arr_free) {
		MEM_freeN(edge_arr);
	}
#endif
}
#endif

#ifdef USE_DISSOLVE
static void vert_dissolve_add(
        struct ISectState *s,
        BMVert *v)
{
	BLI_assert(v->head.htype == BM_VERT);
	BLI_assert(!BM_elem_flag_test(v, BM_ELEM_TAG));
	BLI_assert(BLI_linklist_index(s->vert_dissolve, v) == -1);

	BM_elem_flag_enable(v, BM_ELEM_TAG);
	BLI_linklist_prepend_arena(&s->vert_dissolve, v, s->mem_arena);
}
#endif

static enum ISectType intersect_line_tri(
        const float p0[3], const float p1[3],
        const float *t_cos[3], const float t_nor[3],
        float r_ix[3],
        const struct ISectEpsilon *e)
{
	float p_dir[3];
	unsigned int i_t0;
	float fac;

	sub_v3_v3v3(p_dir, p0, p1);
	normalize_v3(p_dir);

	for (i_t0 = 0; i_t0 < 3; i_t0++) {
		const unsigned int i_t1 = (i_t0 + 1) % 3;
		float te_dir[3];

		sub_v3_v3v3(te_dir, t_cos[i_t0], t_cos[i_t1]);
		normalize_v3(te_dir);
		if (fabsf(dot_v3v3(p_dir, te_dir)) >= 1.0f - e->eps) {
			/* co-linear */
		}
		else {
			float ix_pair[2][3];
			int ix_pair_type;

			ix_pair_type = isect_line_line_epsilon_v3(p0, p1, t_cos[i_t0], t_cos[i_t1], ix_pair[0], ix_pair[1], 0.0f);

			if (ix_pair_type != 0) {
				if (ix_pair_type == 1) {
					copy_v3_v3(ix_pair[1], ix_pair[0]);
				}

				if ((ix_pair_type == 1) ||
				    (len_squared_v3v3(ix_pair[0], ix_pair[1]) <= e->eps_margin_sq))
				{
					fac = line_point_factor_v3(ix_pair[1], t_cos[i_t0], t_cos[i_t1]);
					if ((fac >= e->eps_margin) && (fac <= 1.0f - e->eps_margin)) {
						fac = line_point_factor_v3(ix_pair[0], p0, p1);
						if ((fac >= e->eps_margin) && (fac <= 1.0f - e->eps_margin)) {
							copy_v3_v3(r_ix, ix_pair[0]);
							return (IX_EDGE_TRI_EDGE0 + (enum ISectType)i_t0);
						}
					}
				}
			}
		}
	}

	/* check ray isn't planar with tri */
	if (fabsf(dot_v3v3(p_dir, t_nor)) >= e->eps) {
		if (isect_line_tri_epsilon_v3(p0, p1, t_cos[0], t_cos[1], t_cos[2], &fac, NULL, 0.0f)) {
			if ((fac >= e->eps_margin) && (fac <= 1.0f - e->eps_margin)) {
				interp_v3_v3v3(r_ix, p0, p1, fac);
				if (min_fff(len_squared_v3v3(t_cos[0], r_ix),
				            len_squared_v3v3(t_cos[1], r_ix),
				            len_squared_v3v3(t_cos[2], r_ix)) >= e->eps_margin_sq)
				{
					return IX_EDGE_TRI;
				}
			}
		}
	}

	/* r_ix may be unset */
	return IX_NONE;
}

static BMVert *bm_isect_edge_tri(
        struct ISectState *s,
        BMVert *e_v0, BMVert *e_v1,
        BMVert *t[3], const int t_index,
        const float *t_cos[3], const float t_nor[3],
        enum ISectType *r_side)
{
	BMesh *bm = s->bm;
	int k_arr[IX_TOT][4];
	unsigned int i;
	const int ti[3] = {UNPACK3_EX(BM_elem_index_get, t, )};
	float ix[3];

	if (BM_elem_index_get(e_v0) > BM_elem_index_get(e_v1)) {
		SWAP(BMVert *, e_v0, e_v1);
	}

#ifdef USE_PARANOID
	BLI_assert(len_squared_v3v3(e_v0->co, t[0]->co) >= s->epsilon.eps_sq);
	BLI_assert(len_squared_v3v3(e_v0->co, t[1]->co) >= s->epsilon.eps_sq);
	BLI_assert(len_squared_v3v3(e_v0->co, t[2]->co) >= s->epsilon.eps_sq);
	BLI_assert(len_squared_v3v3(e_v1->co, t[0]->co) >= s->epsilon.eps_sq);
	BLI_assert(len_squared_v3v3(e_v1->co, t[1]->co) >= s->epsilon.eps_sq);
	BLI_assert(len_squared_v3v3(e_v1->co, t[2]->co) >= s->epsilon.eps_sq);
#endif

#define KEY_SET(k, i0, i1, i2, i3) { \
	(k)[0] = i0; \
	(k)[1] = i1; \
	(k)[2] = i2; \
	(k)[3] = i3; \
} (void)0

	/* order tri, then order (1-2, 2-3)*/
#define KEY_EDGE_TRI_ORDER(k) { \
	if (k[2] > k[3]) { \
		SWAP(int, k[2], k[3]); \
	} \
	if (k[0] > k[2]) { \
		SWAP(int, k[0], k[2]); \
		SWAP(int, k[1], k[3]); \
	} \
} (void)0

	KEY_SET(k_arr[IX_EDGE_TRI], BM_elem_index_get(e_v0), BM_elem_index_get(e_v1), t_index, -1);
	/* need to order here */
	KEY_SET(k_arr[IX_EDGE_TRI_EDGE0], BM_elem_index_get(e_v0), BM_elem_index_get(e_v1), ti[0], ti[1]);
	KEY_SET(k_arr[IX_EDGE_TRI_EDGE1], BM_elem_index_get(e_v0), BM_elem_index_get(e_v1), ti[1], ti[2]);
	KEY_SET(k_arr[IX_EDGE_TRI_EDGE2], BM_elem_index_get(e_v0), BM_elem_index_get(e_v1), ti[2], ti[0]);

	KEY_EDGE_TRI_ORDER(k_arr[IX_EDGE_TRI_EDGE0]);
	KEY_EDGE_TRI_ORDER(k_arr[IX_EDGE_TRI_EDGE1]);
	KEY_EDGE_TRI_ORDER(k_arr[IX_EDGE_TRI_EDGE2]);

#undef KEY_SET
#undef KEY_EDGE_TRI_ORDER



	for (i = 0; i < ARRAY_SIZE(k_arr); i++) {
		BMVert *iv;

		iv = BLI_ghash_lookup(s->edgetri_cache, k_arr[i]);

		if (iv) {
#ifdef USE_DUMP
			printf("# cache hit (%d, %d, %d, %d)\n", UNPACK4(k_arr[i]));
#endif
			*r_side = (enum ISectType)i;
			return iv;
		}
	}

	*r_side = intersect_line_tri(e_v0->co, e_v1->co, t_cos, t_nor, ix, &s->epsilon);
	if (*r_side != IX_NONE) {
		BMVert *iv;
		BMEdge *e;
#ifdef USE_DUMP
		printf("# new vertex (%.6f, %.6f, %.6f) %d\n", UNPACK3(ix), *r_side);
#endif

#ifdef USE_PARANOID
		BLI_assert(len_squared_v3v3(ix, e_v0->co) > s->epsilon.eps_sq);
		BLI_assert(len_squared_v3v3(ix, e_v1->co) > s->epsilon.eps_sq);
		BLI_assert(len_squared_v3v3(ix, t[0]->co) > s->epsilon.eps_sq);
		BLI_assert(len_squared_v3v3(ix, t[1]->co) > s->epsilon.eps_sq);
		BLI_assert(len_squared_v3v3(ix, t[2]->co) > s->epsilon.eps_sq);
#endif
		iv = BM_vert_create(bm, ix, NULL, 0);

		e = BM_edge_exists(e_v0, e_v1);
		if (e) {
#ifdef USE_DUMP
			printf("# adding to edge %d\n", BM_elem_index_get(e));
#endif
			edge_verts_add(s, e, iv, false);
		}
		else {
#ifdef USE_DISSOLVE
			vert_dissolve_add(s, iv);
#endif
		}

		if ((*r_side >= IX_EDGE_TRI_EDGE0) && (*r_side <= IX_EDGE_TRI_EDGE2)) {
			i = (unsigned int)(*r_side - IX_EDGE_TRI_EDGE0);
			e = BM_edge_exists(t[i], t[(i + 1) % 3]);
			if (e) {
				edge_verts_add(s, e, iv, false);
			}
		}

		{
			int *k = BLI_memarena_alloc(s->mem_arena, sizeof(int[4]));
			memcpy(k, k_arr[*r_side], sizeof(int[4]));
			BLI_ghash_insert(s->edgetri_cache, k, iv);
		}

		return iv;

	}

	*r_side = IX_NONE;

	return NULL;
}

static bool bm_loop_filter_fn(const BMLoop *l, void *UNUSED(user_data))
{
	if (BM_elem_flag_test(l->e, BM_ELEM_TAG)) {
		return false;
	}

	if (l->radial_next != l) {
		BMLoop *l_iter = l->radial_next;
		const char hflag_test = BM_ELEM_DRAW;  /* XXX, set in MOD_boolean.c */
		const char hflag = BM_elem_flag_test(l->f, hflag_test);
		do {
			if (BM_elem_flag_test(l_iter->f, hflag_test) != hflag) {
				return false;
			}
		} while ((l_iter = l_iter->radial_next) != l);
		return true;
	}
	return false;
}

/**
 * Return true if we have any intersections.
 */
static void bm_isect_tri_tri(
        struct ISectState *s,
        int a_index, int b_index,
        BMLoop **a, BMLoop **b)
{
	BMFace *f_a = (*a)->f;
	BMFace *f_b = (*b)->f;
	BMVert *fv_a[3] = {UNPACK3_EX(, a, ->v)};
	BMVert *fv_b[3] = {UNPACK3_EX(, b, ->v)};
	const float *f_a_cos[3] = {UNPACK3_EX(, fv_a, ->co)};
	const float *f_b_cos[3] = {UNPACK3_EX(, fv_b, ->co)};
	float f_a_nor[3];
	float f_b_nor[3];
	int a_mask = 0;
	int b_mask = 0;
	unsigned int i;


	/* should be enough but may need to bump */
	BMVert *iv_ls_a[8];
	BMVert *iv_ls_b[8];
	STACK_DECLARE(iv_ls_a);
	STACK_DECLARE(iv_ls_b);

	if (UNLIKELY(ELEM(fv_a[0], UNPACK3(fv_b)) ||
	             ELEM(fv_a[1], UNPACK3(fv_b)) ||
	             ELEM(fv_a[2], UNPACK3(fv_b))))
	{
		return;
	}

	STACK_INIT(iv_ls_a, ARRAY_SIZE(iv_ls_a));
	STACK_INIT(iv_ls_b, ARRAY_SIZE(iv_ls_b));

	/* vert-vert
	 * --------- */
	{
		/* first check in any verts are touching
		 * (any case where we wont create new verts)
		 */
		unsigned int i_a;
		for (i_a = 0; i_a < 3; i_a++) {
			unsigned int i_b;
			for (i_b = 0; i_b < 3; i_b++) {
				if (len_squared_v3v3(fv_a[i_a]->co, fv_b[i_b]->co) <= s->epsilon.eps2x_sq) {
					if (!((1 << i_a) & a_mask)) {
						STACK_PUSH(iv_ls_a, fv_a[i_a]);
						a_mask |= (1 << i_a);
#ifdef USE_DUMP
						printf("  ('VERT-VERT-A') %d, %d),\n",
						       i_a, BM_elem_index_get(fv_a[i_a]));
#endif
					}
					if (!((1 << i_b) & b_mask)) {
						STACK_PUSH(iv_ls_b, fv_b[i_b]);
						b_mask |= (1 << i_b);
#ifdef USE_DUMP
						printf("  ('VERT-VERT-B') %d, %d),\n",
						       i_b, BM_elem_index_get(fv_b[i_b]));
#endif
					}
				}
			}
		}
	}

	/* vert-edge
	 * --------- */
	{
		unsigned int i_a;
		for (i_a = 0; i_a < 3; i_a++) {
			if (!((1 << i_a) & a_mask)) {
				unsigned int i_b_e0;
				for (i_b_e0 = 0; i_b_e0 < 3; i_b_e0++) {
					unsigned int i_b_e1 = (i_b_e0 + 1) % 3;
					float fac;
					if (((1 << i_b_e0) | (1 << i_b_e1)) & b_mask)
						continue;
					fac = line_point_factor_v3(fv_a[i_a]->co, fv_b[i_b_e0]->co, fv_b[i_b_e1]->co);
					if ((fac > 0.0f - s->epsilon.eps) && (fac < 1.0f + s->epsilon.eps)) {
						float ix[3];
						interp_v3_v3v3(ix, fv_b[i_b_e0]->co, fv_b[i_b_e1]->co, fac);
						if (len_squared_v3v3(ix, fv_a[i_a]->co) <= s->epsilon.eps2x_sq) {
							BMEdge *e;
							STACK_PUSH(iv_ls_b, fv_a[i_a]);
							// STACK_PUSH(iv_ls_a, fv_a[i_a]);
							a_mask |= (1 << i_a);
							e = BM_edge_exists(fv_b[i_b_e0], fv_b[i_b_e1]);
#ifdef USE_DUMP
							printf("  ('VERT-EDGE-A', %d, %d),\n",
							       BM_elem_index_get(fv_b[i_b_e0]), BM_elem_index_get(fv_b[i_b_e1]));
#endif
							if (e) {
#ifdef USE_DUMP
								printf("# adding to edge %d\n", BM_elem_index_get(e));
#endif
								edge_verts_add(s, e, fv_a[i_a], true);
							}
							break;
						}
					}
				}
			}
		}
	}

	{
		unsigned int i_b;
		for (i_b = 0; i_b < 3; i_b++) {
			if (!((1 << i_b) & b_mask)) {
				unsigned int i_a_e0;
				for (i_a_e0 = 0; i_a_e0 < 3; i_a_e0++) {
					unsigned int i_a_e1 = (i_a_e0 + 1) % 3;
					float fac;
					if (((1 << i_a_e0) | (1 << i_a_e1)) & a_mask)
						continue;
					fac = line_point_factor_v3(fv_b[i_b]->co, fv_a[i_a_e0]->co, fv_a[i_a_e1]->co);
					if ((fac > 0.0f - s->epsilon.eps) && (fac < 1.0f + s->epsilon.eps)) {
						float ix[3];
						interp_v3_v3v3(ix, fv_a[i_a_e0]->co, fv_a[i_a_e1]->co, fac);
						if (len_squared_v3v3(ix, fv_b[i_b]->co) <= s->epsilon.eps2x_sq) {
							BMEdge *e;
							STACK_PUSH(iv_ls_a, fv_b[i_b]);
							// STACK_PUSH(iv_ls_b, fv_b[i_b]);
							b_mask |= (1 << i_b);
							e = BM_edge_exists(fv_a[i_a_e0], fv_a[i_a_e1]);
#ifdef USE_DUMP
							printf("  ('VERT-EDGE-B', %d, %d),\n",
							       BM_elem_index_get(fv_a[i_a_e0]), BM_elem_index_get(fv_a[i_a_e1]));
#endif
							if (e) {
#ifdef USE_DUMP
								printf("  adding to edge %d\n", BM_elem_index_get(e));
#endif
								edge_verts_add(s, e, fv_b[i_b], true);
							}
							break;
						}
					}
				}
			}
		}
	}

	/* vert-tri
	 * -------- */
	{

		float t_scale[3][3];
		unsigned int i_a;

		copy_v3_v3(t_scale[0], fv_b[0]->co);
		copy_v3_v3(t_scale[1], fv_b[1]->co);
		copy_v3_v3(t_scale[2], fv_b[2]->co);
		tri_v3_scale(UNPACK3(t_scale), 1.0f - s->epsilon.eps2x);

		// second check for verts intersecting the triangle
		for (i_a = 0; i_a < 3; i_a++) {
			float ix[3];
			if ((1 << i_a) & a_mask)
				continue;
			if (isect_point_tri_v3(fv_a[i_a]->co, UNPACK3(t_scale), ix)) {
				if (len_squared_v3v3(ix, fv_a[i_a]->co) <= s->epsilon.eps2x_sq) {
					BLI_assert(BLI_array_findindex(iv_ls_a, STACK_SIZE(iv_ls_a), fv_a[i_a]) == -1);
					BLI_assert(BLI_array_findindex(iv_ls_b, STACK_SIZE(iv_ls_b), fv_a[i_a]) == -1);

					STACK_PUSH(iv_ls_a, fv_a[i_a]);
					STACK_PUSH(iv_ls_b, fv_a[i_a]);
					a_mask |= (1 << i_a);
#ifdef USE_DUMP
					printf("  'VERT TRI-A',\n");
#endif
				}
			}
		}
	}

	{
		float t_scale[3][3];
		unsigned int i_b;

		copy_v3_v3(t_scale[0], fv_a[0]->co);
		copy_v3_v3(t_scale[1], fv_a[1]->co);
		copy_v3_v3(t_scale[2], fv_a[2]->co);
		tri_v3_scale(UNPACK3(t_scale), 1.0f - s->epsilon.eps2x);

		for (i_b = 0; i_b < 3; i_b++) {
			float ix[3];
			if ((1 << i_b) & b_mask)
				continue;

			if (isect_point_tri_v3(fv_b[i_b]->co, UNPACK3(t_scale), ix)) {
				if (len_squared_v3v3(ix, fv_b[i_b]->co) <= s->epsilon.eps2x_sq) {
					BLI_assert(BLI_array_findindex((void **)iv_ls_a, STACK_SIZE(iv_ls_a), fv_b[i_b]) == -1);
					BLI_assert(BLI_array_findindex((void **)iv_ls_b, STACK_SIZE(iv_ls_b), fv_b[i_b]) == -1);

					STACK_PUSH(iv_ls_a, fv_b[i_b]);
					STACK_PUSH(iv_ls_b, fv_b[i_b]);
					b_mask |= (1 << i_b);
#ifdef USE_DUMP
					printf("  'VERT TRI-B',\n");
#endif
				}
			}
		}
	}

	if ((STACK_SIZE(iv_ls_a) >= 3) &&
	    (STACK_SIZE(iv_ls_b) >= 3))
	{
#ifdef USE_DUMP
		printf("# OVERLAP\n");
#endif
		return;
	}

	normal_tri_v3(f_a_nor, UNPACK3(f_a_cos));
	normal_tri_v3(f_b_nor, UNPACK3(f_b_cos));

	/* edge-tri & edge-edge
	 * -------------------- */
	{
		unsigned int i_e0;
		for (i_e0 = 0; i_e0 < 3; i_e0++) {
			unsigned int i_e1 = (i_e0 + 1) % 3;
			enum ISectType side;
			BMVert *iv;
			if (((1 << i_e0) | (1 << i_e1)) & a_mask)
				continue;
			iv = bm_isect_edge_tri(s, fv_a[i_e0], fv_a[i_e1], fv_b, b_index, f_b_cos, f_b_nor, &side);
			if (iv) {
				BLI_assert(BLI_array_findindex((void **)iv_ls_a, STACK_SIZE(iv_ls_a), iv) == -1);
				BLI_assert(BLI_array_findindex((void **)iv_ls_b, STACK_SIZE(iv_ls_b), iv) == -1);
				STACK_PUSH(iv_ls_a, iv);
				STACK_PUSH(iv_ls_b, iv);
#ifdef USE_DUMP
				printf("  ('EDGE-TRI-A', %d),\n", side);
#endif
			}
		}

		for (i_e0 = 0; i_e0 < 3; i_e0++) {
			unsigned int i_e1 = (i_e0 + 1) % 3;
			enum ISectType side;
			BMVert *iv;
			if (((1 << i_e0) | (1 << i_e1)) & b_mask)
				continue;
			iv = bm_isect_edge_tri(s, fv_b[i_e0], fv_b[i_e1], fv_a, a_index, f_a_cos, f_a_nor, &side);
			if (iv) {
				/* check this wasn't handled above */
				if (!(side >= IX_EDGE_TRI_EDGE0 && side <= IX_EDGE_TRI_EDGE2)) {
					BLI_assert(BLI_array_findindex((void **)iv_ls_a, STACK_SIZE(iv_ls_a), iv) == -1);
					BLI_assert(BLI_array_findindex((void **)iv_ls_b, STACK_SIZE(iv_ls_b), iv) == -1);
					STACK_PUSH(iv_ls_a, iv);
					STACK_PUSH(iv_ls_b, iv);
#ifdef USE_DUMP
					printf("  ('EDGE-RAY-B', %d),\n", side);
#endif
				}
			}
		}
	}

	{
		for (i = 0; i < 2; i++) {
			BMVert **ie_vs;
			BMFace *f;
			bool ie_exists;
			BMEdge *ie;

			if (i == 0) {
				if (STACK_SIZE(iv_ls_a) != 2)
					continue;
				ie_vs = iv_ls_a;
				f = f_a;
			}
			else {
				if (STACK_SIZE(iv_ls_b) != 2)
					continue;
				ie_vs = iv_ls_b;
				f = f_b;
			}

			/* possible but unlikely we get this - for edge-edge intersection */
			ie = BM_edge_exists(UNPACK2(ie_vs));
			if (ie == NULL) {
				ie_exists = false;
				/* one of the verts must be new if we are making an edge
				 * ...no, we need this in case 2x quads intersect at either ends.
				 * if not (ie_vs[0].index == -1 or ie_vs[1].index == -1):
				 *     continue */
				ie = BM_edge_create(s->bm, UNPACK2(ie_vs), NULL, 0);
				BLI_gset_insert(s->wire_edges, ie);
			}
			else {
				ie_exists = true;
				/* may already exist */
				BLI_gset_add(s->wire_edges, ie);

				if (BM_edge_in_face(ie, f)) {
					continue;
				}
			}

			face_edges_add(s, BM_elem_index_get(f), ie, ie_exists);
			// BLI_assert(len(ie_vs) <= 2)
		}
	}
}

#ifdef USE_BVH


/* overlap or raycast */
#ifdef  USE_BOOLEAN_RAYCAST_OVERLAP
struct OverlapData {
	const float **looptris;

	const float *co;
	float dir[3];

	int num_isect;
};

static bool bmbvh_overlap_cb(void *userdata, int index_a, int UNUSED(index_b), int UNUSED(thread))
{
	struct OverlapData *raycast_data = userdata;
	const float **looptris = raycast_data->looptris;
	const float *v0 = looptris[index_a * 3 + 0];
	const float *v1 = looptris[index_a * 3 + 1];
	const float *v2 = looptris[index_a * 3 + 2];
	float dist;
	float uv[2];

	if (isect_ray_tri_watertight_v3_simple(raycast_data->co, raycast_data->dir, v0, v1, v2, &dist, uv)) {
		raycast_data->num_isect++;
		return true;
	}
	return false;
}

static int isect_bvhtree_point_v3(
        BVHTree *tree,
        const float **looptris,
        const float co[3])
{
	struct OverlapData raycast_data = {
		looptris,
	};


	float plane_cos[6];
	BVHTree *planetree;
	unsigned int tot = 0;
	BVHTreeOverlap *results;

	raycast_data.num_isect = 0;
	raycast_data.co = co;
	raycast_data.dir[0] = 1;
	raycast_data.dir[1] = 0;
	raycast_data.dir[2] = 0;


	planetree = BLI_bvhtree_new(2, 0, 8, 8);

	copy_v3_v3(plane_cos + 0, co);
	copy_v3_v3(plane_cos + 3, co);
	plane_cos[0] += 10;
	BLI_bvhtree_insert(planetree, 0, plane_cos, 2);
	BLI_bvhtree_balance(planetree);

	results = BLI_bvhtree_overlap(tree, planetree, &tot, bmbvh_overlap_cb, &raycast_data);
	BLI_bvhtree_free(planetree);
	if (results)
		MEM_freeN(results);
//	return (tot & 1) == 1;
//	return (raycast_data.num_isect & 1) == 1;
	return raycast_data.num_isect;
}

#else  // raycast

struct RaycastData {
	const float **looptris;
	BLI_Buffer z_buffer;
	float z_buffer_storage[64];
	int num_isect;
};

#define BLI_buffer_init_static(type_, flag_, static_storage_, static_count_) \
	{ \
	/* clear the static memory if this is a calloc'd array */ \
	((void)((flag_ & BLI_BUFFER_USE_CALLOC) ? \
	          memset(static_storage_, 0, sizeof(static_storage_)) : NULL \
	), /* memset-end */ \
	                    static_storage_), \
	                    sizeof(type_), \
	                    0, \
	                    static_count_, \
	                    BLI_BUFFER_USE_STATIC | flag_}

/* TODO(sergey): Make inline? */
BLI_INLINE bool raycast_has_depth(const struct RaycastData *raycast_data, float depth)
{
	size_t i;
#ifdef USE_DUMP
	printf("%s: Searching for depth %f\n", __func__, (double)depth);
#endif
	/* TODO(sergey): Replace with bisect. */
	for (i = 0; i < raycast_data->z_buffer.count; ++i) {
		float current_depth = BLI_buffer_at(&raycast_data->z_buffer,
		                                    float,
		                                    i);
#ifdef USE_DUMP
		printf("  Current depth %f\n", (double)depth);
#endif
		/* TODO(sergey): Perhaps after making BVH watertight we can get rid of
		 * the fabsf() alculation?  Or maybe at least it'll allow getting rid of
		 * such an obscure epsilon here.
		 */
		if (fabsf(current_depth - depth) < FLT_EPSILON * 10) {
#ifdef USE_DUMP
			printf("  Found depth %f\n", (double)depth);
#endif
			return true;
		}
	}
	return false;
}

/* TODO(sergey): Make inline? */
BLI_INLINE void raycast_append_depth(struct RaycastData *raycast_data, float depth)
{
#ifdef USE_DUMP
	printf("%s: Adding depth %f\n", __func__, (double)depth);
#endif
	/* TODO(sergey): Preserve z-buffer sorted state. */
	BLI_buffer_append(&raycast_data->z_buffer, float, depth);
}

#ifdef USE_KDOPBVH_WATERTIGHT
static const struct IsectRayPrecalc isect_precalc_x = {1, 2, 0, 0, 0, 1};
#endif

static void raycast_callback(void *userdata,
                             int index,
                             const BVHTreeRay *ray,
                             BVHTreeRayHit *hit)
{
	struct RaycastData *raycast_data = userdata;
	const float **looptris = raycast_data->looptris;
	const float *v0 = looptris[index * 3 + 0];
	const float *v1 = looptris[index * 3 + 1];
	const float *v2 = looptris[index * 3 + 2];
	float dist, uv[2];
	(void) hit;  /* Ignored. */
	if (
#ifdef USE_KDOPBVH_WATERTIGHT
	    isect_ray_tri_watertight_v3_simple(ray->origin, ray->direction, v0, v1, v2, &dist, uv))
//		isect_ray_tri_watertight_v3(ray->origin, &isect_precalc_x, v0, v1, v2, &dist, uv))
#else
	    isect_ray_tri_epsilon_v3(ray->origin, ray->direction, v0, v1, v2, &dist, uv, FLT_EPSILON))
#endif
	{
		if (dist >= 0.0f && !raycast_has_depth(raycast_data, dist)) {
#ifdef USE_DUMP
			printf("%s:\n", __func__);
			print_v3("  origin", ray->origin);
			print_v3("  direction", ray->direction);
			print_v2("  uv", uv);
			printf("  dist %f\n", dist);
			print_v3("  v0", v0);
			print_v3("  v1", v1);
			print_v3("  v2", v2);
#endif
			raycast_data->num_isect++;
			raycast_append_depth(raycast_data, dist);
		}
	}
}

static int isect_bvhtree_point_v3(
        BVHTree *tree,
        const float **looptris,
        const float co[3])
{
	struct RaycastData raycast_data = {
		looptris,
		BLI_buffer_init_static(float,
		                       0,
		                       raycast_data.z_buffer_storage,
		                       ARRAY_SIZE(raycast_data.z_buffer_storage)),
		{0},
		0
	};
	BVHTreeRayHit hit = {0};
	float dir[3] = {1.0f, 0.0f, 0.0f};

	// copy_v3_fl(dir, (float)M_SQRT1_3);

	/* Need to initialize hit even tho it's not used.
	 * This is to make it so kdotree believes we didn't intersect anything and
	 * keeps calling the intersect callback.
	 */
	hit.index = -1;
	hit.dist = FLT_MAX;

	BLI_bvhtree_ray_cast(tree,
	                     co, dir,
	                     0.0f,
	                     &hit,
	                     raycast_callback,
	                     &raycast_data);

#ifdef USE_DUMP
	printf("%s: Total intersections: %d\n", __func__, raycast_data.num_isect);
#endif

//	return (raycast_data.num_isect & 1) == 1;
	return raycast_data.num_isect;
}
#endif  // USE_BOOLEAN_RAYCAST_OVERLAP


#endif

/**
 * Intersect tessellated faces
 * leaving the resulting edges tagged.
 *
 * \param test_fn Return value: -1: skip, 0: tree_a, 1: tree_b (use_self == false)
 * \param boolean_mode 0: no-boolean, 1: intersection... etc.
 */
bool BM_mesh_intersect(
        BMesh *bm,
        struct BMLoop *(*looptris)[3], const int looptris_tot,
        int (*test_fn)(BMFace *f, void *user_data), void *user_data,
        const bool use_self, const bool use_separate, const bool use_dissolve,
        const int boolean_mode,
        const float eps)
{
	struct ISectState s;
	bool has_isect;
	const int totface_orig = bm->totface;

	/* needed for boolean, since cutting up faces moves the loops within the face */
	const float **looptri_coords = NULL;

#ifdef USE_BVH
	BVHTree *tree_a, *tree_b;
	unsigned int tree_overlap_tot;
	BVHTreeOverlap *overlap;
#else
	int i_a, i_b;
#endif

#ifdef USE_BOOLEAN_RAYCAST_DRAW
	bl_debug_draw_quad_clear();
#endif

	s.bm = bm;

	s.edgetri_cache = BLI_ghash_new(BLI_ghashutil_inthash_v4_p, BLI_ghashutil_inthash_v4_cmp, __func__);

	s.edge_verts = BLI_ghash_ptr_new(__func__);
	s.face_edges = BLI_ghash_int_new(__func__);
	s.wire_edges = BLI_gset_ptr_new(__func__);
	s.vert_dissolve = NULL;

	s.mem_arena = BLI_memarena_new(BLI_MEMARENA_STD_BUFSIZE, __func__);

	/* setup epsilon from base */
	s.epsilon.eps = eps;
	s.epsilon.eps2x = eps * 2.0f;
	s.epsilon.eps_margin = s.epsilon.eps2x * 10.0f;

	s.epsilon.eps_sq = s.epsilon.eps * s.epsilon.eps;
	s.epsilon.eps2x_sq = s.epsilon.eps2x * s.epsilon.eps2x;
	s.epsilon.eps_margin_sq = s.epsilon.eps_margin * s.epsilon.eps_margin;

	BM_mesh_elem_index_ensure(
	        bm,
	        BM_VERT |
	        BM_EDGE |
#ifdef USE_NET
	        BM_FACE |
#endif
	        0);


	BM_mesh_elem_table_ensure(
	        bm,
#ifdef USE_SPLICE
	        BM_EDGE |
#endif
#ifdef USE_NET
	        BM_FACE |
#endif
	        0);

#ifdef USE_DISSOLVE
	if (use_dissolve) {
		BM_mesh_elem_hflag_disable_all(bm, BM_EDGE | BM_VERT, BM_ELEM_TAG, false);
	}
#endif

#ifdef USE_DUMP
	printf("data = [\n");
#endif

	if (boolean_mode != BOOLEAN_NONE) {
		/* keep original geometrty for raycast callbacks */
		float **cos;
		int i, j;

		cos = MEM_mallocN((size_t)looptris_tot * sizeof(*looptri_coords) * 3, __func__);
		for (i = 0, j = 0; i < looptris_tot; i++) {
			cos[j++] = looptris[i][0]->v->co;
			cos[j++] = looptris[i][1]->v->co;
			cos[j++] = looptris[i][2]->v->co;
		}
		looptri_coords = (const float **)cos;
	}

#ifdef USE_BVH
	{
		int i;
		tree_a = BLI_bvhtree_new(looptris_tot, s.epsilon.eps_margin, 8, 8);
		for (i = 0; i < looptris_tot; i++) {
			if (test_fn(looptris[i][0]->f, user_data) == 0) {
				const float t_cos[3][3] = {
					{UNPACK3(looptris[i][0]->v->co)},
					{UNPACK3(looptris[i][1]->v->co)},
					{UNPACK3(looptris[i][2]->v->co)},
				};

				BLI_bvhtree_insert(tree_a, i, (const float *)t_cos, 3);
			}
		}
		BLI_bvhtree_balance(tree_a);
	}

	if (use_self == false) {
		int i;
		tree_b = BLI_bvhtree_new(looptris_tot, s.epsilon.eps_margin, 8, 8);
		for (i = 0; i < looptris_tot; i++) {
			if (test_fn(looptris[i][0]->f, user_data) == 1) {
				const float t_cos[3][3] = {
					{UNPACK3(looptris[i][0]->v->co)},
					{UNPACK3(looptris[i][1]->v->co)},
					{UNPACK3(looptris[i][2]->v->co)},
				};

				BLI_bvhtree_insert(tree_b, i, (const float *)t_cos, 3);
			}
		}
		BLI_bvhtree_balance(tree_b);
	}
	else {
		tree_b = tree_a;
	}

	overlap = BLI_bvhtree_overlap(tree_b, tree_a, &tree_overlap_tot, NULL, NULL);

	if (overlap) {
		unsigned int i;

		for (i = 0; i < tree_overlap_tot; i++) {
#ifdef USE_DUMP
			printf("  ((%d, %d), (\n",
			       overlap[i].indexA,
			       overlap[i].indexB);
#endif
			bm_isect_tri_tri(
			        &s,
			        overlap[i].indexA,
			        overlap[i].indexB,
			        looptris[overlap[i].indexA],
			        looptris[overlap[i].indexB]);
#ifdef USE_DUMP
			printf(")),\n");
#endif
		}
		MEM_freeN(overlap);
	}

	if (boolean_mode == BOOLEAN_NONE) {
		/* no booleans, just free immediate */
		BLI_bvhtree_free(tree_a);
		if (tree_a != tree_b) {
			BLI_bvhtree_free(tree_b);
		}
	}

#else
	{
		for (i_a = 0; i_a < looptris_tot; i_a++) {
			const int t_a = test_fn(looptris[i_a][0]->f, user_data);
			for (i_b = i_a + 1; i_b < looptris_tot; i_b++) {
				const int t_b = test_fn(looptris[i_b][0]->f, user_data);

				if (use_self) {
					if ((t_a != 0) || (t_b != 0)) {
						continue;
					}
				}
				else {
					if ((t_a != t_b) && !ELEM(-1, t_a, t_b)) {
						continue;
					}
				}

#ifdef USE_DUMP
				printf("  ((%d, %d), (",
				       i_a, i_b);
#endif
				bm_isect_tri_tri(
				        &s,
				        i_a,
				        i_b,
				        looptris[i_a],
				        looptris[i_b]);
#ifdef USE_DUMP
			printf(")),\n");
#endif
			}
		}
	}
#endif  /* USE_BVH */

#ifdef USE_DUMP
	printf("]\n");
#endif

	/* --------- */

#ifdef USE_SPLICE
	{
		GHashIterator gh_iter;

		GHASH_ITER (gh_iter, s.edge_verts) {
			BMEdge *e = BLI_ghashIterator_getKey(&gh_iter);
			struct LinkBase *v_ls_base = BLI_ghashIterator_getValue(&gh_iter);

			BMVert *v_start;
			BMVert *v_end;
			BMVert *v_prev;
			bool is_wire;

			LinkNode *node;

			/* direction is arbitrary, could be swapped */
			v_start = e->v1;
			v_end = e->v2;

			if (v_ls_base->list_len > 1) {
				edge_verts_sort(v_start->co, v_ls_base);
			}

#ifdef USE_DUMP
			printf("# SPLITTING EDGE: %d, %d\n", BM_elem_index_get(e), v_ls_base->list_len);
#endif
			/* intersect */
			is_wire = BLI_gset_haskey(s.wire_edges,  e);

#ifdef USE_PARANOID
			for (node = v_ls_base->list; node; node = node->next) {
				BMVert *_v = node->link;
				BLI_assert(len_squared_v3v3(_v->co, e->v1->co) > s.epsilon.eps_sq);
				BLI_assert(len_squared_v3v3(_v->co, e->v2->co) > s.epsilon.eps_sq);
			}
#endif

			v_prev = v_start;

			for (node = v_ls_base->list; node; node = node->next) {
				BMVert *vi = node->link;
				const float fac = line_point_factor_v3(vi->co, e->v1->co, e->v2->co);

				if (BM_vert_in_edge(e, v_prev)) {
					BMEdge *e_split;
					v_prev = BM_edge_split(bm, e, v_prev, &e_split, CLAMPIS(fac, 0.0f, 1.0f));
					BLI_assert(BM_vert_in_edge(e, v_end));

					if (!BM_edge_exists(v_prev, vi) &&
					    !BM_vert_splice_check_double(v_prev, vi) &&
					    !BM_vert_pair_share_face_check(v_prev, vi))
					{
						BM_vert_splice(bm, vi, v_prev);
					}
					else {
						copy_v3_v3(v_prev->co, vi->co);
					}
					v_prev = vi;
					if (is_wire) {
						BLI_gset_insert(s.wire_edges, e_split);
					}
				}
			}
			UNUSED_VARS_NDEBUG(v_end);
		}
	}
#endif


	/* important to handle before edgenet */
#ifdef USE_DISSOLVE
	if (use_dissolve && (boolean_mode == BOOLEAN_NONE)) {
		/* first pass */
		BMVert *(*splice_ls)[2];
		STACK_DECLARE(splice_ls);
		LinkNode *node;


		for (node = s.vert_dissolve; node; node = node->next) {
			BMVert *v = node->link;
			if (BM_elem_flag_test(v, BM_ELEM_TAG)) {
				if (!BM_vert_is_edge_pair(v)) {
					BM_elem_flag_disable(v, BM_ELEM_TAG);
				}
			}
		}

		splice_ls = MEM_mallocN(BLI_gset_size(s.wire_edges) * sizeof(*splice_ls), __func__);
		STACK_INIT(splice_ls, BLI_gset_size(s.wire_edges));

		for (node = s.vert_dissolve; node; node = node->next) {
			BMEdge *e_pair[2];
			BMVert *v = node->link;
			BMVert *v_a, *v_b;

			if (!BM_elem_flag_test(v, BM_ELEM_TAG)) {
				continue;
			}

			/* get chain */
			e_pair[0] = v->e;
			e_pair[1] = BM_DISK_EDGE_NEXT(v->e, v);

			if (BM_elem_flag_test(e_pair[0], BM_ELEM_TAG) ||
			    BM_elem_flag_test(e_pair[1], BM_ELEM_TAG))
			{
				continue;
			}

			v_a = BM_edge_other_vert(e_pair[0], v);
			v_b = BM_edge_other_vert(e_pair[1], v);

			/* simple case */
			if (BM_elem_flag_test(v_a, BM_ELEM_TAG) &&
			    BM_elem_flag_test(v_b, BM_ELEM_TAG))
			{
				/* only start on an edge-case */
				/* pass */
			}
			else if ((!BM_elem_flag_test(v_a, BM_ELEM_TAG)) &&
			         (!BM_elem_flag_test(v_b, BM_ELEM_TAG)))
			{
				/* simple case, single edge spans face */
				BMVert **splice_pair;
				BM_elem_flag_enable(e_pair[1], BM_ELEM_TAG);
				splice_pair = STACK_PUSH_RET(splice_ls);
				splice_pair[0] = v;
				splice_pair[1] = v_b;
#ifdef USE_DUMP
				printf("# Simple Case!\n");
#endif
			}
			else {
#ifdef USE_PARANOID
				BMEdge *e_keep;
#endif
				BMEdge *e;
				BMEdge *e_step;
				BMVert *v_step;

				/* walk the chain! */
				if (BM_elem_flag_test(v_a, BM_ELEM_TAG)) {
					e = e_pair[0];
#ifdef USE_PARANOID
					e_keep = e_pair[1];
#endif
				}
				else {
					SWAP(BMVert *, v_a, v_b);
					e = e_pair[1];
#ifdef USE_PARANOID
					e_keep = e_pair[0];
#endif
				}

				/* WALK */
				v_step = v;
				e_step = e;

				while (true) {
					BMEdge *e_next;
					BMVert *v_next;

					v_next = BM_edge_other_vert(e_step, v_step);
					BM_elem_flag_enable(e_step, BM_ELEM_TAG);
					if (!BM_elem_flag_test(v_next, BM_ELEM_TAG)) {
						BMVert **splice_pair;
#ifdef USE_PARANOID
						BLI_assert(e_step != e_keep);
#endif
						splice_pair = STACK_PUSH_RET(splice_ls);
						splice_pair[0] = v;
						splice_pair[1] = v_next;
						break;
					}
					else {
						e_next = bm_vert_other_edge(v_next, e_step);
					}

					e_step = e_next;
					v_step = v_next;
					BM_elem_flag_enable(e_step, BM_ELEM_TAG);
#ifdef USE_PARANOID
					BLI_assert(e_step != e_keep);
#endif
#ifdef USE_DUMP
					printf("# walk step %p %p\n", e_next, v_next);
#endif
				}
#ifdef USE_PARANOID
				BLI_assert(BM_elem_flag_test(e_keep, BM_ELEM_TAG) == 0);
#endif
			}
		}

		/* Remove edges! */
		{
			GHashIterator gh_iter;

			GHASH_ITER (gh_iter, s.face_edges) {
				struct LinkBase *e_ls_base = BLI_ghashIterator_getValue(&gh_iter);
				LinkNode **node_prev_p;
				unsigned int i;

				node_prev_p = &e_ls_base->list;
				for (i = 0, node = e_ls_base->list; node; i++, node = node->next) {
					BMEdge *e = node->link;
					if (BM_elem_flag_test(e, BM_ELEM_TAG)) {
						/* allocated by arena, don't free */
						*node_prev_p = node->next;
						e_ls_base->list_len--;
					}
					else {
						node_prev_p = &node->next;
					}
				}
			}
		}

		{
			BMIter eiter;
			BMEdge *e, *e_next;

			BM_ITER_MESH_MUTABLE (e, e_next, &eiter, bm, BM_EDGES_OF_MESH) {
				if (BM_elem_flag_test(e, BM_ELEM_TAG)) {

					/* in rare and annoying cases,
					 * there can be faces from 's.face_edges' removed by the edges.
					 * These are degenerate cases, so just make sure we don't reference the faces again. */
					if (e->l) {
						BMLoop *l_iter = e->l;
						BMFace **faces;

						faces = bm->ftable;

						do {
							const int f_index = BM_elem_index_get(l_iter->f);
							if (f_index >= 0) {
								BLI_assert(f_index < totface_orig);
								/* we could check if these are in: 's.face_edges', but easier just to remove */
								faces[f_index] = NULL;
							}
						} while ((l_iter = l_iter->radial_next) != e->l);
					}

					BLI_gset_remove(s.wire_edges, e, NULL);
					BM_edge_kill(bm, e);
				}
			}
		}

		/* Remove verts! */
		{
			GSet *verts_invalid = BLI_gset_ptr_new(__func__);

			for (node = s.vert_dissolve; node; node = node->next) {
				/* arena allocated, don't free */
				BMVert *v = node->link;
				if (BM_elem_flag_test(v, BM_ELEM_TAG)) {
					if (!v->e) {
						BLI_gset_add(verts_invalid, v);
						BM_vert_kill(bm, v);
					}
				}
			}

			{
				unsigned int i;
				for (i = 0; i < STACK_SIZE(splice_ls); i++) {
					if (!BLI_gset_haskey(verts_invalid, splice_ls[i][0]) &&
					    !BLI_gset_haskey(verts_invalid, splice_ls[i][1]))
					{
						if (!BM_edge_exists(UNPACK2(splice_ls[i])) &&
						    !BM_vert_splice_check_double(UNPACK2(splice_ls[i])))
						{
							BM_vert_splice(bm, splice_ls[i][1], splice_ls[i][0]);
						}
					}
				}
			}

			BLI_gset_free(verts_invalid, NULL);
		}

		MEM_freeN(splice_ls);
	}
#endif  /* USE_DISSOLVE */


	/* now split faces */
#ifdef USE_NET
	{
		GHashIterator gh_iter;
		BMFace **faces;

		faces = bm->ftable;

		GHASH_ITER (gh_iter, s.face_edges) {
			const int f_index = GET_INT_FROM_POINTER(BLI_ghashIterator_getKey(&gh_iter));
			BMFace *f;
			struct LinkBase *e_ls_base = BLI_ghashIterator_getValue(&gh_iter);

			BLI_assert(f_index >= 0 && f_index < totface_orig);

			f = faces[f_index];
			if (UNLIKELY(f == NULL)) {
				continue;
			}

			BLI_assert(BM_elem_index_get(f) == f_index);

			face_edges_split(bm, f, e_ls_base);
		}
	}
#endif  /* USE_NET */
	(void)totface_orig;

#ifdef USE_SEPARATE
	if (use_separate) {
		GSetIterator gs_iter;

		BM_mesh_elem_hflag_disable_all(bm, BM_EDGE, BM_ELEM_TAG, false);

		GSET_ITER (gs_iter, s.wire_edges) {
			BMEdge *e = BLI_gsetIterator_getKey(&gs_iter);
			BM_elem_flag_enable(e, BM_ELEM_TAG);
		}

		BM_mesh_edgesplit(bm, false, true, false);
	}
	else if (boolean_mode != BOOLEAN_NONE) {
		GSetIterator gs_iter;

		/* no need to clear for boolean */

		GSET_ITER (gs_iter, s.wire_edges) {
			BMEdge *e = BLI_gsetIterator_getKey(&gs_iter);
			BM_elem_flag_enable(e, BM_ELEM_TAG);
		}
	}
#else
	(void)use_separate;
#endif  /* USE_SEPARATE */

	if ((boolean_mode != BOOLEAN_NONE)) {
			BVHTree *tree_pair[2] = {tree_a, tree_b};

		/* group vars */
		int *groups_array;
		int (*group_index)[2];
		int group_tot;
		int i;
		BMFace **ftable;

		BM_mesh_elem_table_ensure(bm, BM_FACE);
		ftable = bm->ftable;

		groups_array = MEM_mallocN(sizeof(*groups_array) * (size_t)bm->totface, __func__);
		group_tot = BM_mesh_calc_face_groups(
		        bm, groups_array, &group_index,
		        bm_loop_filter_fn, NULL,
		        0, BM_EDGE);

#ifdef USE_DUMP
		printf("%s: Total face-groups: %d\n", __func__, group_tot);
#endif

		/* first check if island is inside */

		/* TODO, find if islands are inside/outside,
		 * for now remove alternate islands, as simple testcase */

		printf("Found %d\n", group_tot);
		for (i = 0; i < group_tot; i++) {
			int fg     = group_index[i][0];
			int fg_end = group_index[i][1] + fg;
			bool do_remove, do_flip;

			{
				/* for now assyme this is an OK face to test with (not degenerate!) */
				BMFace *f = ftable[groups_array[fg]];
				float co[3];
				int hits;
				int side = test_fn(f, user_data) == 0;

				// BM_face_calc_center_mean(f, co);
				BM_face_calc_point_in_face(f, co);

				hits = isect_bvhtree_point_v3(tree_pair[side], looptri_coords, co);

				switch (boolean_mode) {
					case BOOLEAN_ISECT:
						do_remove = ((hits & 1) != 1);
						do_flip = false;
						break;
					case BOOLEAN_UNION:
						do_remove = ((hits & 1) == 1);
						do_flip = false;
						break;
					case BOOLEAN_DIFFERENCE:
						do_remove = ((hits & 1) == 1) == side;
						do_flip = (side == 0);
						break;
				}

#ifdef USE_BOOLEAN_RAYCAST_DRAW
				{
					unsigned int colors[4] = {0x00000000, 0xffffffff, 0xff000000, 0x0000ff};
					float co_other[3] = {UNPACK3(co)};
					co_other[0] += 1000.0f;
					bl_debug_color_set(colors[(hits & 1) == 1]);
					bl_debug_draw_edge_add(co, co_other);
				}
#endif

			}

			if (do_remove) {
				for (; fg != fg_end; fg++) {
//					BM_face_kill_loose(bm, ftable[groups_array[fg]]);
					ftable[groups_array[fg]]->mat_nr = -1;
				}
			}
			else if (do_flip) {
				for (; fg != fg_end; fg++) {
					BM_face_normal_flip(bm, ftable[groups_array[fg]]);
				}
			}
		}

		MEM_freeN(groups_array);
		MEM_freeN(group_index);

#ifdef USE_DISSOLVE
		/* We have dissolve code above, this is alternative logic,
		 * we need to do it after the boolean is executed. */
		if (use_dissolve) {
			LinkNode *node;
			for (node = s.vert_dissolve; node; node = node->next) {
				BMVert *v = node->link;
				if (BM_vert_is_edge_pair(v)) {
					/* we wont create degenerate faces from this */
					bool ok = true;

					/* would we create a 2-sided-face?
					 * if so, don't dissolve this since we may */
					if (v->e->l) {
						BMLoop *l_iter = v->e->l;
						do {
							if (l_iter->f->len == 3) {
								ok = false;
								break;
							}
						} while ((l_iter = l_iter->radial_next) != v->e->l);
					}

					if (ok) {
						BM_vert_collapse_edge(bm, v->e, v, true, false);
					}
				}
			}
		}
#endif

		{
			int tot = bm->totface;
			for (i = 0; i < tot; i++) {
				if (ftable[i]->mat_nr == -1) {
					BM_face_kill_loose(bm, ftable[i]);
				}
			}
		}
	}

	if (boolean_mode != BOOLEAN_NONE) {
		MEM_freeN(looptri_coords);

		/* no booleans, just free immediate */
		BLI_bvhtree_free(tree_a);
		if (tree_a != tree_b) {
			BLI_bvhtree_free(tree_b);
		}
	}

	has_isect = (BLI_ghash_size(s.face_edges) != 0);

	/* cleanup */
	BLI_ghash_free(s.edgetri_cache, NULL, NULL);

	BLI_ghash_free(s.edge_verts, NULL, NULL);
	BLI_ghash_free(s.face_edges, NULL, NULL);
	BLI_gset_free(s.wire_edges, NULL);

	BLI_memarena_free(s.mem_arena);

	return has_isect;
}
