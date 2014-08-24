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
 * The Original Code is Copyright (C) 2001-2002 by NaN Holding BV.
 * All rights reserved.
 *
 * The Original Code is: all of this file.
 *
 * Contributor(s): none yet.
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/blenkernel/intern/curve.c
 *  \ingroup bke
 */

extern "C" {
#include <math.h>  // floor
#include <string.h>
#include <stdlib.h>

#include "MEM_guardedalloc.h"

#include "BLI_blenlib.h"
#include "BLI_math.h"
#include "BLI_utildefines.h"
#include "BLI_ghash.h"
#include "BLI_polyfill2d.h"
#include "BLI_threads.h"

#include "DNA_curve_types.h"
#include "DNA_material_types.h"

/* for dereferencing pointers */
#include "DNA_key_types.h"
#include "DNA_scene_types.h"
#include "DNA_vfont_types.h"
#include "DNA_object_types.h"
#include "DNA_mesh_types.h"
#include "DNA_meshdata_types.h"

#include "BKE_animsys.h"
#include "BKE_curve.h"
#include "BKE_depsgraph.h"
#include "BKE_mesh.h"
#include "BKE_displist.h"
#include "BKE_font.h"
#include "BKE_global.h"
#include "BKE_key.h"
#include "BKE_library.h"
#include "BKE_main.h"
#include "BKE_object.h"
#include "BKE_material.h"
#include "BKE_customdata.h"
}

#include "surf_gridmesh.h"
#include <map>

/* globals */

/* local */
static int cu_isectLL(const float v1[3], const float v2[3], const float v3[3], const float v4[3],
                      short cox, short coy,
                      float *lambda, float *mu, float vec[3]);

void BKE_curve_unlink(Curve *cu)
{
	int a;

	for (a = 0; a < cu->totcol; a++) {
		if (cu->mat[a]) cu->mat[a]->id.us--;
		cu->mat[a] = NULL;
	}
	if (cu->vfont)
		cu->vfont->id.us--;
	cu->vfont = NULL;

	if (cu->vfontb)
		cu->vfontb->id.us--;
	cu->vfontb = NULL;

	if (cu->vfonti)
		cu->vfonti->id.us--;
	cu->vfonti = NULL;

	if (cu->vfontbi)
		cu->vfontbi->id.us--;
	cu->vfontbi = NULL;

	if (cu->key)
		cu->key->id.us--;
	cu->key = NULL;
}

/* frees editcurve entirely */
void BKE_curve_editfont_free(Curve *cu)
{
	if (cu->editfont) {
		EditFont *ef = cu->editfont;

		if (ef->textbuf)
			MEM_freeN(ef->textbuf);
		if (ef->textbufinfo)
			MEM_freeN(ef->textbufinfo);
		if (ef->copybuf)
			MEM_freeN(ef->copybuf);
		if (ef->copybufinfo)
			MEM_freeN(ef->copybufinfo);
		if (ef->selboxes)
			MEM_freeN(ef->selboxes);

		MEM_freeN(ef);
		cu->editfont = NULL;
	}
}

void BKE_curve_editNurb_keyIndex_free(EditNurb *editnurb)
{
	if (!editnurb->keyindex) {
		return;
	}
	BLI_ghash_free(editnurb->keyindex, NULL, MEM_freeN);
	editnurb->keyindex = NULL;
}

void BKE_curve_editNurb_free(Curve *cu)
{
	if (cu->editnurb) {
		BKE_nurbList_free(&cu->editnurb->nurbs);
		BKE_curve_editNurb_keyIndex_free(cu->editnurb);
		MEM_freeN(cu->editnurb);
		cu->editnurb = NULL;
	}
}

void BKE_nurb_knot_calc_u(Nurb *nu)
{
	int pnts=nu->pntsu, order=nu->orderu;
	BKE_nurbs_editKnot_destroy(nu);
	float *knots = (float*)MEM_mallocN(sizeof(float)*(pnts+2*order), "NURB_knots_u");
	BKE_bspline_knot_calc(nu->flagu, pnts, order, knots);
	if (nu->knotsu) MEM_freeN(nu->knotsu);
	nu->knotsu = knots;
}

void BKE_nurb_knot_calc_v(Nurb *nu)
{
	int pnts=nu->pntsv, order=nu->orderv;
	BKE_nurbs_editKnot_destroy(nu);
	float *knots = (float*)MEM_mallocN(sizeof(float)*(pnts+2*order), "NURB_knots_v");
	BKE_bspline_knot_calc(nu->flagv, pnts, order, knots);
	if (nu->knotsv) MEM_freeN(nu->knotsv);
	nu->knotsv = knots;
}

/* don't free curve itself */
void BKE_curve_free(Curve *cu)
{
	BKE_nurbList_free(&cu->nurb);
	BKE_curve_editfont_free(cu);

	BKE_curve_editNurb_free(cu);
	BKE_curve_unlink(cu);
	BKE_free_animdata((ID *)cu);

	if (cu->mat)
		MEM_freeN(cu->mat);
	if (cu->str)
		MEM_freeN(cu->str);
	if (cu->strinfo)
		MEM_freeN(cu->strinfo);
	if (cu->bb)
		MEM_freeN(cu->bb);
	if (cu->tb)
		MEM_freeN(cu->tb);
}

Curve *BKE_curve_add(Main *bmain, const char *name, int type)
{
	Curve *cu;

	cu = (Curve*)BKE_libblock_alloc(bmain, ID_CU, name);
	copy_v3_fl(cu->size, 1.0f);
	cu->flag = CU_FRONT | CU_BACK | CU_DEFORM_BOUNDS_OFF | CU_PATH_RADIUS;
	cu->pathlen = 100;
	cu->resolu = cu->resolv = (type == OB_SURF) ? 8 : 12;
	cu->width = 1.0;
	cu->wordspace = 1.0;
	cu->spacing = cu->linedist = 1.0;
	cu->fsize = 1.0;
	cu->ulheight = 0.05;
	cu->texflag = CU_AUTOSPACE;
	cu->smallcaps_scale = 0.75f;
	/* XXX: this one seems to be the best one in most cases, at least for curve deform... */
	cu->twist_mode = CU_TWIST_MINIMUM;
	cu->type = type;
	cu->bevfac1 = 0.0f;
	cu->bevfac2 = 1.0f;
	cu->bevfac1_mapping = CU_BEVFAC_MAP_RESOLU;
	cu->bevfac2_mapping = CU_BEVFAC_MAP_RESOLU;

	cu->bb = BKE_boundbox_alloc_unit();

	if (type == OB_FONT) {
		cu->vfont = cu->vfontb = cu->vfonti = cu->vfontbi = BKE_vfont_builtin_get();
		cu->vfont->id.us += 4;
		cu->str = (char*)MEM_mallocN(12, "str");
		BLI_strncpy(cu->str, "Text", 12);
		cu->len = cu->len_wchar = cu->pos = 4;
		cu->strinfo = (struct CharInfo*)MEM_callocN(12 * sizeof(CharInfo), "strinfo new");
		cu->totbox = cu->actbox = 1;
		cu->tb = (struct TextBox*)MEM_callocN(MAXTEXTBOX * sizeof(TextBox), "textbox");
		cu->tb[0].w = cu->tb[0].h = 0.0;
	}

	return cu;
}

Curve *BKE_curve_copy(Curve *cu)
{
	Curve *cun;
	int a;

	cun = (Curve*)BKE_libblock_copy(&cu->id);
	BLI_listbase_clear(&cun->nurb);
	BKE_nurbList_duplicate(&(cun->nurb), &(cu->nurb));

	cun->mat = (struct Material **)MEM_dupallocN(cu->mat);
	for (a = 0; a < cun->totcol; a++) {
		id_us_plus((ID *)cun->mat[a]);
	}

	cun->str = (char*)MEM_dupallocN(cu->str);
	cun->strinfo = (struct CharInfo*)MEM_dupallocN(cu->strinfo);
	cun->tb = (struct TextBox*)MEM_dupallocN(cu->tb);
	cun->bb = (struct BoundBox*)MEM_dupallocN(cu->bb);

	cun->key = BKE_key_copy(cu->key);
	if (cun->key) cun->key->from = (ID *)cun;

	cun->editnurb = NULL;
	cun->editfont = NULL;

#if 0   // XXX old animation system
	/* single user ipo too */
	if (cun->ipo) cun->ipo = copy_ipo(cun->ipo);
#endif // XXX old animation system

	id_us_plus((ID *)cun->vfont);
	id_us_plus((ID *)cun->vfontb);
	id_us_plus((ID *)cun->vfonti);
	id_us_plus((ID *)cun->vfontbi);

	return cun;
}

static void extern_local_curve(Curve *cu)
{
	id_lib_extern((ID *)cu->vfont);
	id_lib_extern((ID *)cu->vfontb);
	id_lib_extern((ID *)cu->vfonti);
	id_lib_extern((ID *)cu->vfontbi);

	if (cu->mat) {
		extern_local_matarar(cu->mat, cu->totcol);
	}
}

void BKE_curve_make_local(Curve *cu)
{
	Main *bmain = G.main;
	Object *ob;
	bool is_local = false, is_lib = false;

	/* - when there are only lib users: don't do
	 * - when there are only local users: set flag
	 * - mixed: do a copy
	 */

	if (cu->id.lib == NULL)
		return;

	if (cu->id.us == 1) {
		id_clear_lib_data(bmain, &cu->id);
		extern_local_curve(cu);
		return;
	}

	for (ob = (Object*)bmain->object.first; ob && ELEM(0, is_lib, is_local); ob = (Object*)ob->id.next) {
		if (ob->data == cu) {
			if (ob->id.lib) is_lib = true;
			else is_local = true;
		}
	}

	if (is_local && is_lib == false) {
		id_clear_lib_data(bmain, &cu->id);
		extern_local_curve(cu);
	}
	else if (is_local && is_lib) {
		Curve *cu_new = BKE_curve_copy(cu);
		cu_new->id.us = 0;

		BKE_id_lib_local_paths(bmain, cu->id.lib, &cu_new->id);

		for (ob = (Object*)bmain->object.first; ob; ob = (Object*)ob->id.next) {
			if (ob->data == cu) {
				if (ob->id.lib == NULL) {
					ob->data = cu_new;
					cu_new->id.us++;
					cu->id.us--;
				}
			}
		}
	}
}

/* Get list of nurbs from editnurbs structure */
ListBase *BKE_curve_editNurbs_get(Curve *cu)
{
	if (cu->editnurb) {
		return &cu->editnurb->nurbs;
	}

	return NULL;
}

short BKE_curve_type_get(Curve *cu)
{
	Nurb *nu;
	int type = cu->type;

	if (cu->vfont) {
		return OB_FONT;
	}

	if (!cu->type) {
		type = OB_CURVE;

		for (nu = (Nurb*)cu->nurb.first; nu; nu = (Nurb*)nu->next) {
			if (nu->pntsv > 1) {
				type = OB_SURF;
			}
		}
	}

	return type;
}

void BKE_curve_curve_dimension_update(Curve *cu)
{
	ListBase *nurbs = BKE_curve_nurbs_get(cu);
	Nurb *nu = (Nurb*)nurbs->first;

	if (cu->flag & CU_3D) {
		for (; nu; nu = nu->next) {
			nu->flag &= ~CU_2D;
		}
	}
	else {
		for (; nu; nu = nu->next) {
			nu->flag |= CU_2D;
			BKE_nurb_ensure2D(nu);

			/* since the handles are moved they need to be auto-located again */
			if (nu->type == CU_BEZIER)
				BKE_nurb_handles_calc(nu);
		}
	}
}

void BKE_curve_type_test(Object *ob)
{
	ob->type = BKE_curve_type_get((Curve*)ob->data);

	if (ob->type == OB_CURVE)
		BKE_curve_curve_dimension_update((Curve *)ob->data);
}

void BKE_curve_boundbox_calc(Curve *cu, float r_loc[3], float r_size[3])
{
	BoundBox *bb;
	float min[3], max[3];
	float mloc[3], msize[3];

	if (cu->bb == NULL) cu->bb = (struct BoundBox*)MEM_callocN(sizeof(BoundBox), "boundbox");
	bb = cu->bb;

	if (!r_loc) r_loc = mloc;
	if (!r_size) r_size = msize;

	INIT_MINMAX(min, max);
	if (!BKE_curve_minmax(cu, true, min, max)) {
		min[0] = min[1] = min[2] = -1.0f;
		max[0] = max[1] = max[2] = 1.0f;
	}

	mid_v3_v3v3(r_loc, min, max);

	r_size[0] = (max[0] - min[0]) / 2.0f;
	r_size[1] = (max[1] - min[1]) / 2.0f;
	r_size[2] = (max[2] - min[2]) / 2.0f;

	BKE_boundbox_init_from_minmax(bb, min, max);

	bb->flag &= ~BOUNDBOX_DIRTY;
}

BoundBox *BKE_curve_boundbox_get(Object *ob)
{
	Curve *cu = (Curve*)ob->data;

	if (ob->bb)
		return ob->bb;

	if (cu->bb == NULL || (cu->bb->flag & BOUNDBOX_DIRTY)) {
		BKE_curve_texspace_calc(cu);
	}

	return cu->bb;
}

void BKE_curve_texspace_calc(Curve *cu)
{
	float loc[3], size[3];
	int a;

	BKE_curve_boundbox_calc(cu, loc, size);

	if (cu->texflag & CU_AUTOSPACE) {
		for (a = 0; a < 3; a++) {
			if (size[a] == 0.0f) size[a] = 1.0f;
			else if (size[a] > 0.0f && size[a] < 0.00001f) size[a] = 0.00001f;
			else if (size[a] < 0.0f && size[a] > -0.00001f) size[a] = -0.00001f;
		}

		copy_v3_v3(cu->loc, loc);
		copy_v3_v3(cu->size, size);
		zero_v3(cu->rot);
	}
}

void BKE_curve_texspace_get(Curve *cu, float r_loc[3], float r_rot[3], float r_size[3])
{
	if (cu->bb == NULL || (cu->bb->flag & BOUNDBOX_DIRTY)) {
		BKE_curve_texspace_calc(cu);
	}

	if (r_loc) copy_v3_v3(r_loc,  cu->loc);
	if (r_rot) copy_v3_v3(r_rot,  cu->rot);
	if (r_size) copy_v3_v3(r_size, cu->size);
}

struct NurbEditKnot* BKE_nurbs_editKnot_get(struct Nurb *nu) {
	if (nu->editknot) return nu->editknot;
	BKE_nurbs_editKnot_propagate_nurb2ek(nu);
	return nu->editknot;
}

void BKE_nurbs_editKnot_propagate_ek2nurb(struct Nurb *nu) {
	NurbEditKnot *ek = nu->editknot;
	if (!ek) return;
	int ek_knotu=0, ek_knotv=0;

	for (int i=0; i<ek->num_breaksu; i++) ek_knotu += ek->breaksu[i].multiplicity;
	BLI_assert(KNOTSU(nu)==ek_knotu);
	for (int i=0,flatidx=0; i<ek->num_breaksu; i++) {
		int mult = ek->breaksu[i].multiplicity;
		float breakpt = ek->breaksu[i].loc;
		for (int j=0; j<mult; j++)
			nu->knotsu[flatidx++] = breakpt;
	}

	if (ek->num_breaksv) {
		for (int i=0; i<ek->num_breaksv; i++) ek_knotv += ek->breaksv[i].multiplicity;
		BLI_assert(KNOTSV(nu)==ek_knotv);
		for (int i=0,flatidx=0; i<ek->num_breaksv; i++) {
			int mult = ek->breaksv[i].multiplicity;
			float breakpt = ek->breaksv[i].loc;
			for (int j=0; j<mult; j++)
				nu->knotsv[flatidx++] = breakpt;
		}
	}
}

/* Maintains flags for knots at a given location across knot update
 * Also clears NurbTrim SELECT flag whenever it allocates a new editknot (which
 * happens on editmode enter) */
void BKE_nurbs_editKnot_propagate_nurb2ek(struct Nurb *nu) {
	NurbEditKnot *ek = nu->editknot;
	NurbEditKnot old_ek;
	NurbTrim *nt;
	bool update_ek;
	if (ek) {
		old_ek = *ek;
		update_ek = true;
	} else {
		ek = nu->editknot = (NurbEditKnot*)MEM_callocN(sizeof(*ek),"NURBS_editknot_prop");
		for (nt=(NurbTrim*)nu->trims.first; nt; nt=nt->next) {
			nt->flag &= ~SELECT;
		}
		update_ek = false;
	}

	int capu = ek->capu = KNOTSU(nu);
	ek->breaksu = (NurbBreakpt*)MEM_callocN(capu*sizeof(NurbBreakpt), "NURBS_editknot_u");
	float last_knot=INFINITY;
	int old_idx=0, breakidx=-1;
	for (int i=0; i<KNOTSU(nu); i++) {
		float knot = nu->knotsu[i];
		if (knot!=last_knot) {
			breakidx += 1;
			last_knot = knot;
		}
		if (update_ek) { /* try to propagate SELECT etc from old ek to new ek */
			while (old_ek.breaksu[old_idx].loc<knot && old_idx<old_ek.num_breaksu)
				old_idx++;
			if (old_ek.breaksu[old_idx].loc==knot)
				ek->breaksu[breakidx].flag = old_ek.breaksu[old_idx].flag;
		}
		ek->breaksu[breakidx].loc = knot;
		ek->breaksu[breakidx].multiplicity += 1;
	}
	ek->num_breaksu = breakidx+1;

	if (nu->pntsv==1) {
		ek->num_breaksv = 0; /* This is a curve. There are no breakpoints in v direction. */
		ek->breaksv = NULL;
	} else {
		int capv = ek->capv = KNOTSV(nu);
		ek->breaksv = (NurbBreakpt*)MEM_callocN(capv*sizeof(NurbBreakpt), "NURBS_editknot_v");
		last_knot=INFINITY;
		old_idx=0; breakidx=-1;
		for (int i=0; i<KNOTSV(nu); i++) {
			float knot = nu->knotsv[i];
			if (knot!=last_knot) {
				breakidx += 1;
				last_knot = knot;
			}
			if (update_ek) { /* try to propagate SELECT etc from old ek to new ek */
				while (old_ek.breaksv[old_idx].loc<knot && old_idx<old_ek.num_breaksv)
					old_idx++;
				if (old_ek.breaksv[old_idx].loc==knot)
					ek->breaksv[breakidx].flag = old_ek.breaksv[old_idx].flag;
			}
			ek->breaksv[breakidx].loc = knot;
			ek->breaksv[breakidx].multiplicity += 1;
		}
		ek->num_breaksv = breakidx+1;
	}

	if (update_ek) {
		MEM_freeN(old_ek.breaksu);
		MEM_freeN(old_ek.breaksv);
	}
	
//	printf("U: Propagated {");
//	for (int i=0; i<KNOTSU(nu); i++) printf("%3.1f ",nu->knotsu[i]);
//	printf("} -> {");
//	for (int i=0; i<ek->num_breaksu; i++) printf("%3.1fx%i ",ek->breaksu[i].loc,ek->breaksu[i].multiplicity);
//	printf("}\n");
//	printf("V: Propagated {");
//	for (int i=0; i<KNOTSV(nu); i++) printf("%3.1f ",nu->knotsv[i]);
//	printf("} -> {");
//	for (int i=0; i<ek->num_breaksv; i++) printf("%3.1fx%i ",ek->breaksv[i].loc,ek->breaksv[i].multiplicity);
//	printf("}\n");
}

void BKE_nurbs_editKnot_destroy(struct Nurb *nu) {
	NurbEditKnot* ek = nu->editknot;
	if (!ek) return;
	if (ek->breaksu) MEM_freeN(ek->breaksu);
	if (ek->breaksv) MEM_freeN(ek->breaksv);
	MEM_freeN(nu->editknot);
	nu->editknot = NULL;
}

bool BKE_nurbList_index_get_co(ListBase *nurb, const int index, float r_co[3])
{
	Nurb *nu;
	int tot = 0;

	for (nu = (Nurb*)nurb->first; nu; nu = nu->next) {
		int tot_nu;
		if (nu->type == CU_BEZIER) {
			tot_nu = nu->pntsu;
			if (index - tot < tot_nu) {
				copy_v3_v3(r_co, nu->bezt[index - tot].vec[1]);
				return true;
			}
		}
		else {
			tot_nu = nu->pntsu * nu->pntsv;
			if (index - tot < tot_nu) {
				copy_v3_v3(r_co, nu->bp[index - tot].vec);
				return true;
			}
		}
		tot += tot_nu;
	}

	return false;
}

int BKE_nurbList_verts_count(ListBase *nurb)
{
	Nurb *nu;
	int tot = 0;

	nu = (Nurb*)nurb->first;
	while (nu) {
		if (nu->bezt)
			tot += 3 * nu->pntsu;
		else if (nu->bp)
			tot += nu->pntsu * nu->pntsv;

		nu = nu->next;
	}
	return tot;
}

int BKE_nurbList_verts_count_without_handles(ListBase *nurb)
{
	Nurb *nu;
	int tot = 0;

	nu = (Nurb*)nurb->first;
	while (nu) {
		if (nu->bezt)
			tot += nu->pntsu;
		else if (nu->bp)
			tot += nu->pntsu * nu->pntsv;

		nu = nu->next;
	}
	return tot;
}

/* **************** NURBS ROUTINES ******************** */

void BKE_nurb_free(Nurb *nu)
{
	if (nu == NULL) return;

	if (nu->bezt)
		MEM_freeN(nu->bezt);
	nu->bezt = NULL;
	if (nu->bp)
		MEM_freeN(nu->bp);
	nu->bp = NULL;
	if (nu->knotsu)
		MEM_freeN(nu->knotsu);
	nu->knotsu = NULL;
	if (nu->knotsv)
		MEM_freeN(nu->knotsv);
	nu->knotsv = NULL;
	
	for (NurbTrim *nt = (NurbTrim*)nu->trims.first; nt;) {
		NurbTrim *nt_to_free = nt;
		nt = nt->next;
		BKE_nurbTrim_free(nt_to_free);
	}
	
	if (nu->UV_idxs) MEM_freeN(nu->UV_idxs);
	if (nu->UV_verts) MEM_freeN(nu->UV_verts);
	if (nu->editknot) BKE_nurbs_editKnot_destroy(nu);

	MEM_freeN(nu);
}

void BKE_nurbTrim_free(NurbTrim *nt) {
	BKE_nurbList_free(&nt->nurb_list);
	MEM_freeN(nt);
}

void BKE_nurbList_free(ListBase *lb)
{
	Nurb *nu, *next;

	if (lb == NULL) return;

	nu = (Nurb*)lb->first;
	while (nu) {
		next = nu->next;
		BKE_nurb_free(nu);
		nu = next;
	}
	BLI_listbase_clear(lb);
}

Nurb *BKE_nurb_duplicate(Nurb *nu)
{
	Nurb *newnu;
	int len;

	newnu = (Nurb *)MEM_mallocN(sizeof(Nurb), "duplicateNurb");
	if (newnu == NULL) return NULL;
	memcpy(newnu, nu, sizeof(Nurb));

	if (nu->bezt) {
		newnu->bezt =
		    (BezTriple *)MEM_mallocN((nu->pntsu) * sizeof(BezTriple), "duplicateNurb2");
		memcpy(newnu->bezt, nu->bezt, nu->pntsu * sizeof(BezTriple));
	}
	else {
		len = nu->pntsu * nu->pntsv;
		newnu->bp =
		    (BPoint *)MEM_mallocN((len) * sizeof(BPoint), "duplicateNurb3");
		memcpy(newnu->bp, nu->bp, len * sizeof(BPoint));

		newnu->knotsu = newnu->knotsv = NULL;

		if (nu->knotsu) {
			len = KNOTSU(nu);
			if (len) {
				newnu->knotsu = (float*)MEM_mallocN(len * sizeof(float), "duplicateNurb4");
				memcpy(newnu->knotsu, nu->knotsu, sizeof(float) * len);
			}
		}
		if (nu->pntsv > 1 && nu->knotsv) {
			len = KNOTSV(nu);
			if (len) {
				newnu->knotsv = (float*)MEM_mallocN(len * sizeof(float), "duplicateNurb5");
				memcpy(newnu->knotsv, nu->knotsv, sizeof(float) * len);
			}
		}
	}
	
	newnu->trims.first = newnu->trims.last = NULL;
	for (NurbTrim *nt = (NurbTrim*)nu->trims.first; nt; nt=nt->next) {
		NurbTrim *dup_nt = BKE_nurbTrim_duplicate(nt);
		dup_nt->parent_nurb = newnu;
		BLI_addtail(&newnu->trims, dup_nt);
	}
	BKE_nurbs_cached_UV_mesh_clear(newnu,false);
	newnu->editknot = NULL;

	return newnu;
}

/* copy the nurb but allow for different number of points (to be copied after this) */
Nurb *BKE_nurb_copy(Nurb *src, int pntsu, int pntsv)
{
	Nurb *newnu = (Nurb *)MEM_mallocN(sizeof(Nurb), "copyNurb");
	memcpy(newnu, src, sizeof(Nurb));

	if (pntsu == 1) SWAP(int, pntsu, pntsv);
	newnu->pntsu = pntsu;
	newnu->pntsv = pntsv;

	if (src->bezt) {
		newnu->bezt = (BezTriple *)MEM_mallocN(pntsu * pntsv * sizeof(BezTriple), "copyNurb2");
	}
	else {
		newnu->bp = (BPoint *)MEM_mallocN(pntsu * pntsv * sizeof(BPoint), "copyNurb3");
	}
	
	newnu->trims.first = newnu->trims.last = NULL;
	for (NurbTrim *nt = (NurbTrim*)src->trims.first; nt; nt=nt->next) {
		NurbTrim *dup_nt = BKE_nurbTrim_duplicate(nt);
		BLI_addtail(&newnu->trims, dup_nt);
	}
	BKE_nurbs_cached_UV_mesh_clear(newnu,false);
	newnu->editknot = NULL;

	return newnu;
}

NurbTrim *BKE_nurbTrim_duplicate(NurbTrim *nt) {
	NurbTrim *ret = (NurbTrim*)MEM_callocN(sizeof(NurbTrim), "duplicateNurbTrim");
	BKE_nurbList_duplicate(&ret->nurb_list, &nt->nurb_list);
	ret->type = nt->type;
	ret->parent_nurb = nt->parent_nurb;
	return ret;
}

int BKE_nurbTrim_tess(struct NurbTrim *nt, float (**uv_out)[2]) {
	int tot_tess_pts = 0;
	for (Nurb* nu = (Nurb*)nt->nurb_list.first; nu; nu=nu->next) {
		int tess_pts = nu->pntsu * nu->resolu + 1;
		if (nu->flagu&CU_NURB_ENDPOINT) tess_pts = (nu->pntsu+2-nu->orderu)*nu->resolu;
		tot_tess_pts += tess_pts;
	}
	float (*uv)[2] = (float(*)[2])MEM_mallocN(sizeof(*uv)*tot_tess_pts, "BKE_nurbTrim_tess");
	*uv_out = uv;
	for (Nurb* nu = (Nurb*)nt->nurb_list.first; nu; nu=nu->next) {
		int tess_pts = nu->pntsu * nu->resolu + 1;
		if (nu->flagu&CU_NURB_ENDPOINT) tess_pts = (nu->pntsu+2-nu->orderu)*nu->resolu;
		float *U = nu->knotsu;
		int pntsu = nu->pntsu;
		BPoint *bp = nu->bp;
		int orderu = nu->orderu;
		float umin, umax;
		BKE_nurbs_domain(nu, &umin, &umax, NULL, NULL);
		float du = (umax-umin)/(tess_pts-1);
		for (int i=0; i<tess_pts; i++) {
			BPoint pt;
			float u = (i!=tess_pts-1)? umin+i*du : umax;
			BKE_nurbs_curve_eval(u, U, pntsu, orderu, bp, 1, 0, &pt);
			uv[i][0] = pt.vec[0];
			uv[i][1] = pt.vec[1];
		}
		uv += tess_pts;
	}
	return tot_tess_pts;
}

void BKE_nurbTrim_update_data(struct NurbTrim *nt) {
	BKE_nurbs_cached_UV_mesh_clear((Nurb*)nt->parent_nurb, true);
}

void BKE_nurbList_duplicate(ListBase *lb1, ListBase *lb2)
{
	Nurb *nu, *nun;

	BKE_nurbList_free(lb1);

	nu = (Nurb*)lb2->first;
	while (nu) {
		nun = BKE_nurb_duplicate(nu);
		BLI_addtail(lb1, nun);

		nu = nu->next;
	}
}

void BKE_nurb_ensure2D(Nurb *nu)
{
	BezTriple *bezt;
	BPoint *bp;
	int a;

	if ((nu->flag & CU_2D) == 0)
		return;

	if (nu->type == CU_BEZIER) {
		a = nu->pntsu;
		bezt = nu->bezt;
		while (a--) {
			bezt->vec[0][2] = 0.0;
			bezt->vec[1][2] = 0.0;
			bezt->vec[2][2] = 0.0;
			bezt++;
		}
	}
	else {
		a = nu->pntsu * nu->pntsv;
		bp = nu->bp;
		while (a--) {
			bp->vec[2] = 0.0;
			bp++;
		}
	}
}

/**
 * if use_radius is truth, minmax will take points' radius into account,
 * which will make boundbox closer to beveled curve.
 */
void BKE_nurb_minmax(Nurb *nu, bool use_radius, float min[3], float max[3])
{
	BezTriple *bezt;
	BPoint *bp;
	int a;
	float point[3];

	if (nu->type == CU_BEZIER) {
		a = nu->pntsu;
		bezt = nu->bezt;
		while (a--) {
			if (use_radius) {
				float radius_vector[3];
				radius_vector[0] = radius_vector[1] = radius_vector[2] = bezt->radius;

				add_v3_v3v3(point, bezt->vec[1], radius_vector);
				minmax_v3v3_v3(min, max, point);

				sub_v3_v3v3(point, bezt->vec[1], radius_vector);
				minmax_v3v3_v3(min, max, point);
			}
			else {
				minmax_v3v3_v3(min, max, bezt->vec[1]);
			}
			minmax_v3v3_v3(min, max, bezt->vec[0]);
			minmax_v3v3_v3(min, max, bezt->vec[2]);
			bezt++;
		}
	}
	else {
		a = nu->pntsu * nu->pntsv;
		bp = nu->bp;
		while (a--) {
			if (nu->pntsv == 1 && use_radius) {
				float radius_vector[3];
				radius_vector[0] = radius_vector[1] = radius_vector[2] = bp->radius;

				add_v3_v3v3(point, bp->vec, radius_vector);
				minmax_v3v3_v3(min, max, point);

				sub_v3_v3v3(point, bp->vec, radius_vector);
				minmax_v3v3_v3(min, max, point);
			}
			else {
				/* Surfaces doesn't use bevel, so no need to take radius into account. */
				minmax_v3v3_v3(min, max, bp->vec);
			}
			bp++;
		}
	}
}

/* be sure to call makeknots after this */
void BKE_nurb_points_add(Nurb *nu, int number)
{
	BPoint *bp;
	int i;

	nu->bp = (BPoint*)MEM_recallocN(nu->bp, (nu->pntsu + number) * sizeof(BPoint));

	for (i = 0, bp = &nu->bp[nu->pntsu]; i < number; i++, bp++) {
		bp->radius = 1.0f;
	}

	nu->pntsu += number;
	BKE_nurbs_cached_UV_mesh_clear(nu, true);
}

void BKE_nurb_bezierPoints_add(Nurb *nu, int number)
{
	BezTriple *bezt;
	int i;

	nu->bezt = (BezTriple*)MEM_recallocN(nu->bezt, (nu->pntsu + number) * sizeof(BezTriple));

	for (i = 0, bezt = &nu->bezt[nu->pntsu]; i < number; i++, bezt++) {
		bezt->radius = 1.0f;
	}

	nu->pntsu += number;
	BKE_nurbs_cached_UV_mesh_clear(nu, true);
}


BezTriple *BKE_nurb_bezt_get_next(Nurb *nu, BezTriple *bezt)
{
	BezTriple *bezt_next;

	BLI_assert(ARRAY_HAS_ITEM(bezt, nu->bezt, nu->pntsu));

	if (bezt == &nu->bezt[nu->pntsu - 1]) {
		if (nu->flagu & CU_NURB_CYCLIC) {
			bezt_next = nu->bezt;
		}
		else {
			bezt_next = NULL;
		}
	}
	else {
		bezt_next = bezt + 1;
	}

	return bezt_next;
}

BPoint *BKE_nurb_bpoint_get_next(Nurb *nu, BPoint *bp)
{
	BPoint *bp_next;

	BLI_assert(ARRAY_HAS_ITEM(bp, nu->bp, nu->pntsu));

	if (bp == &nu->bp[nu->pntsu - 1]) {
		if (nu->flagu & CU_NURB_CYCLIC) {
			bp_next = nu->bp;
		}
		else {
			bp_next = NULL;
		}
	}
	else {
		bp_next = bp + 1;
	}

	return bp_next;
}

BezTriple *BKE_nurb_bezt_get_prev(Nurb *nu, BezTriple *bezt)
{
	BezTriple *bezt_prev;

	BLI_assert(ARRAY_HAS_ITEM(bezt, nu->bezt, nu->pntsu));

	if (bezt == nu->bezt) {
		if (nu->flagu & CU_NURB_CYCLIC) {
			bezt_prev = &nu->bezt[nu->pntsu - 1];
		}
		else {
			bezt_prev = NULL;
		}
	}
	else {
		bezt_prev = bezt - 1;
	}

	return bezt_prev;
}

BPoint *BKE_nurb_bpoint_get_prev(Nurb *nu, BPoint *bp)
{
	BPoint *bp_prev;

	BLI_assert(ARRAY_HAS_ITEM(bp, nu->bp, nu->pntsu));

	if (bp == nu->bp) {
		if (nu->flagu & CU_NURB_CYCLIC) {
			bp_prev = &nu->bp[nu->pntsu - 1];
		}
		else {
			bp_prev = NULL;
		}
	}
	else {
		bp_prev = bp - 1;
	}

	return bp_prev;
}

void BKE_nurb_bezt_calc_normal(struct Nurb *UNUSED(nu), struct BezTriple *bezt, float r_normal[3])
{
	/* calculate the axis matrix from the spline */
	float dir_prev[3], dir_next[3];

	sub_v3_v3v3(dir_prev, bezt->vec[0], bezt->vec[1]);
	sub_v3_v3v3(dir_next, bezt->vec[1], bezt->vec[2]);

	normalize_v3(dir_prev);
	normalize_v3(dir_next);

	add_v3_v3v3(r_normal, dir_prev, dir_next);
	normalize_v3(r_normal);
}

void BKE_nurb_bezt_calc_plane(struct Nurb *nu, struct BezTriple *bezt, float r_plane[3])
{
	float dir_prev[3], dir_next[3];

	sub_v3_v3v3(dir_prev, bezt->vec[0], bezt->vec[1]);
	sub_v3_v3v3(dir_next, bezt->vec[1], bezt->vec[2]);

	normalize_v3(dir_prev);
	normalize_v3(dir_next);

	cross_v3_v3v3(r_plane, dir_prev, dir_next);
	if (normalize_v3(r_plane) < FLT_EPSILON) {
		BezTriple *bezt_prev = BKE_nurb_bezt_get_prev(nu, bezt);
		BezTriple *bezt_next = BKE_nurb_bezt_get_next(nu, bezt);

		if (bezt_prev) {
			sub_v3_v3v3(dir_prev, bezt_prev->vec[1], bezt->vec[1]);
			normalize_v3(dir_prev);
		}
		if (bezt_next) {
			sub_v3_v3v3(dir_next, bezt->vec[1], bezt_next->vec[1]);
			normalize_v3(dir_next);
		}
		cross_v3_v3v3(r_plane, dir_prev, dir_next);
	}

	/* matches with bones more closely */
	{
		float dir_mid[3], tvec[3];
		add_v3_v3v3(dir_mid, dir_prev, dir_next);
		cross_v3_v3v3(tvec, r_plane, dir_mid);
		copy_v3_v3(r_plane, tvec);
	}

	normalize_v3(r_plane);
}

/* ~~~~~~~~~~~~~~~~~~~~Non Uniform Rational B Spline calculations ~~~~~~~~~~~ */
/* Fills <basis> with <pnts> weights corresponding to the curve evaluated
 * at point t. <start> is the index of the first nz basis function at t, <end>
 * is the index of the last nz basis function at t.
 */
static void basisNurb(float t, short order, int pnts, float *knots, float *basis, int *start, int *end)
{
	float d, e;
	int i, i1 = 0, i2 = 0, j, orderpluspnts, opp2, o2;

	orderpluspnts = order + pnts;
	opp2 = orderpluspnts - 1;

	/* this is for float inaccuracy */
	if (t < knots[0])
		t = knots[0];
	else if (t > knots[opp2]) 
		t = knots[opp2];

	/* this part is order '1' */
	o2 = order + 1;
	for (i = 0; i < opp2; i++) {
		if (knots[i] != knots[i + 1] && t >= knots[i] && t <= knots[i + 1]) {
			basis[i] = 1.0;
			i1 = i - o2;
			if (i1 < 0) i1 = 0;
			i2 = i;
			i++;
			while (i < opp2) {
				basis[i] = 0.0;
				i++;
			}
			break;
		}
		else
			basis[i] = 0.0;
	}
	basis[i] = 0.0;

	/* this is order 2, 3, ... */
	for (j = 2; j <= order; j++) {

		if (i2 + j >= orderpluspnts) i2 = opp2 - j;

		for (i = i1; i <= i2; i++) {
			if (basis[i] != 0.0f)
				d = ((t - knots[i]) * basis[i]) / (knots[i + j - 1] - knots[i]);
			else
				d = 0.0f;

			if (basis[i + 1] != 0.0f)
				e = ((knots[i + j] - t) * basis[i + 1]) / (knots[i + j] - knots[i + 1]);
			else
				e = 0.0;

			basis[i] = d + e;
		}
	}

	*start = 1000;
	*end = 0;

	for (i = i1; i <= i2; i++) {
		if (basis[i] > 0.0f) {
			*end = i;
			if (*start == 1000) *start = i;
		}
	}
}

/**
 * \param coord_array Has to be 3 * 4 * pntsu * resolu in size and zero-ed
 * \param tilt_array   set when non-NULL
 * \param radius_array set when non-NULL
 */
void BKE_nurb_makeCurve(Nurb *nu, float *coord_array, float *tilt_array, float *radius_array, float *weight_array,
                        int resolu, int stride)
{
	BPoint *bp;
	float u, ustart, uend, ustep, sumdiv;
	float *basisu, *sum, *fp;
	float *coord_fp = coord_array, *tilt_fp = tilt_array, *radius_fp = radius_array, *weight_fp = weight_array;
	int i, len, istart, iend, cycl;

	if (nu->knotsu == NULL)
		return;
	if (nu->orderu > nu->pntsu)
		return;
	if (coord_array == NULL)
		return;

	/* allocate and initialize */
	len = nu->pntsu;
	if (len == 0)
		return;
	sum = (float *)MEM_callocN(sizeof(float) * len, "makeNurbcurve1");

	resolu = (resolu * SEGMENTSU(nu));

	if (resolu == 0) {
		MEM_freeN(sum);
		return;
	}

	fp = nu->knotsu;
	ustart = fp[nu->orderu - 1];
	if (nu->flagu & CU_NURB_CYCLIC)
		uend = fp[nu->pntsu + nu->orderu - 1];
	else
		uend = fp[nu->pntsu];
	ustep = (uend - ustart) / (resolu - ((nu->flagu & CU_NURB_CYCLIC) ? 0 : 1));

	basisu = (float *)MEM_mallocN(sizeof(float) * KNOTSU(nu), "makeNurbcurve3");

	if (nu->flagu & CU_NURB_CYCLIC)
		cycl = nu->orderu - 1;
	else
		cycl = 0;

	u = ustart;
	while (resolu--) {
		basisNurb(u, nu->orderu, nu->pntsu + cycl, nu->knotsu, basisu, &istart, &iend);

		/* calc sum */
		sumdiv = 0.0;
		fp = sum;
		bp = nu->bp + istart - 1;
		for (i = istart; i <= iend; i++, fp++) {
			if (i >= nu->pntsu)
				bp = nu->bp + (i - nu->pntsu);
			else
				bp++;

			*fp = basisu[i] * bp->vec[3];
			sumdiv += *fp;
		}
		if ((sumdiv != 0.0f) && (sumdiv < 0.999f || sumdiv > 1.001f)) {
			/* is normalizing needed? */
			fp = sum;
			for (i = istart; i <= iend; i++, fp++) {
				*fp /= sumdiv;
			}
		}

		zero_v3(coord_fp);

		/* one! (1.0) real point */
		fp = sum;
		bp = nu->bp + istart - 1;
		for (i = istart; i <= iend; i++, fp++) {
			if (i >= nu->pntsu)
				bp = nu->bp + (i - nu->pntsu);
			else
				bp++;

			if (*fp != 0.0f) {
				madd_v3_v3fl(coord_fp, bp->vec, *fp);

				if (tilt_fp)
					(*tilt_fp) += (*fp) * bp->alfa;

				if (radius_fp)
					(*radius_fp) += (*fp) * bp->radius;

				if (weight_fp)
					(*weight_fp) += (*fp) * bp->weight;
			}
		}

		coord_fp = (float *)(((char *)coord_fp) + stride);

		if (tilt_fp)
			tilt_fp = (float *)(((char *)tilt_fp) + stride);
		if (radius_fp)
			radius_fp = (float *)(((char *)radius_fp) + stride);
		if (weight_fp)
			weight_fp = (float *)(((char *)weight_fp) + stride);

		u += ustep;
	}

	/* free */
	MEM_freeN(sum);
	MEM_freeN(basisu);
}

/* forward differencing method for bezier curve */
void BKE_curve_forward_diff_bezier(float q0, float q1, float q2, float q3, float *p, int it, int stride)
{
	float rt0, rt1, rt2, rt3, f;
	int a;

	f = (float)it;
	rt0 = q0;
	rt1 = 3.0f * (q1 - q0) / f;
	f *= f;
	rt2 = 3.0f * (q0 - 2.0f * q1 + q2) / f;
	f *= it;
	rt3 = (q3 - q0 + 3.0f * (q1 - q2)) / f;

	q0 = rt0;
	q1 = rt1 + rt2 + rt3;
	q2 = 2 * rt2 + 6 * rt3;
	q3 = 6 * rt3;

	for (a = 0; a <= it; a++) {
		*p = q0;
		p = (float *)(((char *)p) + stride);
		q0 += q1;
		q1 += q2;
		q2 += q3;
	}
}

static void forward_diff_bezier_cotangent(const float p0[3], const float p1[3], const float p2[3], const float p3[3],
                                          float p[3], int it, int stride)
{
	/* note that these are not perpendicular to the curve
	 * they need to be rotated for this,
	 *
	 * This could also be optimized like BKE_curve_forward_diff_bezier */
	int a;
	for (a = 0; a <= it; a++) {
		float t = (float)a / (float)it;

		int i;
		for (i = 0; i < 3; i++) {
			p[i] = (-6.0f  * t +  6.0f) * p0[i] +
			       ( 18.0f * t - 12.0f) * p1[i] +
			       (-18.0f * t +  6.0f) * p2[i] +
			       ( 6.0f  * t)         * p3[i];
		}
		normalize_v3(p);
		p = (float *)(((char *)p) + stride);
	}
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

float *BKE_curve_surf_make_orco(Object *ob)
{
	/* Note: this function is used in convertblender only atm, so
	 * suppose nonzero curve's render resolution should always be used */
	Curve *cu = (Curve*)ob->data;
	Nurb *nu;
	int a, b, tot = 0;
	int sizeu, sizev;
	int resolu, resolv;
	float *fp, *coord_array;

	/* first calculate the size of the datablock */
	nu = (Nurb*)cu->nurb.first;
	while (nu) {
		/* as we want to avoid the seam in a cyclic nurbs
		 * texture wrapping, reserve extra orco data space to save these extra needed
		 * vertex based UV coordinates for the meridian vertices.
		 * Vertices on the 0/2pi boundary are not duplicated inside the displist but later in
		 * the renderface/vert construction.
		 *
		 * See also convertblender.c: init_render_surf()
		 */

		resolu = cu->resolu_ren ? cu->resolu_ren : nu->resolu;
		resolv = cu->resolv_ren ? cu->resolv_ren : nu->resolv;

		sizeu = nu->pntsu * resolu;
		sizev = nu->pntsv * resolv;
		if (nu->flagu & CU_NURB_CYCLIC) sizeu++;
		if (nu->flagv & CU_NURB_CYCLIC) sizev++;
		if (nu->pntsv > 1) tot += sizeu * sizev;

		nu = nu->next;
	}
	/* makeNurbfaces wants zeros */
	fp = coord_array = (float*)MEM_callocN(3 * sizeof(float) * tot, "make_orco");

	nu = (Nurb*)cu->nurb.first;
	while (nu) {
		resolu = cu->resolu_ren ? cu->resolu_ren : nu->resolu;
		resolv = cu->resolv_ren ? cu->resolv_ren : nu->resolv;

		if (nu->pntsv > 1) {
			sizeu = nu->pntsu * resolu;
			sizev = nu->pntsv * resolv;

			if (nu->flagu & CU_NURB_CYCLIC)
				sizeu++;
			if (nu->flagv & CU_NURB_CYCLIC)
				sizev++;

			if (cu->flag & CU_UV_ORCO) {
				for (b = 0; b < sizeu; b++) {
					for (a = 0; a < sizev; a++) {

						if (sizev < 2)
							fp[0] = 0.0f;
						else
							fp[0] = -1.0f + 2.0f * ((float)a) / (sizev - 1);

						if (sizeu < 2)
							fp[1] = 0.0f;
						else
							fp[1] = -1.0f + 2.0f * ((float)b) / (sizeu - 1);

						fp[2] = 0.0;

						fp += 3;
					}
				}
			}
			else {
				/*
				int size = (nu->pntsu * resolu) * (nu->pntsv * resolv) * 3 * sizeof(float);
				float *_tdata = (float*)MEM_mallocN(size, "temp data");
				float *tdata = _tdata;

				BKE_nurb_makeFaces(nu, tdata, 0, resolu, resolv);

				for (b = 0; b < sizeu; b++) {
					int use_b = b;
					if (b == sizeu - 1 && (nu->flagu & CU_NURB_CYCLIC))
						use_b = false;

					for (a = 0; a < sizev; a++) {
						int use_a = a;
						if (a == sizev - 1 && (nu->flagv & CU_NURB_CYCLIC))
							use_a = false;

						tdata = _tdata + 3 * (use_b * (nu->pntsv * resolv) + use_a);

						fp[0] = (tdata[0] - cu->loc[0]) / cu->size[0];
						fp[1] = (tdata[1] - cu->loc[1]) / cu->size[1];
						fp[2] = (tdata[2] - cu->loc[2]) / cu->size[2];
						fp += 3;
					}
				}

				MEM_freeN(_tdata);
				*/
			}
		}
		nu = nu->next;
	}

	return coord_array;
}


/* NOTE: This routine is tied to the order of vertex
 * built by displist and as passed to the renderer.
 */
float *BKE_curve_make_orco(Scene *scene, Object *ob, int *r_numVerts)
{
	Curve *cu = (Curve*)ob->data;
	DispList *dl;
	int u, v, numVerts;
	float *fp, *coord_array;
	ListBase disp = {NULL, NULL};

	BKE_displist_make_curveTypes_forOrco(scene, ob, &disp);

	numVerts = 0;
	for (dl = (DispList*)disp.first; dl; dl = dl->next) {
		if (dl->type == DL_INDEX3) {
			numVerts += dl->nr;
		}
		else if (dl->type == DL_SURF) {
			/* convertblender.c uses the Surface code for creating renderfaces when cyclic U only
			 * (closed circle beveling)
			 */
			if (dl->flag & DL_CYCL_U) {
				if (dl->flag & DL_CYCL_V)
					numVerts += (dl->parts + 1) * (dl->nr + 1);
				else
					numVerts += dl->parts * (dl->nr + 1);
			}
			else if (dl->flag & DL_CYCL_V) {
				numVerts += (dl->parts + 1) * dl->nr;
			}
			else
				numVerts += dl->parts * dl->nr;
		}
	}

	if (r_numVerts)
		*r_numVerts = numVerts;

	fp = coord_array = (float*)MEM_mallocN(3 * sizeof(float) * numVerts, "cu_orco");
	for (dl = (DispList*)disp.first; dl; dl = dl->next) {
		if (dl->type == DL_INDEX3) {
			for (u = 0; u < dl->nr; u++, fp += 3) {
				if (cu->flag & CU_UV_ORCO) {
					fp[0] = 2.0f * u / (dl->nr - 1) - 1.0f;
					fp[1] = 0.0;
					fp[2] = 0.0;
				}
				else {
					copy_v3_v3(fp, &dl->verts[u * 3]);

					fp[0] = (fp[0] - cu->loc[0]) / cu->size[0];
					fp[1] = (fp[1] - cu->loc[1]) / cu->size[1];
					fp[2] = (fp[2] - cu->loc[2]) / cu->size[2];
				}
			}
		}
		else if (dl->type == DL_SURF) {
			int sizeu = dl->nr, sizev = dl->parts;

			/* exception as handled in convertblender.c too */
			if (dl->flag & DL_CYCL_U) {
				sizeu++;
				if (dl->flag & DL_CYCL_V)
					sizev++;
			}
			else  if (dl->flag & DL_CYCL_V) {
				sizev++;
			}

			for (u = 0; u < sizev; u++) {
				for (v = 0; v < sizeu; v++, fp += 3) {
					if (cu->flag & CU_UV_ORCO) {
						fp[0] = 2.0f * u / (sizev - 1) - 1.0f;
						fp[1] = 2.0f * v / (sizeu - 1) - 1.0f;
						fp[2] = 0.0;
					}
					else {
						const float *vert;
						int realv = v % dl->nr;
						int realu = u % dl->parts;

						vert = dl->verts + 3 * (dl->nr * realu + realv);
						copy_v3_v3(fp, vert);

						fp[0] = (fp[0] - cu->loc[0]) / cu->size[0];
						fp[1] = (fp[1] - cu->loc[1]) / cu->size[1];
						fp[2] = (fp[2] - cu->loc[2]) / cu->size[2];
					}
				}
			}
		}
	}

	BKE_displist_free(&disp);

	return coord_array;
}


/* ***************** BEVEL ****************** */

void BKE_curve_bevel_make(Scene *scene, Object *ob, ListBase *disp,
                          const bool for_render, const bool use_render_resolution)
{
	DispList *dl, *dlnew;
	Curve *bevcu, *cu;
	float *fp, facx, facy, angle, dangle;
	int nr, a;

	cu = (Curve*)ob->data;
	BLI_listbase_clear(disp);

	/* if a font object is being edited, then do nothing */
// XXX	if ( ob == obedit && ob->type == OB_FONT ) return;

	if (cu->bevobj) {
		if (cu->bevobj->type != OB_CURVE)
			return;

		bevcu = (Curve*)cu->bevobj->data;
		if (bevcu->ext1 == 0.0f && bevcu->ext2 == 0.0f) {
			ListBase bevdisp = {NULL, NULL};
			facx = cu->bevobj->size[0];
			facy = cu->bevobj->size[1];

			if (for_render) {
				BKE_displist_make_curveTypes_forRender(scene, cu->bevobj, &bevdisp, NULL, false, use_render_resolution);
				dl = (DispList*)bevdisp.first;
			}
			else if (cu->bevobj->curve_cache) {
				dl = (DispList*)cu->bevobj->curve_cache->disp.first;
			}
			else {
				BLI_assert(cu->bevobj->curve_cache != NULL);
				dl = NULL;
			}

			while (dl) {
				if (ELEM(dl->type, DL_POLY, DL_SEGM)) {
					dlnew = (DispList*)MEM_mallocN(sizeof(DispList), "makebevelcurve1");
					*dlnew = *dl;
					dlnew->verts = (float*)MEM_mallocN(3 * sizeof(float) * dl->parts * dl->nr, "makebevelcurve1");
					memcpy(dlnew->verts, dl->verts, 3 * sizeof(float) * dl->parts * dl->nr);

					if (dlnew->type == DL_SEGM)
						dlnew->flag |= (DL_FRONT_CURVE | DL_BACK_CURVE);

					BLI_addtail(disp, dlnew);
					fp = dlnew->verts;
					nr = dlnew->parts * dlnew->nr;
					while (nr--) {
						fp[2] = fp[1] * facy;
						fp[1] = -fp[0] * facx;
						fp[0] = 0.0;
						fp += 3;
					}
				}
				dl = dl->next;
			}

			BKE_displist_free(&bevdisp);
		}
	}
	else if (cu->ext1 == 0.0f && cu->ext2 == 0.0f) {
		/* pass */
	}
	else if (cu->ext2 == 0.0f) {
		dl = (DispList*)MEM_callocN(sizeof(DispList), "makebevelcurve2");
		dl->verts = (float*)MEM_mallocN(2 * sizeof(float[3]), "makebevelcurve2");
		BLI_addtail(disp, dl);
		dl->type = DL_SEGM;
		dl->parts = 1;
		dl->flag = DL_FRONT_CURVE | DL_BACK_CURVE;
		dl->nr = 2;

		fp = dl->verts;
		fp[0] = fp[1] = 0.0;
		fp[2] = -cu->ext1;
		fp[3] = fp[4] = 0.0;
		fp[5] = cu->ext1;
	}
	else if ( (cu->flag & (CU_FRONT | CU_BACK)) == 0 && cu->ext1 == 0.0f) { // we make a full round bevel in that case
		nr = 4 + 2 * cu->bevresol;

		dl = (DispList*)MEM_callocN(sizeof(DispList), "makebevelcurve p1");
		dl->verts = (float*)MEM_mallocN(nr * sizeof(float[3]), "makebevelcurve p1");
		BLI_addtail(disp, dl);
		dl->type = DL_POLY;
		dl->parts = 1;
		dl->flag = DL_BACK_CURVE;
		dl->nr = nr;

		/* a circle */
		fp = dl->verts;
		dangle = (2.0f * (float)M_PI / (nr));
		angle = -(nr - 1) * dangle;

		for (a = 0; a < nr; a++) {
			fp[0] = 0.0;
			fp[1] = (cosf(angle) * (cu->ext2));
			fp[2] = (sinf(angle) * (cu->ext2)) - cu->ext1;
			angle += dangle;
			fp += 3;
		}
	}
	else {
		short dnr;

		/* bevel now in three parts, for proper vertex normals */
		/* part 1, back */

		if ((cu->flag & CU_BACK) || !(cu->flag & CU_FRONT)) {
			dnr = nr = 2 + cu->bevresol;
			if ( (cu->flag & (CU_FRONT | CU_BACK)) == 0)
				nr = 3 + 2 * cu->bevresol;

			dl = (DispList*)MEM_callocN(sizeof(DispList), "makebevelcurve p1");
			dl->verts = (float*)MEM_mallocN(nr * sizeof(float[3]), "makebevelcurve p1");
			BLI_addtail(disp, dl);
			dl->type = DL_SEGM;
			dl->parts = 1;
			dl->flag = DL_BACK_CURVE;
			dl->nr = nr;

			/* half a circle */
			fp = dl->verts;
			dangle = (0.5 * M_PI / (dnr - 1));
			angle = -(nr - 1) * dangle;

			for (a = 0; a < nr; a++) {
				fp[0] = 0.0;
				fp[1] = (float)(cosf(angle) * (cu->ext2));
				fp[2] = (float)(sinf(angle) * (cu->ext2)) - cu->ext1;
				angle += dangle;
				fp += 3;
			}
		}

		/* part 2, sidefaces */
		if (cu->ext1 != 0.0f) {
			nr = 2;

			dl = (DispList*)MEM_callocN(sizeof(DispList), "makebevelcurve p2");
			dl->verts = (float*)MEM_mallocN(nr * sizeof(float[3]), "makebevelcurve p2");
			BLI_addtail(disp, dl);
			dl->type = DL_SEGM;
			dl->parts = 1;
			dl->nr = nr;

			fp = dl->verts;
			fp[1] = cu->ext2;
			fp[2] = -cu->ext1;
			fp[4] = cu->ext2;
			fp[5] = cu->ext1;

			if ( (cu->flag & (CU_FRONT | CU_BACK)) == 0) {
				dl = (DispList*)MEM_dupallocN(dl);
				dl->verts = (float*)MEM_dupallocN(dl->verts);
				BLI_addtail(disp, dl);

				fp = dl->verts;
				fp[1] = -fp[1];
				fp[2] = -fp[2];
				fp[4] = -fp[4];
				fp[5] = -fp[5];
			}
		}

		/* part 3, front */
		if ((cu->flag & CU_FRONT) || !(cu->flag & CU_BACK)) {
			dnr = nr = 2 + cu->bevresol;
			if ( (cu->flag & (CU_FRONT | CU_BACK)) == 0)
				nr = 3 + 2 * cu->bevresol;

			dl = (DispList*)MEM_callocN(sizeof(DispList), "makebevelcurve p3");
			dl->verts = (float*)MEM_mallocN(nr * sizeof(float[3]), "makebevelcurve p3");
			BLI_addtail(disp, dl);
			dl->type = DL_SEGM;
			dl->flag = DL_FRONT_CURVE;
			dl->parts = 1;
			dl->nr = nr;

			/* half a circle */
			fp = dl->verts;
			angle = 0.0;
			dangle = (0.5 * M_PI / (dnr - 1));

			for (a = 0; a < nr; a++) {
				fp[0] = 0.0;
				fp[1] = (float)(cosf(angle) * (cu->ext2));
				fp[2] = (float)(sinf(angle) * (cu->ext2)) + cu->ext1;
				angle += dangle;
				fp += 3;
			}
		}
	}
}

static int cu_isectLL(const float v1[3], const float v2[3], const float v3[3], const float v4[3],
                      short cox, short coy,
                      float *lambda, float *mu, float vec[3])
{
	/* return:
	 * -1: collinear
	 *  0: no intersection of segments
	 *  1: exact intersection of segments
	 *  2: cross-intersection of segments
	 */
	float deler;

	deler = (v1[cox] - v2[cox]) * (v3[coy] - v4[coy]) - (v3[cox] - v4[cox]) * (v1[coy] - v2[coy]);
	if (deler == 0.0f)
		return -1;

	*lambda = (v1[coy] - v3[coy]) * (v3[cox] - v4[cox]) - (v1[cox] - v3[cox]) * (v3[coy] - v4[coy]);
	*lambda = -(*lambda / deler);

	deler = v3[coy] - v4[coy];
	if (deler == 0) {
		deler = v3[cox] - v4[cox];
		*mu = -(*lambda * (v2[cox] - v1[cox]) + v1[cox] - v3[cox]) / deler;
	}
	else {
		*mu = -(*lambda * (v2[coy] - v1[coy]) + v1[coy] - v3[coy]) / deler;
	}
	vec[cox] = *lambda * (v2[cox] - v1[cox]) + v1[cox];
	vec[coy] = *lambda * (v2[coy] - v1[coy]) + v1[coy];

	if (*lambda >= 0.0f && *lambda <= 1.0f && *mu >= 0.0f && *mu <= 1.0f) {
		if (*lambda == 0.0f || *lambda == 1.0f || *mu == 0.0f || *mu == 1.0f)
			return 1;
		return 2;
	}
	return 0;
}


static bool bevelinside(BevList *bl1, BevList *bl2)
{
	/* is bl2 INSIDE bl1 ? with left-right method and "lambda's" */
	/* returns '1' if correct hole  */
	BevPoint *bevp, *prevbevp;
	float min, max, vec[3], hvec1[3], hvec2[3], lab, mu;
	int nr, links = 0, rechts = 0, mode;

	/* take first vertex of possible hole */

	bevp = bl2->bevpoints;
	hvec1[0] = bevp->vec[0];
	hvec1[1] = bevp->vec[1];
	hvec1[2] = 0.0;
	copy_v3_v3(hvec2, hvec1);
	hvec2[0] += 1000;

	/* test it with all edges of potential surounding poly */
	/* count number of transitions left-right  */

	bevp = bl1->bevpoints;
	nr = bl1->nr;
	prevbevp = bevp + (nr - 1);

	while (nr--) {
		min = prevbevp->vec[1];
		max = bevp->vec[1];
		if (max < min) {
			min = max;
			max = prevbevp->vec[1];
		}
		if (min != max) {
			if (min <= hvec1[1] && max >= hvec1[1]) {
				/* there's a transition, calc intersection point */
				mode = cu_isectLL(prevbevp->vec, bevp->vec, hvec1, hvec2, 0, 1, &lab, &mu, vec);
				/* if lab==0.0 or lab==1.0 then the edge intersects exactly a transition
				 * only allow for one situation: we choose lab= 1.0
				 */
				if (mode >= 0 && lab != 0.0f) {
					if (vec[0] < hvec1[0]) links++;
					else rechts++;
				}
			}
		}
		prevbevp = bevp;
		bevp++;
	}

	return (links & 1) && (rechts & 1);
}


struct BevelSort {
	BevList *bl;
	float left;
	int dir;
};

static int vergxcobev(const void *a1, const void *a2)
{
	const struct BevelSort *x1 = (BevelSort*)a1, *x2 = (BevelSort*)a2;

	if (x1->left > x2->left)
		return 1;
	else if (x1->left < x2->left)
		return -1;
	return 0;
}

/* this function cannot be replaced with atan2, but why? */

static void calc_bevel_sin_cos(float x1, float y1, float x2, float y2,
                               float *r_sina, float *r_cosa)
{
	float t01, t02, x3, y3;

	t01 = sqrtf(x1 * x1 + y1 * y1);
	t02 = sqrtf(x2 * x2 + y2 * y2);
	if (t01 == 0.0f)
		t01 = 1.0f;
	if (t02 == 0.0f)
		t02 = 1.0f;

	x1 /= t01;
	y1 /= t01;
	x2 /= t02;
	y2 /= t02;

	t02 = x1 * x2 + y1 * y2;
	if (fabsf(t02) >= 1.0f)
		t02 = 0.5 * M_PI;
	else
		t02 = (saacos(t02)) / 2.0f;

	t02 = sinf(t02);
	if (t02 == 0.0f)
		t02 = 1.0f;

	x3 = x1 - x2;
	y3 = y1 - y2;
	if (x3 == 0 && y3 == 0) {
		x3 = y1;
		y3 = -x1;
	}
	else {
		t01 = sqrtf(x3 * x3 + y3 * y3);
		x3 /= t01;
		y3 /= t01;
	}

	*r_sina = -y3 / t02;
	*r_cosa =  x3 / t02;

}

static void alfa_bezpart(BezTriple *prevbezt, BezTriple *bezt, Nurb *nu, float *tilt_array, float *radius_array,
                         float *weight_array, int resolu, int stride)
{
	BezTriple *pprev, *next, *last;
	float fac, dfac, t[4];
	int a;

	if (tilt_array == NULL && radius_array == NULL)
		return;

	last = nu->bezt + (nu->pntsu - 1);

	/* returns a point */
	if (prevbezt == nu->bezt) {
		if (nu->flagu & CU_NURB_CYCLIC)
			pprev = last;
		else
			pprev = prevbezt;
	}
	else
		pprev = prevbezt - 1;

	/* next point */
	if (bezt == last) {
		if (nu->flagu & CU_NURB_CYCLIC)
			next = nu->bezt;
		else
			next = bezt;
	}
	else
		next = bezt + 1;

	fac = 0.0;
	dfac = 1.0f / (float)resolu;

	for (a = 0; a < resolu; a++, fac += dfac) {
		if (tilt_array) {
			if (nu->tilt_interp == KEY_CU_EASE) { /* May as well support for tilt also 2.47 ease interp */
				*tilt_array = prevbezt->alfa +
					(bezt->alfa - prevbezt->alfa) * (3.0f * fac * fac - 2.0f * fac * fac * fac);
			}
			else {
				key_curve_position_weights(fac, t, nu->tilt_interp);
				*tilt_array = t[0] * pprev->alfa + t[1] * prevbezt->alfa + t[2] * bezt->alfa + t[3] * next->alfa;
			}

			tilt_array = (float *)(((char *)tilt_array) + stride);
		}

		if (radius_array) {
			if (nu->radius_interp == KEY_CU_EASE) {
				/* Support 2.47 ease interp
				 * Note! - this only takes the 2 points into account,
				 * giving much more localized results to changes in radius, sometimes you want that */
				*radius_array = prevbezt->radius +
					(bezt->radius - prevbezt->radius) * (3.0f * fac * fac - 2.0f * fac * fac * fac);
			}
			else {

				/* reuse interpolation from tilt if we can */
				if (tilt_array == NULL || nu->tilt_interp != nu->radius_interp) {
					key_curve_position_weights(fac, t, nu->radius_interp);
				}
				*radius_array = t[0] * pprev->radius + t[1] * prevbezt->radius +
					t[2] * bezt->radius + t[3] * next->radius;
			}

			radius_array = (float *)(((char *)radius_array) + stride);
		}

		if (weight_array) {
			/* basic interpolation for now, could copy tilt interp too  */
			*weight_array = prevbezt->weight +
				(bezt->weight - prevbezt->weight) * (3.0f * fac * fac - 2.0f * fac * fac * fac);

			weight_array = (float *)(((char *)weight_array) + stride);
		}
	}
}

/* make_bevel_list_3D_* funcs, at a minimum these must
 * fill in the bezp->quat and bezp->dir values */

/* utility for make_bevel_list_3D_* funcs */
static void bevel_list_calc_bisect(BevList *bl)
{
	BevPoint *bevp2, *bevp1, *bevp0;
	int nr;
	bool is_cyclic = bl->poly != -1;

	if (is_cyclic) {
		bevp2 = bl->bevpoints;
		bevp1 = bevp2 + (bl->nr - 1);
		bevp0 = bevp1 - 1;
		nr = bl->nr;
	}
	else {
		/* If spline is not cyclic, direction of first and
		 * last bevel points matches direction of CV handle.
		 *
		 * This is getting calculated earlier when we know
		 * CV's handles and here we might simply skip evaluation
		 * of direction for this guys.
		 */

		bevp0 = bl->bevpoints;
		bevp1 = bevp0 + 1;
		bevp2 = bevp1 + 1;

		nr = bl->nr - 2;
	}

	while (nr--) {
		/* totally simple */
		bisect_v3_v3v3v3(bevp1->dir, bevp0->vec, bevp1->vec, bevp2->vec);

		bevp0 = bevp1;
		bevp1 = bevp2;
		bevp2++;
	}
}
static void bevel_list_flip_tangents(BevList *bl)
{
	BevPoint *bevp2, *bevp1, *bevp0;
	int nr;

	bevp2 = bl->bevpoints;
	bevp1 = bevp2 + (bl->nr - 1);
	bevp0 = bevp1 - 1;

	nr = bl->nr;
	while (nr--) {
		if (angle_normalized_v3v3(bevp0->tan, bevp1->tan) > DEG2RADF(90.0f))
			negate_v3(bevp1->tan);

		bevp0 = bevp1;
		bevp1 = bevp2;
		bevp2++;
	}
}
/* apply user tilt */
static void bevel_list_apply_tilt(BevList *bl)
{
	BevPoint *bevp2, *bevp1;
	int nr;
	float q[4];

	bevp2 = bl->bevpoints;
	bevp1 = bevp2 + (bl->nr - 1);

	nr = bl->nr;
	while (nr--) {
		axis_angle_to_quat(q, bevp1->dir, bevp1->alfa);
		mul_qt_qtqt(bevp1->quat, q, bevp1->quat);
		normalize_qt(bevp1->quat);

		bevp1 = bevp2;
		bevp2++;
	}
}
/* smooth quats, this function should be optimized, it can get slow with many iterations. */
static void bevel_list_smooth(BevList *bl, int smooth_iter)
{
	BevPoint *bevp2, *bevp1, *bevp0;
	int nr;

	float q[4];
	float bevp0_quat[4];
	int a;

	for (a = 0; a < smooth_iter; a++) {
		bevp2 = bl->bevpoints;
		bevp1 = bevp2 + (bl->nr - 1);
		bevp0 = bevp1 - 1;

		nr = bl->nr;

		if (bl->poly == -1) { /* check its not cyclic */
			/* skip the first point */
			/* bevp0 = bevp1; */
			bevp1 = bevp2;
			bevp2++;
			nr--;

			bevp0 = bevp1;
			bevp1 = bevp2;
			bevp2++;
			nr--;
		}

		copy_qt_qt(bevp0_quat, bevp0->quat);

		while (nr--) {
			/* interpolate quats */
			float zaxis[3] = {0, 0, 1}, cross[3], q2[4];
			interp_qt_qtqt(q, bevp0_quat, bevp2->quat, 0.5);
			normalize_qt(q);

			mul_qt_v3(q, zaxis);
			cross_v3_v3v3(cross, zaxis, bevp1->dir);
			axis_angle_to_quat(q2, cross, angle_normalized_v3v3(zaxis, bevp1->dir));
			normalize_qt(q2);

			copy_qt_qt(bevp0_quat, bevp1->quat);
			mul_qt_qtqt(q, q2, q);
			interp_qt_qtqt(bevp1->quat, bevp1->quat, q, 0.5);
			normalize_qt(bevp1->quat);

			/* bevp0 = bevp1; */ /* UNUSED */
			bevp1 = bevp2;
			bevp2++;
		}
	}
}

static void make_bevel_list_3D_zup(BevList *bl)
{
	BevPoint *bevp = bl->bevpoints;
	int nr = bl->nr;

	bevel_list_calc_bisect(bl);

	while (nr--) {
		vec_to_quat(bevp->quat, bevp->dir, 5, 1);
		bevp++;
	}
}

static void minimum_twist_between_two_points(BevPoint *current_point, BevPoint *previous_point)
{
	float angle = angle_normalized_v3v3(previous_point->dir, current_point->dir);
	float q[4];

	if (angle > 0.0f) { /* otherwise we can keep as is */
		float cross_tmp[3];
		cross_v3_v3v3(cross_tmp, previous_point->dir, current_point->dir);
		axis_angle_to_quat(q, cross_tmp, angle);
		mul_qt_qtqt(current_point->quat, q, previous_point->quat);
	}
	else {
		copy_qt_qt(current_point->quat, previous_point->quat);
	}
}

static void make_bevel_list_3D_minimum_twist(BevList *bl)
{
	BevPoint *bevp2, *bevp1, *bevp0; /* standard for all make_bevel_list_3D_* funcs */
	int nr;
	float q[4];

	bevel_list_calc_bisect(bl);

	bevp2 = bl->bevpoints;
	bevp1 = bevp2 + (bl->nr - 1);
	bevp0 = bevp1 - 1;

	nr = bl->nr;
	while (nr--) {

		if (nr + 4 > bl->nr) { /* first time and second time, otherwise first point adjusts last */
			vec_to_quat(bevp1->quat, bevp1->dir, 5, 1);
		}
		else {
			minimum_twist_between_two_points(bevp1, bevp0);
		}

		bevp0 = bevp1;
		bevp1 = bevp2;
		bevp2++;
	}

	if (bl->poly != -1) { /* check for cyclic */

		/* Need to correct for the start/end points not matching
		 * do this by calculating the tilt angle difference, then apply
		 * the rotation gradually over the entire curve
		 *
		 * note that the split is between last and second last, rather than first/last as youd expect.
		 *
		 * real order is like this
		 * 0,1,2,3,4 --> 1,2,3,4,0
		 *
		 * this is why we compare last with second last
		 * */
		float vec_1[3] = {0, 1, 0}, vec_2[3] = {0, 1, 0}, angle, ang_fac, cross_tmp[3];

		BevPoint *bevp_first;
		BevPoint *bevp_last;


		bevp_first = bl->bevpoints;
		bevp_first += bl->nr - 1;
		bevp_last = bevp_first;
		bevp_last--;

		/* quats and vec's are normalized, should not need to re-normalize */
		mul_qt_v3(bevp_first->quat, vec_1);
		mul_qt_v3(bevp_last->quat, vec_2);
		normalize_v3(vec_1);
		normalize_v3(vec_2);

		/* align the vector, can avoid this and it looks 98% OK but
		 * better to align the angle quat roll's before comparing */
		{
			cross_v3_v3v3(cross_tmp, bevp_last->dir, bevp_first->dir);
			angle = angle_normalized_v3v3(bevp_first->dir, bevp_last->dir);
			axis_angle_to_quat(q, cross_tmp, angle);
			mul_qt_v3(q, vec_2);
		}

		angle = angle_normalized_v3v3(vec_1, vec_2);

		/* flip rotation if needs be */
		cross_v3_v3v3(cross_tmp, vec_1, vec_2);
		normalize_v3(cross_tmp);
		if (angle_normalized_v3v3(bevp_first->dir, cross_tmp) < DEG2RADF(90.0f))
			angle = -angle;

		bevp2 = bl->bevpoints;
		bevp1 = bevp2 + (bl->nr - 1);
		bevp0 = bevp1 - 1;

		nr = bl->nr;
		while (nr--) {
			ang_fac = angle * (1.0f - ((float)nr / bl->nr)); /* also works */

			axis_angle_to_quat(q, bevp1->dir, ang_fac);
			mul_qt_qtqt(bevp1->quat, q, bevp1->quat);

			bevp0 = bevp1;
			bevp1 = bevp2;
			bevp2++;
		}
	}
	else {
		/* Need to correct quat for the first/last point,
		 * this is so because previously it was only calculated
		 * using it's own direction, which might not correspond
		 * the twist of neighbor point.
		 */
		bevp1 = bl->bevpoints;
		bevp0 = bevp1 + 1;
		minimum_twist_between_two_points(bevp1, bevp0);

		bevp2 = bl->bevpoints;
		bevp1 = bevp2 + (bl->nr - 1);
		bevp0 = bevp1 - 1;
		minimum_twist_between_two_points(bevp1, bevp0);
	}
}

static void make_bevel_list_3D_tangent(BevList *bl)
{
	BevPoint *bevp2, *bevp1, *bevp0; /* standard for all make_bevel_list_3D_* funcs */
	int nr;

	float bevp0_tan[3];

	bevel_list_calc_bisect(bl);
	bevel_list_flip_tangents(bl);

	/* correct the tangents */
	bevp2 = bl->bevpoints;
	bevp1 = bevp2 + (bl->nr - 1);
	bevp0 = bevp1 - 1;

	nr = bl->nr;
	while (nr--) {
		float cross_tmp[3];
		cross_v3_v3v3(cross_tmp, bevp1->tan, bevp1->dir);
		cross_v3_v3v3(bevp1->tan, cross_tmp, bevp1->dir);
		normalize_v3(bevp1->tan);

		bevp0 = bevp1;
		bevp1 = bevp2;
		bevp2++;
	}


	/* now for the real twist calc */
	bevp2 = bl->bevpoints;
	bevp1 = bevp2 + (bl->nr - 1);
	bevp0 = bevp1 - 1;

	copy_v3_v3(bevp0_tan, bevp0->tan);

	nr = bl->nr;
	while (nr--) {
		/* make perpendicular, modify tan in place, is ok */
		float cross_tmp[3];
		float zero[3] = {0, 0, 0};

		cross_v3_v3v3(cross_tmp, bevp1->tan, bevp1->dir);
		normalize_v3(cross_tmp);
		tri_to_quat(bevp1->quat, zero, cross_tmp, bevp1->tan); /* XXX - could be faster */

		/* bevp0 = bevp1; */ /* UNUSED */
		bevp1 = bevp2;
		bevp2++;
	}
}

static void make_bevel_list_3D(BevList *bl, int smooth_iter, int twist_mode)
{
	switch (twist_mode) {
		case CU_TWIST_TANGENT:
			make_bevel_list_3D_tangent(bl);
			break;
		case CU_TWIST_MINIMUM:
			make_bevel_list_3D_minimum_twist(bl);
			break;
		default: /* CU_TWIST_Z_UP default, pre 2.49c */
			make_bevel_list_3D_zup(bl);
			break;
	}

	if (smooth_iter)
		bevel_list_smooth(bl, smooth_iter);

	bevel_list_apply_tilt(bl);
}

/* only for 2 points */
static void make_bevel_list_segment_3D(BevList *bl)
{
	float q[4];

	BevPoint *bevp2 = bl->bevpoints;
	BevPoint *bevp1 = bevp2 + 1;

	/* simple quat/dir */
	sub_v3_v3v3(bevp1->dir, bevp1->vec, bevp2->vec);
	normalize_v3(bevp1->dir);

	vec_to_quat(bevp1->quat, bevp1->dir, 5, 1);

	axis_angle_to_quat(q, bevp1->dir, bevp1->alfa);
	mul_qt_qtqt(bevp1->quat, q, bevp1->quat);
	normalize_qt(bevp1->quat);
	copy_v3_v3(bevp2->dir, bevp1->dir);
	copy_qt_qt(bevp2->quat, bevp1->quat);
}

/* only for 2 points */
static void make_bevel_list_segment_2D(BevList *bl)
{
	BevPoint *bevp2 = bl->bevpoints;
	BevPoint *bevp1 = bevp2 + 1;

	const float x1 = bevp1->vec[0] - bevp2->vec[0];
	const float y1 = bevp1->vec[1] - bevp2->vec[1];

	calc_bevel_sin_cos(x1, y1, -x1, -y1, &(bevp1->sina), &(bevp1->cosa));
	bevp2->sina = bevp1->sina;
	bevp2->cosa = bevp1->cosa;

	/* fill in dir & quat */
	make_bevel_list_segment_3D(bl);
}

static void make_bevel_list_2D(BevList *bl)
{
	/* note: bevp->dir and bevp->quat are not needed for beveling but are
	 * used when making a path from a 2D curve, therefor they need to be set - Campbell */

	BevPoint *bevp0, *bevp1, *bevp2;
	int nr;

	if (bl->poly != -1) {
		bevp2 = bl->bevpoints;
		bevp1 = bevp2 + (bl->nr - 1);
		bevp0 = bevp1 - 1;
		nr = bl->nr;
	}
	else {
		bevp0 = bl->bevpoints;
		bevp1 = bevp0 + 1;
		bevp2 = bevp1 + 1;

		nr = bl->nr - 2;
	}

	while (nr--) {
		const float x1 = bevp1->vec[0] - bevp0->vec[0];
		const float x2 = bevp1->vec[0] - bevp2->vec[0];
		const float y1 = bevp1->vec[1] - bevp0->vec[1];
		const float y2 = bevp1->vec[1] - bevp2->vec[1];

		calc_bevel_sin_cos(x1, y1, x2, y2, &(bevp1->sina), &(bevp1->cosa));

		/* from: make_bevel_list_3D_zup, could call but avoid a second loop.
		 * no need for tricky tilt calculation as with 3D curves */
		bisect_v3_v3v3v3(bevp1->dir, bevp0->vec, bevp1->vec, bevp2->vec);
		vec_to_quat(bevp1->quat, bevp1->dir, 5, 1);
		/* done with inline make_bevel_list_3D_zup */

		bevp0 = bevp1;
		bevp1 = bevp2;
		bevp2++;
	}

	/* correct non-cyclic cases */
	if (bl->poly == -1) {
		BevPoint *bevp;
		float angle;

		/* first */
		bevp = bl->bevpoints;
		angle = atan2(bevp->dir[0], bevp->dir[1]) - M_PI / 2.0;
		bevp->sina = sinf(angle);
		bevp->cosa = cosf(angle);
		vec_to_quat(bevp->quat, bevp->dir, 5, 1);

		/* last */
		bevp = bl->bevpoints;
		bevp += (bl->nr - 1);
		angle = atan2(bevp->dir[0], bevp->dir[1]) - M_PI / 2.0;
		bevp->sina = sinf(angle);
		bevp->cosa = cosf(angle);
		vec_to_quat(bevp->quat, bevp->dir, 5, 1);
	}
}

static void bevlist_firstlast_direction_calc_from_bpoint(Nurb *nu, BevList *bl)
{
	if (nu->pntsu > 1) {
		BPoint *first_bp = nu->bp, *last_bp = nu->bp + (nu->pntsu - 1);
		BevPoint *first_bevp, *last_bevp;

		first_bevp = bl->bevpoints;
		last_bevp = first_bevp + (bl->nr - 1);

		sub_v3_v3v3(first_bevp->dir, (first_bp + 1)->vec, first_bp->vec);
		normalize_v3(first_bevp->dir);

		sub_v3_v3v3(last_bevp->dir, last_bp->vec, (last_bp - 1)->vec);
		normalize_v3(last_bevp->dir);
	}
}

void BKE_curve_bevelList_free(ListBase *bev)
{
	BevList *bl, *blnext;
	for (bl = (BevList*)bev->first; bl != NULL; bl = blnext) {
		blnext = bl->next;
		if (bl->seglen != NULL) {
			MEM_freeN(bl->seglen);
		}
		if (bl->segbevcount != NULL) {
			MEM_freeN(bl->segbevcount);
		}
		MEM_freeN(bl);
	}
	bev->first = bev->last = NULL;
}

void BKE_curve_bevelList_make(Object *ob, ListBase *nurbs, bool for_render)
{
	/*
	 * - convert all curves to polys, with indication of resol and flags for double-vertices
	 * - possibly; do a smart vertice removal (in case Nurb)
	 * - separate in individual blocks with BoundBox
	 * - AutoHole detection
	 */

	/* this function needs an object, because of tflag and upflag */
	Curve *cu = (Curve*)ob->data;
	Nurb *nu;
	BezTriple *bezt, *prevbezt;
	BPoint *bp;
	BevList *bl, *blnew, *blnext;
	BevPoint *bevp, *bevp2, *bevp1 = NULL, *bevp0;
	const float treshold = 0.00001f;
	float min, inp;
	float *seglen;
	struct BevelSort *sortdata, *sd, *sd1;
	int a, b, nr, poly, resolu = 0, len = 0, segcount;
	int *segbevcount;
	bool do_tilt, do_radius, do_weight;
	bool is_editmode = false;
	ListBase *bev;

	/* segbevcount alsp requires seglen. */
	const bool need_seglen =
		ELEM(cu->bevfac1_mapping, CU_BEVFAC_MAP_SEGMENT, CU_BEVFAC_MAP_SPLINE) ||
		ELEM(cu->bevfac2_mapping, CU_BEVFAC_MAP_SEGMENT, CU_BEVFAC_MAP_SPLINE);

	bev = &ob->curve_cache->bev;

	/* do we need to calculate the radius for each point? */
	/* do_radius = (cu->bevobj || cu->taperobj || (cu->flag & CU_FRONT) || (cu->flag & CU_BACK)) ? 0 : 1; */

	/* STEP 1: MAKE POLYS  */

	BKE_curve_bevelList_free(&ob->curve_cache->bev);
	nu = (Nurb*)nurbs->first;

	if (cu->editnurb && ob->type != OB_FONT) {
		is_editmode = 1;
	}

	for (; nu; nu = nu->next) {
		
		if (nu->hide && is_editmode)
			continue;
		
		/* check if we will calculate tilt data */
		do_tilt = CU_DO_TILT(cu, nu);
		do_radius = CU_DO_RADIUS(cu, nu); /* normal display uses the radius, better just to calculate them */
		do_weight = true;

		/* check we are a single point? also check we are not a surface and that the orderu is sane,
		 * enforced in the UI but can go wrong possibly */
		if (!BKE_nurb_check_valid_u(nu)) {
			bl = (BevList*)MEM_callocN(sizeof(BevList) + 1 * sizeof(BevPoint), "makeBevelList1");
			BLI_addtail(bev, bl);
			bl->nr = 0;
			bl->charidx = nu->charidx;
		}
		else {
			if (for_render && cu->resolu_ren != 0)
				resolu = cu->resolu_ren;
			else
				resolu = nu->resolu;

			segcount = SEGMENTSU(nu);

			if (nu->type == CU_POLY) {
				len = nu->pntsu;
				bl = (BevList*)MEM_callocN(sizeof(BevList) + len * sizeof(BevPoint), "makeBevelList2");
				if (need_seglen) {
					bl->seglen = (float*)MEM_mallocN(segcount * sizeof(float), "makeBevelList2_seglen");
					bl->segbevcount = (int*)MEM_mallocN(segcount * sizeof(int), "makeBevelList2_segbevcount");
				}
				BLI_addtail(bev, bl);

				bl->poly = (nu->flagu & CU_NURB_CYCLIC) ? 0 : -1;
				bl->nr = len;
				bl->dupe_nr = 0;
				bl->charidx = nu->charidx;
				bevp = bl->bevpoints;
				bevp->offset = 0;
				bp = nu->bp;
				seglen = bl->seglen;
				segbevcount = bl->segbevcount;
				BLI_assert(segcount >= len);

				while (len--) {
					copy_v3_v3(bevp->vec, bp->vec);
					bevp->alfa = bp->alfa;
					bevp->radius = bp->radius;
					bevp->weight = bp->weight;
					bevp->split_tag = true;
					bp++;
					if (seglen != NULL) {
						*seglen = len_v3v3(bevp->vec, bp->vec);
						bevp++;
						bevp->offset = *seglen;
						if (*seglen > treshold) *segbevcount = 1;
						else *segbevcount = 0;
						seglen++;
						segbevcount++;
					}
					else {
						bevp++;
					}
				}

				if ((nu->flagu & CU_NURB_CYCLIC) == 0) {
					bevlist_firstlast_direction_calc_from_bpoint(nu, bl);
					if (seglen != NULL) {
						*seglen = len_v3v3(bevp->vec, nu->bp->vec);
						bl->bevpoints->offset = *seglen;
						*segbevcount = 1;
					}
				}
			}
			else if (nu->type == CU_BEZIER) {
				/* in case last point is not cyclic */
				len = segcount * resolu + 1;

				bl = (BevList*)MEM_callocN(sizeof(BevList) + len * sizeof(BevPoint), "makeBevelBPoints");
				if (need_seglen) {
					bl->seglen = (float*)MEM_mallocN(segcount * sizeof(float), "makeBevelBPoints_seglen");
					bl->segbevcount = (int*)MEM_mallocN(segcount * sizeof(int), "makeBevelBPoints_segbevcount");
				}
				BLI_addtail(bev, bl);

				bl->poly = (nu->flagu & CU_NURB_CYCLIC) ? 0 : -1;
				bl->charidx = nu->charidx;

				bevp = bl->bevpoints;
				seglen = bl->seglen;
				segbevcount = bl->segbevcount;

				bevp->offset = 0;
				if (seglen != NULL) {
					*seglen = 0;
					*segbevcount = 0;
				}

				a = nu->pntsu - 1;
				bezt = nu->bezt;
				if (nu->flagu & CU_NURB_CYCLIC) {
					a++;
					prevbezt = nu->bezt + (nu->pntsu - 1);
				}
				else {
					prevbezt = bezt;
					bezt++;
				}

				sub_v3_v3v3(bevp->dir, prevbezt->vec[2], prevbezt->vec[1]);
				normalize_v3(bevp->dir);

				BLI_assert(segcount >= a);

				while (a--) {
					if (prevbezt->h2 == HD_VECT && bezt->h1 == HD_VECT) {

						copy_v3_v3(bevp->vec, prevbezt->vec[1]);
						bevp->alfa = prevbezt->alfa;
						bevp->radius = prevbezt->radius;
						bevp->weight = prevbezt->weight;
						bevp->split_tag = true;
						bevp->dupe_tag = false;
						bevp++;
						bl->nr++;
						bl->dupe_nr = 1;
						if (seglen != NULL) {
							*seglen = len_v3v3(prevbezt->vec[1], bezt->vec[1]);
							bevp->offset = *seglen;
							seglen++;
							/* match segbevcount to the cleaned up bevel lists (see STEP 2) */
							if (bevp->offset > treshold) *segbevcount = 1;
							segbevcount++;
						}
					}
					else {
						/* always do all three, to prevent data hanging around */
						int j;

						/* BevPoint must stay aligned to 4 so sizeof(BevPoint)/sizeof(float) works */
						for (j = 0; j < 3; j++) {
							BKE_curve_forward_diff_bezier(prevbezt->vec[1][j],  prevbezt->vec[2][j],
							                              bezt->vec[0][j],      bezt->vec[1][j],
							                              &(bevp->vec[j]), resolu, sizeof(BevPoint));
						}

						/* if both arrays are NULL do nothiong */
						alfa_bezpart(prevbezt, bezt, nu,
						             do_tilt    ? &bevp->alfa : NULL,
						             do_radius  ? &bevp->radius : NULL,
						             do_weight  ? &bevp->weight : NULL,
						             resolu, sizeof(BevPoint));


						if (cu->twist_mode == CU_TWIST_TANGENT) {
							forward_diff_bezier_cotangent(prevbezt->vec[1], prevbezt->vec[2],
							                              bezt->vec[0],     bezt->vec[1],
							                              bevp->tan, resolu, sizeof(BevPoint));
						}

						/* indicate with handlecodes double points */
						if (prevbezt->h1 == prevbezt->h2) {
							if (prevbezt->h1 == 0 || prevbezt->h1 == HD_VECT)
								bevp->split_tag = true;
						}
						else {
							if (prevbezt->h1 == 0 || prevbezt->h1 == HD_VECT)
								bevp->split_tag = true;
							else if (prevbezt->h2 == 0 || prevbezt->h2 == HD_VECT)
								bevp->split_tag = true;
						}

						/* seglen */
						if (seglen != NULL) {
							*seglen = 0;
							*segbevcount = 0;
							for (j = 0; j < resolu; j++) {
								bevp0 = bevp;
								bevp++;
								bevp->offset = len_v3v3(bevp0->vec, bevp->vec);
								/* match seglen and segbevcount to the cleaned up bevel lists (see STEP 2) */
								if (bevp->offset > treshold) {
									*seglen += bevp->offset;
									*segbevcount += 1;
								}
							}
							seglen++;
							segbevcount++;
						}
						else {
							bevp += resolu;
						}
						bl->nr += resolu;
					}
					prevbezt = bezt;
					bezt++;
				}

				if ((nu->flagu & CU_NURB_CYCLIC) == 0) {      /* not cyclic: endpoint */
					copy_v3_v3(bevp->vec, prevbezt->vec[1]);
					bevp->alfa = prevbezt->alfa;
					bevp->radius = prevbezt->radius;
					bevp->weight = prevbezt->weight;

					sub_v3_v3v3(bevp->dir, prevbezt->vec[1], prevbezt->vec[0]);
					normalize_v3(bevp->dir);

					bl->nr++;
				}
			}
			else if (nu->type == CU_NURBS) {
				if (nu->pntsv == 1) {
					len = (resolu * segcount);

					bl = (BevList*)MEM_callocN(sizeof(BevList) + len * sizeof(BevPoint), "makeBevelList3");
					if (need_seglen) {
						bl->seglen = (float*)MEM_mallocN(segcount * sizeof(float), "makeBevelList3_seglen");
						bl->segbevcount = (int*)MEM_mallocN(segcount * sizeof(int), "makeBevelList3_segbevcount");
					}
					BLI_addtail(bev, bl);
					bl->nr = len;
					bl->dupe_nr = 0;
					bl->poly = (nu->flagu & CU_NURB_CYCLIC) ? 0 : -1;
					bl->charidx = nu->charidx;

					bevp = bl->bevpoints;
					seglen = bl->seglen;
					segbevcount = bl->segbevcount;

					BKE_nurb_makeCurve(nu, &bevp->vec[0],
					                   do_tilt      ? &bevp->alfa : NULL,
					                   do_radius    ? &bevp->radius : NULL,
					                   do_weight    ? &bevp->weight : NULL,
					                   resolu, sizeof(BevPoint));

					/* match seglen and segbevcount to the cleaned up bevel lists (see STEP 2) */
					if (seglen != NULL) {
						nr = segcount;
						bevp0 = bevp;
						bevp++;
						while (nr) {
							int j;
							*seglen = 0;
							*segbevcount = 0;
							/* We keep last bevel segment zero-length. */
							for (j = 0; j < ((nr == 1) ? (resolu - 1) : resolu); j++) {
								bevp->offset = len_v3v3(bevp0->vec, bevp->vec);
								if (bevp->offset > treshold) {
									*seglen += bevp->offset;
									*segbevcount += 1;
								}
								bevp0 = bevp;
								bevp++;
							}
							seglen++;
							segbevcount++;
							nr--;
						}
					}

					if ((nu->flagu & CU_NURB_CYCLIC) == 0) {
						bevlist_firstlast_direction_calc_from_bpoint(nu, bl);
					}
				}
			}
		}
	}

	/* STEP 2: DOUBLE POINTS AND AUTOMATIC RESOLUTION, REDUCE DATABLOCKS */
	bl = (BevList*)bev->first;
	while (bl) {
		if (bl->nr) { /* null bevel items come from single points */
			bool is_cyclic = bl->poly != -1;
			nr = bl->nr;
			if (is_cyclic) {
				bevp1 = bl->bevpoints;
				bevp0 = bevp1 + (nr - 1);
			}
			else {
				bevp0 = bl->bevpoints;
				bevp0->offset = 0;
				bevp1 = bevp0 + 1;
			}
			nr--;
			while (nr--) {
				if (seglen != NULL) {
					if (fabsf(bevp1->offset) < treshold) {
						bevp0->dupe_tag = true;
						bl->dupe_nr++;
					}
				}
				else {
					if (fabsf(bevp0->vec[0] - bevp1->vec[0]) < 0.00001f) {
						if (fabsf(bevp0->vec[1] - bevp1->vec[1]) < 0.00001f) {
							if (fabsf(bevp0->vec[2] - bevp1->vec[2]) < 0.00001f) {
								bevp0->dupe_tag = true;
								bl->dupe_nr++;
							}
						}
					}
				}
				bevp0 = bevp1;
				bevp1++;
			}
		}
		bl = bl->next;
	}
	bl = (BevList*)bev->first;
	while (bl) {
		blnext = bl->next;
		if (bl->nr && bl->dupe_nr) {
			nr = bl->nr - bl->dupe_nr + 1;  /* +1 because vectorbezier sets flag too */
			blnew = (BevList*)MEM_mallocN(sizeof(BevList) + nr * sizeof(BevPoint), "makeBevelList4");
			memcpy(blnew, bl, sizeof(BevList));
			blnew->segbevcount = bl->segbevcount;
			blnew->seglen = bl->seglen;
			blnew->nr = 0;
			BLI_remlink(bev, bl);
			BLI_insertlinkbefore(bev, blnext, blnew);    /* to make sure bevlijst is tuned with nurblist */
			bevp0 = bl->bevpoints;
			bevp1 = blnew->bevpoints;
			nr = bl->nr;
			while (nr--) {
				if (bevp0->dupe_tag == 0) {
					memcpy(bevp1, bevp0, sizeof(BevPoint));
					bevp1++;
					blnew->nr++;
				}
				bevp0++;
			}
			MEM_freeN(bl);
			blnew->dupe_nr = 0;
		}
		bl = blnext;
	}

	/* STEP 3: POLYS COUNT AND AUTOHOLE */
	bl = (BevList*)bev->first;
	poly = 0;
	while (bl) {
		if (bl->nr && bl->poly >= 0) {
			poly++;
			bl->poly = poly;
			bl->hole = 0;
		}
		bl = bl->next;
	}

	/* find extreme left points, also test (turning) direction */
	if (poly > 0) {
		sd = sortdata = (struct BevelSort*)MEM_mallocN(sizeof(struct BevelSort) * poly, "makeBevelList5");
		bl = (BevList*)bev->first;
		while (bl) {
			if (bl->poly > 0) {

				min = 300000.0;
				bevp = bl->bevpoints;
				nr = bl->nr;
				while (nr--) {
					if (min > bevp->vec[0]) {
						min = bevp->vec[0];
						bevp1 = bevp;
					}
					bevp++;
				}
				sd->bl = bl;
				sd->left = min;

				bevp = bl->bevpoints;
				if (bevp1 == bevp)
					bevp0 = bevp + (bl->nr - 1);
				else
					bevp0 = bevp1 - 1;
				bevp = bevp + (bl->nr - 1);
				if (bevp1 == bevp)
					bevp2 = bl->bevpoints;
				else
					bevp2 = bevp1 + 1;

				inp = ((bevp1->vec[0] - bevp0->vec[0]) * (bevp0->vec[1] - bevp2->vec[1]) +
				       (bevp0->vec[1] - bevp1->vec[1]) * (bevp0->vec[0] - bevp2->vec[0]));

				if (inp > 0.0f)
					sd->dir = 1;
				else
					sd->dir = 0;

				sd++;
			}

			bl = bl->next;
		}
		qsort(sortdata, poly, sizeof(struct BevelSort), vergxcobev);

		sd = sortdata + 1;
		for (a = 1; a < poly; a++, sd++) {
			bl = sd->bl;     /* is bl a hole? */
			sd1 = sortdata + (a - 1);
			for (b = a - 1; b >= 0; b--, sd1--) { /* all polys to the left */
				if (sd1->bl->charidx == bl->charidx) { /* for text, only check matching char */
					if (bevelinside(sd1->bl, bl)) {
						bl->hole = 1 - sd1->bl->hole;
						break;
					}
				}
			}
		}

		/* turning direction */
		if ((cu->flag & CU_3D) == 0) {
			sd = sortdata;
			for (a = 0; a < poly; a++, sd++) {
				if (sd->bl->hole == sd->dir) {
					bl = sd->bl;
					bevp1 = bl->bevpoints;
					bevp2 = bevp1 + (bl->nr - 1);
					nr = bl->nr / 2;
					while (nr--) {
						SWAP(BevPoint, *bevp1, *bevp2);
						bevp1++;
						bevp2--;
					}
				}
			}
		}
		MEM_freeN(sortdata);
	}

	/* STEP 4: 2D-COSINES or 3D ORIENTATION */
	if ((cu->flag & CU_3D) == 0) {
		/* 2D Curves */
		for (bl = (BevList*)bev->first; bl; bl = bl->next) {
			if (bl->nr < 2) {
				BevPoint *bevp = bl->bevpoints;
				unit_qt(bevp->quat);
			}
			else if (bl->nr == 2) {   /* 2 pnt, treat separate */
				make_bevel_list_segment_2D(bl);
			}
			else {
				make_bevel_list_2D(bl);
			}
		}
	}
	else {
		/* 3D Curves */
		for (bl = (BevList*)bev->first; bl; bl = bl->next) {
			if (bl->nr < 2) {
				BevPoint *bevp = bl->bevpoints;
				unit_qt(bevp->quat);
			}
			else if (bl->nr == 2) {   /* 2 pnt, treat separate */
				make_bevel_list_segment_3D(bl);
			}
			else {
				make_bevel_list_3D(bl, (int)(resolu * cu->twist_smooth), cu->twist_mode);
			}
		}
	}
}

/* ****************** HANDLES ************** */

static void calchandleNurb_intern(BezTriple *bezt, BezTriple *prev, BezTriple *next,
                                  bool is_fcurve, bool skip_align)
{
	/* defines to avoid confusion */
#define p2_h1 (p2 - 3)
#define p2_h2 (p2 + 3)

	float *p1, *p2, *p3, pt[3];
	float dvec_a[3], dvec_b[3];
	float len, len_a, len_b;
	float len_ratio;
	const float eps = 1e-5;

	if (bezt->h1 == 0 && bezt->h2 == 0) {
		return;
	}

	p2 = bezt->vec[1];

	if (prev == NULL) {
		p3 = next->vec[1];
		pt[0] = 2.0f * p2[0] - p3[0];
		pt[1] = 2.0f * p2[1] - p3[1];
		pt[2] = 2.0f * p2[2] - p3[2];
		p1 = pt;
	}
	else {
		p1 = prev->vec[1];
	}

	if (next == NULL) {
		pt[0] = 2.0f * p2[0] - p1[0];
		pt[1] = 2.0f * p2[1] - p1[1];
		pt[2] = 2.0f * p2[2] - p1[2];
		p3 = pt;
	}
	else {
		p3 = next->vec[1];
	}

	sub_v3_v3v3(dvec_a, p2, p1);
	sub_v3_v3v3(dvec_b, p3, p2);

	if (is_fcurve) {
		len_a = dvec_a[0];
		len_b = dvec_b[0];
	}
	else {
		len_a = len_v3(dvec_a);
		len_b = len_v3(dvec_b);
	}

	if (len_a == 0.0f) len_a = 1.0f;
	if (len_b == 0.0f) len_b = 1.0f;

	len_ratio = len_a / len_b;

	if (ELEM(bezt->h1, HD_AUTO, HD_AUTO_ANIM) || ELEM(bezt->h2, HD_AUTO, HD_AUTO_ANIM)) {    /* auto */
		float tvec[3];
		tvec[0] = dvec_b[0] / len_b + dvec_a[0] / len_a;
		tvec[1] = dvec_b[1] / len_b + dvec_a[1] / len_a;
		tvec[2] = dvec_b[2] / len_b + dvec_a[2] / len_a;

		if (is_fcurve) {
			len = tvec[0];
		}
		else {
			len = len_v3(tvec);
		}
		len *=  2.5614f;

		if (len != 0.0f) {
			/* only for fcurves */
			bool leftviolate = false, rightviolate = false;

			if (len_a > 5.0f * len_b)
				len_a = 5.0f * len_b;
			if (len_b > 5.0f * len_a)
				len_b = 5.0f * len_a;

			if (ELEM(bezt->h1, HD_AUTO, HD_AUTO_ANIM)) {
				len_a /= len;
				madd_v3_v3v3fl(p2_h1, p2, tvec, -len_a);

				if ((bezt->h1 == HD_AUTO_ANIM) && next && prev) { /* keep horizontal if extrema */
					float ydiff1 = prev->vec[1][1] - bezt->vec[1][1];
					float ydiff2 = next->vec[1][1] - bezt->vec[1][1];
					if ((ydiff1 <= 0.0f && ydiff2 <= 0.0f) || (ydiff1 >= 0.0f && ydiff2 >= 0.0f)) {
						bezt->vec[0][1] = bezt->vec[1][1];
					}
					else { /* handles should not be beyond y coord of two others */
						if (ydiff1 <= 0.0f) {
							if (prev->vec[1][1] > bezt->vec[0][1]) {
								bezt->vec[0][1] = prev->vec[1][1];
								leftviolate = 1;
							}
						}
						else {
							if (prev->vec[1][1] < bezt->vec[0][1]) {
								bezt->vec[0][1] = prev->vec[1][1];
								leftviolate = 1;
							}
						}
					}
				}
			}
			if (ELEM(bezt->h2, HD_AUTO, HD_AUTO_ANIM)) {
				len_b /= len;
				madd_v3_v3v3fl(p2_h2, p2, tvec,  len_b);

				if ((bezt->h2 == HD_AUTO_ANIM) && next && prev) { /* keep horizontal if extrema */
					float ydiff1 = prev->vec[1][1] - bezt->vec[1][1];
					float ydiff2 = next->vec[1][1] - bezt->vec[1][1];
					if ( (ydiff1 <= 0.0f && ydiff2 <= 0.0f) || (ydiff1 >= 0.0f && ydiff2 >= 0.0f) ) {
						bezt->vec[2][1] = bezt->vec[1][1];
					}
					else { /* handles should not be beyond y coord of two others */
						if (ydiff1 <= 0.0f) {
							if (next->vec[1][1] < bezt->vec[2][1]) {
								bezt->vec[2][1] = next->vec[1][1];
								rightviolate = 1;
							}
						}
						else {
							if (next->vec[1][1] > bezt->vec[2][1]) {
								bezt->vec[2][1] = next->vec[1][1];
								rightviolate = 1;
							}
						}
					}
				}
			}
			if (leftviolate || rightviolate) { /* align left handle */
				BLI_assert(is_fcurve);
#if 0
				if (is_fcurve)
#endif
				{
					/* simple 2d calculation */
					float h1_x = p2_h1[0] - p2[0];
					float h2_x = p2[0] - p2_h2[0];

					if (leftviolate) {
						p2_h2[1] = p2[1] + ((p2[1] - p2_h1[1]) / h1_x) * h2_x;
					}
					else {
						p2_h1[1] = p2[1] + ((p2[1] - p2_h2[1]) / h2_x) * h1_x;
					}
				}
#if 0
				else {
					float h1[3], h2[3];
					float dot;

					sub_v3_v3v3(h1, p2_h1, p2);
					sub_v3_v3v3(h2, p2, p2_h2);

					len_a = normalize_v3(h1);
					len_b = normalize_v3(h2);

					dot = dot_v3v3(h1, h2);

					if (leftviolate) {
						mul_v3_fl(h1, dot * len_b);
						sub_v3_v3v3(p2_h2, p2, h1);
					}
					else {
						mul_v3_fl(h2, dot * len_a);
						add_v3_v3v3(p2_h1, p2, h2);
					}
				}
#endif
			}
		}
	}

	if (bezt->h1 == HD_VECT) {    /* vector */
		madd_v3_v3v3fl(p2_h1, p2, dvec_a, -1.0f / 3.0f);
	}
	if (bezt->h2 == HD_VECT) {
		madd_v3_v3v3fl(p2_h2, p2, dvec_b,  1.0f / 3.0f);
	}

	if (skip_align || (!ELEM(HD_ALIGN, bezt->h1, bezt->h2) && !ELEM(HD_ALIGN_DOUBLESIDE, bezt->h1, bezt->h2))) {
		/* handles need to be updated during animation and applying stuff like hooks,
		 * but in such situations it's quite difficult to distinguish in which order
		 * align handles should be aligned so skip them for now */
		return;
	}

	len_a = len_v3v3(p2, p2_h1);
	len_b = len_v3v3(p2, p2_h2);

	if (len_a == 0.0f) len_a = 1.0f;
	if (len_b == 0.0f) len_b = 1.0f;

	len_ratio = len_a / len_b;

	if (bezt->f1 & SELECT) { /* order of calculation */
		if (ELEM(bezt->h2, HD_ALIGN, HD_ALIGN_DOUBLESIDE)) { /* aligned */
			if (len_a > eps) {
				len = 1.0f / len_ratio;
				p2_h2[0] = p2[0] + len * (p2[0] - p2_h1[0]);
				p2_h2[1] = p2[1] + len * (p2[1] - p2_h1[1]);
				p2_h2[2] = p2[2] + len * (p2[2] - p2_h1[2]);
			}
		}
		if (ELEM(bezt->h1, HD_ALIGN, HD_ALIGN_DOUBLESIDE)) {
			if (len_b > eps) {
				len = len_ratio;
				p2_h1[0] = p2[0] + len * (p2[0] - p2_h2[0]);
				p2_h1[1] = p2[1] + len * (p2[1] - p2_h2[1]);
				p2_h1[2] = p2[2] + len * (p2[2] - p2_h2[2]);
			}
		}
	}
	else {
		if (ELEM(bezt->h1, HD_ALIGN, HD_ALIGN_DOUBLESIDE)) {
			if (len_b > eps) {
				len = len_ratio;
				p2_h1[0] = p2[0] + len * (p2[0] - p2_h2[0]);
				p2_h1[1] = p2[1] + len * (p2[1] - p2_h2[1]);
				p2_h1[2] = p2[2] + len * (p2[2] - p2_h2[2]);
			}
		}
		if (ELEM(bezt->h2, HD_ALIGN, HD_ALIGN_DOUBLESIDE)) {   /* aligned */
			if (len_a > eps) {
				len = 1.0f / len_ratio;
				p2_h2[0] = p2[0] + len * (p2[0] - p2_h1[0]);
				p2_h2[1] = p2[1] + len * (p2[1] - p2_h1[1]);
				p2_h2[2] = p2[2] + len * (p2[2] - p2_h1[2]);
			}
		}
	}

#undef p2_h1
#undef p2_h2
}

static void calchandlesNurb_intern(Nurb *nu, bool skip_align)
{
	BezTriple *bezt, *prev, *next;
	int a;

	if (nu->type != CU_BEZIER)
		return;
	if (nu->pntsu < 2)
		return;

	a = nu->pntsu;
	bezt = nu->bezt;
	if (nu->flagu & CU_NURB_CYCLIC) prev = bezt + (a - 1);
	else prev = NULL;
	next = bezt + 1;

	while (a--) {
		calchandleNurb_intern(bezt, prev, next, 0, skip_align);
		prev = bezt;
		if (a == 1) {
			if (nu->flagu & CU_NURB_CYCLIC)
				next = nu->bezt;
			else
				next = NULL;
		}
		else
			next++;

		bezt++;
	}
}

void BKE_nurb_handle_calc(BezTriple *bezt, BezTriple *prev, BezTriple *next, const bool is_fcurve)
{
	calchandleNurb_intern(bezt, prev, next, is_fcurve, false);
}

void BKE_nurb_handles_calc(Nurb *nu) /* first, if needed, set handle flags */
{
	calchandlesNurb_intern(nu, false);
}

/* similar to BKE_nurb_handle_calc but for curves and
 * figures out the previous and next for us */
void BKE_nurb_handle_calc_simple(Nurb *nu, BezTriple *bezt)
{
	if (nu->pntsu > 1) {
		BezTriple *prev = BKE_nurb_bezt_get_prev(nu, bezt);
		BezTriple *next = BKE_nurb_bezt_get_next(nu, bezt);
		BKE_nurb_handle_calc(bezt, prev, next, 0);
	}
}

/**
 * Use when something has changed handle positions.
 *
 * The caller needs to recalculate handles.
 */
void BKE_nurb_bezt_handle_test(BezTriple *bezt, const bool use_handle)
{
	short flag = 0;

#define SEL_F1 (1 << 0)
#define SEL_F2 (1 << 1)
#define SEL_F3 (1 << 2)

	if (use_handle) {
		if (bezt->f1 & SELECT) flag |= SEL_F1;
		if (bezt->f2 & SELECT) flag |= SEL_F2;
		if (bezt->f3 & SELECT) flag |= SEL_F3;
	}
	else {
		flag = (bezt->f2 & SELECT) ? (SEL_F1 | SEL_F2 | SEL_F3) : 0;
	}

	/* check for partial selection */
	if (!ELEM(flag, 0, SEL_F1 | SEL_F2 | SEL_F3)) {
		if (ELEM(bezt->h1, HD_AUTO, HD_AUTO_ANIM)) {
			bezt->h1 = HD_ALIGN;
		}
		if (ELEM(bezt->h2, HD_AUTO, HD_AUTO_ANIM)) {
			bezt->h2 = HD_ALIGN;
		}

		if (bezt->h1 == HD_VECT) {
			if ((!(flag & SEL_F1)) != (!(flag & SEL_F2))) {
				bezt->h1 = HD_FREE;
			}
		}
		if (bezt->h2 == HD_VECT) {
			if ((!(flag & SEL_F3)) != (!(flag & SEL_F2))) {
				bezt->h2 = HD_FREE;
			}
		}
	}

#undef SEL_F1
#undef SEL_F2
#undef SEL_F3

}

void BKE_nurb_handles_test(Nurb *nu, const bool use_handle)
{
	BezTriple *bezt;
	int a;

	if (nu->type != CU_BEZIER)
		return;

	bezt = nu->bezt;
	a = nu->pntsu;
	while (a--) {
		BKE_nurb_bezt_handle_test(bezt, use_handle);
		bezt++;
	}

	BKE_nurb_handles_calc(nu);
}

void BKE_nurb_handles_autocalc(Nurb *nu, int flag)
{
	/* checks handle coordinates and calculates type */
	const float eps = 0.0001f;
	const float eps_sq = eps * eps;

	BezTriple *bezt2, *bezt1, *bezt0;
	int i;

	if (nu == NULL || nu->bezt == NULL)
		return;

	bezt2 = nu->bezt;
	bezt1 = bezt2 + (nu->pntsu - 1);
	bezt0 = bezt1 - 1;
	i = nu->pntsu;

	while (i--) {
		bool align = false, leftsmall = false, rightsmall = false;

		/* left handle: */
		if (flag == 0 || (bezt1->f1 & flag) ) {
			bezt1->h1 = HD_FREE;
			/* distance too short: vectorhandle */
			if (len_squared_v3v3(bezt1->vec[1], bezt0->vec[1]) < eps_sq) {
				bezt1->h1 = HD_VECT;
				leftsmall = true;
			}
			else {
				/* aligned handle? */
				if (dist_squared_to_line_v3(bezt1->vec[1], bezt1->vec[0], bezt1->vec[2]) < eps_sq) {
					align = true;
					bezt1->h1 = HD_ALIGN;
				}
				/* or vector handle? */
				if (dist_squared_to_line_v3(bezt1->vec[0], bezt1->vec[1], bezt0->vec[1]) < eps_sq)
					bezt1->h1 = HD_VECT;
			}
		}
		/* right handle: */
		if (flag == 0 || (bezt1->f3 & flag) ) {
			bezt1->h2 = HD_FREE;
			/* distance too short: vectorhandle */
			if (len_squared_v3v3(bezt1->vec[1], bezt2->vec[1]) < eps_sq) {
				bezt1->h2 = HD_VECT;
				rightsmall = true;
			}
			else {
				/* aligned handle? */
				if (align) bezt1->h2 = HD_ALIGN;

				/* or vector handle? */
				if (dist_squared_to_line_v3(bezt1->vec[2], bezt1->vec[1], bezt2->vec[1]) < eps_sq)
					bezt1->h2 = HD_VECT;
			}
		}
		if (leftsmall && bezt1->h2 == HD_ALIGN)
			bezt1->h2 = HD_FREE;
		if (rightsmall && bezt1->h1 == HD_ALIGN)
			bezt1->h1 = HD_FREE;

		/* undesired combination: */
		if (bezt1->h1 == HD_ALIGN && bezt1->h2 == HD_VECT)
			bezt1->h1 = HD_FREE;
		if (bezt1->h2 == HD_ALIGN && bezt1->h1 == HD_VECT)
			bezt1->h2 = HD_FREE;

		bezt0 = bezt1;
		bezt1 = bezt2;
		bezt2++;
	}

	BKE_nurb_handles_calc(nu);
}

void BKE_nurbList_handles_autocalc(ListBase *editnurb, int flag)
{
	Nurb *nu;

	nu = (Nurb*)editnurb->first;
	while (nu) {
		BKE_nurb_handles_autocalc(nu, flag);
		nu = nu->next;
	}
}

void BKE_nurbList_handles_set(ListBase *editnurb, const char code)
{
	/* code==1: set autohandle */
	/* code==2: set vectorhandle */
	/* code==3 (HD_ALIGN) it toggle, vectorhandles become HD_FREE */
	/* code==4: sets icu flag to become IPO_AUTO_HORIZ, horizontal extremes on auto-handles */
	/* code==5: Set align, like 3 but no toggle */
	/* code==6: Clear align, like 3 but no toggle */
	Nurb *nu;
	BezTriple *bezt;
	int a;

	if (ELEM(code, HD_AUTO, HD_VECT)) {
		nu = (Nurb*)editnurb->first;
		while (nu) {
			if (nu->type == CU_BEZIER) {
				bezt = nu->bezt;
				a = nu->pntsu;
				while (a--) {
					if ((bezt->f1 & SELECT) || (bezt->f3 & SELECT)) {
						if (bezt->f1 & SELECT)
							bezt->h1 = code;
						if (bezt->f3 & SELECT)
							bezt->h2 = code;
						if (bezt->h1 != bezt->h2) {
							if (ELEM(bezt->h1, HD_ALIGN, HD_AUTO))
								bezt->h1 = HD_FREE;
							if (ELEM(bezt->h2, HD_ALIGN, HD_AUTO))
								bezt->h2 = HD_FREE;
						}
					}
					bezt++;
				}
				BKE_nurb_handles_calc(nu);
			}
			nu = nu->next;
		}
	}
	else {
		char h_new = HD_FREE;

		/* there is 1 handle not FREE: FREE it all, else make ALIGNED  */
		if (code == 5) {
			h_new = HD_ALIGN;
		}
		else if (code == 6) {
			h_new = HD_FREE;
		}
		else {
			/* Toggle */
			for (nu = (Nurb*)editnurb->first; nu; nu = nu->next) {
				if (nu->type == CU_BEZIER) {
					bezt = nu->bezt;
					a = nu->pntsu;
					while (a--) {
						if (((bezt->f1 & SELECT) && bezt->h1 != HD_FREE) ||
						    ((bezt->f3 & SELECT) && bezt->h2 != HD_FREE))
						{
							h_new = HD_AUTO;
							break;
						}
						bezt++;
					}
				}
			}
			h_new = (h_new == HD_FREE) ? HD_ALIGN : HD_FREE;
		}
		for (nu = (Nurb*)editnurb->first; nu; nu = nu->next) {
			if (nu->type == CU_BEZIER) {
				bezt = nu->bezt;
				a = nu->pntsu;
				while (a--) {
					if (bezt->f1 & SELECT) bezt->h1 = h_new;
					if (bezt->f3 & SELECT) bezt->h2 = h_new;

					bezt++;
				}
				BKE_nurb_handles_calc(nu);
			}
		}
	}
}

void BKE_nurbList_handles_recalculate(ListBase *editnurb, const bool calc_length, const char flag)
{
	Nurb *nu;
	BezTriple *bezt;
	int a;

	for (nu = (Nurb*)editnurb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			bool changed = false;

			for (a = nu->pntsu, bezt = nu->bezt; a--; bezt++) {

				const bool h1_select = (bezt->f1 & flag) == flag;
				const bool h2_select = (bezt->f3 & flag) == flag;

				if (h1_select || h2_select) {

					/* Override handle types to HD_AUTO and recalculate */

					char h1_back, h2_back;
					float co1_back[3], co2_back[3];

					h1_back = bezt->h1;
					h2_back = bezt->h2;

					bezt->h1 = HD_AUTO;
					bezt->h2 = HD_AUTO;

					copy_v3_v3(co1_back, bezt->vec[0]);
					copy_v3_v3(co2_back, bezt->vec[2]);

					BKE_nurb_handle_calc_simple(nu, bezt);

					bezt->h1 = h1_back;
					bezt->h2 = h2_back;

					if (h1_select) {
						if (!calc_length) {
							dist_ensure_v3_v3fl(bezt->vec[0], bezt->vec[1], len_v3v3(co1_back, bezt->vec[1]));
						}
					}
					else {
						copy_v3_v3(bezt->vec[0], co1_back);
					}

					if (h2_select) {
						if (!calc_length) {
							dist_ensure_v3_v3fl(bezt->vec[2], bezt->vec[1], len_v3v3(co2_back, bezt->vec[1]));
						}
					}
					else {
						copy_v3_v3(bezt->vec[2], co2_back);
					}

					changed = true;
				}
			}

			if (changed) {
				/* Recalculate the whole curve */
				BKE_nurb_handles_calc(nu);
			}
		}
	}
}

void BKE_nurbList_flag_set(ListBase *editnurb, short flag)
{
	Nurb *nu;
	BezTriple *bezt;
	BPoint *bp;
	int a;

	for (nu = (Nurb*)editnurb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			a = nu->pntsu;
			bezt = nu->bezt;
			while (a--) {
				bezt->f1 = bezt->f2 = bezt->f3 = flag;
				bezt++;
			}
		}
		else {
			a = nu->pntsu * nu->pntsv;
			bp = nu->bp;
			while (a--) {
				bp->f1 = flag;
				bp++;
			}
		}
	}
}

void BKE_nurb_direction_switch(Nurb *nu)
{
	BezTriple *bezt1, *bezt2;
	BPoint *bp1, *bp2;
	float *fp1, *fp2, *tempf;
	int a, b;

	if (nu->pntsu == 1 && nu->pntsv == 1) {
		return;
	}

	if (nu->type == CU_BEZIER) {
		a = nu->pntsu;
		bezt1 = nu->bezt;
		bezt2 = bezt1 + (a - 1);
		if (a & 1) a += 1;  /* if odd, also swap middle content */
		a /= 2;
		while (a > 0) {
			if (bezt1 != bezt2) {
				SWAP(BezTriple, *bezt1, *bezt2);
			}

			swap_v3_v3(bezt1->vec[0], bezt1->vec[2]);

			if (bezt1 != bezt2) {
				swap_v3_v3(bezt2->vec[0], bezt2->vec[2]);
			}

			SWAP(char, bezt1->h1, bezt1->h2);
			SWAP(char, bezt1->f1, bezt1->f3);

			if (bezt1 != bezt2) {
				SWAP(char, bezt2->h1, bezt2->h2);
				SWAP(char, bezt2->f1, bezt2->f3);
				bezt1->alfa = -bezt1->alfa;
				bezt2->alfa = -bezt2->alfa;
			}
			else {
				bezt1->alfa = -bezt1->alfa;
			}
			a--;
			bezt1++;
			bezt2--;
		}
	}
	else if (nu->pntsv == 1) {
		a = nu->pntsu;
		bp1 = nu->bp;
		bp2 = bp1 + (a - 1);
		a /= 2;
		while (bp1 != bp2 && a > 0) {
			SWAP(BPoint, *bp1, *bp2);
			a--;
			bp1->alfa = -bp1->alfa;
			bp2->alfa = -bp2->alfa;
			bp1++;
			bp2--;
		}
		/* If there're odd number of points no need to touch coord of middle one,
		 * but still need to change it's tilt.
		 */
		if (nu->pntsu & 1) {
			bp1->alfa = -bp1->alfa;
		}
		if (nu->type == CU_NURBS) {
			/* no knots for too short paths */
			if (nu->knotsu) {
				/* inverse knots */
				a = KNOTSU(nu);
				fp1 = nu->knotsu;
				fp2 = fp1 + (a - 1);
				a /= 2;
				while (fp1 != fp2 && a > 0) {
					SWAP(float, *fp1, *fp2);
					a--;
					fp1++;
					fp2--;
				}
				/* and make in increasing order again */
				a = KNOTSU(nu);
				fp1 = nu->knotsu;
				fp2 = tempf = (float*)MEM_mallocN(sizeof(float) * a, "switchdirect");
				a--;
				fp2[a] = fp1[a];
				while (a--) {
					fp2[0] = fabsf(fp1[1] - fp1[0]);
					fp1++;
					fp2++;
				}

				a = KNOTSU(nu) - 1;
				fp1 = nu->knotsu;
				fp2 = tempf;
				fp1[0] = 0.0;
				fp1++;
				while (a--) {
					fp1[0] = fp1[-1] + fp2[0];
					fp1++;
					fp2++;
				}
				MEM_freeN(tempf);
			}
		}
	}
	else {
		for (b = 0; b < nu->pntsv; b++) {
			bp1 = nu->bp + b * nu->pntsu;
			a = nu->pntsu;
			bp2 = bp1 + (a - 1);
			a /= 2;

			while (bp1 != bp2 && a > 0) {
				SWAP(BPoint, *bp1, *bp2);
				a--;
				bp1++;
				bp2--;
			}
		}
	}
}


float (*BKE_curve_nurbs_vertexCos_get(ListBase *lb, int *r_numVerts))[3]
{
	int i, numVerts = *r_numVerts = BKE_nurbList_verts_count(lb);
	float *co;
	float (*cos)[3] = (float(*)[3])MEM_mallocN(sizeof(*cos) * numVerts, "cu_vcos");
	Nurb *nu;

	co = cos[0];
	for (nu = (Nurb*)lb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			BezTriple *bezt = nu->bezt;

			for (i = 0; i < nu->pntsu; i++, bezt++) {
				copy_v3_v3(co, bezt->vec[0]); co += 3;
				copy_v3_v3(co, bezt->vec[1]); co += 3;
				copy_v3_v3(co, bezt->vec[2]); co += 3;
			}
		}
		else {
			BPoint *bp = nu->bp;

			for (i = 0; i < nu->pntsu * nu->pntsv; i++, bp++) {
				copy_v3_v3(co, bp->vec); co += 3;
			}
		}
	}

	return cos;
}

void BK_curve_nurbs_vertexCos_apply(ListBase *lb, float (*vertexCos)[3])
{
	const float *co = vertexCos[0];
	Nurb *nu;
	int i;

	for (nu = (Nurb*)lb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			BezTriple *bezt = nu->bezt;

			for (i = 0; i < nu->pntsu; i++, bezt++) {
				copy_v3_v3(bezt->vec[0], co); co += 3;
				copy_v3_v3(bezt->vec[1], co); co += 3;
				copy_v3_v3(bezt->vec[2], co); co += 3;
			}
		}
		else {
			BPoint *bp = nu->bp;

			for (i = 0; i < nu->pntsu * nu->pntsv; i++, bp++) {
				copy_v3_v3(bp->vec, co); co += 3;
			}
		}

		calchandlesNurb_intern(nu, true);
	}
}

float (*BKE_curve_nurbs_keyVertexCos_get(ListBase *lb, float *key))[3]
{
	int i, numVerts = BKE_nurbList_verts_count(lb);
	float *co;
	float (*cos)[3] = (float(*)[3])MEM_mallocN(sizeof(*cos) * numVerts, "cu_vcos");
	Nurb *nu;

	co = cos[0];
	for (nu = (Nurb*)lb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			BezTriple *bezt = nu->bezt;

			for (i = 0; i < nu->pntsu; i++, bezt++) {
				copy_v3_v3(co, key); co += 3; key += 3;
				copy_v3_v3(co, key); co += 3; key += 3;
				copy_v3_v3(co, key); co += 3; key += 3;
				key += 3; /* skip tilt */
			}
		}
		else {
			BPoint *bp = nu->bp;

			for (i = 0; i < nu->pntsu * nu->pntsv; i++, bp++) {
				copy_v3_v3(co, key); co += 3; key += 3;
				key++; /* skip tilt */
			}
		}
	}

	return cos;
}

void BKE_curve_nurbs_keyVertexTilts_apply(ListBase *lb, float *key)
{
	Nurb *nu;
	int i;

	for (nu = (Nurb*)lb->first; nu; nu = nu->next) {
		if (nu->type == CU_BEZIER) {
			BezTriple *bezt = nu->bezt;

			for (i = 0; i < nu->pntsu; i++, bezt++) {
				key += 3 * 3;
				bezt->alfa = *key;
				key += 3;
			}
		}
		else {
			BPoint *bp = nu->bp;

			for (i = 0; i < nu->pntsu * nu->pntsv; i++, bp++) {
				key += 3;
				bp->alfa = *key;
				key++;
			}
		}
	}
}

bool BKE_nurb_check_valid_u(struct Nurb *nu)
{
	if (nu->pntsu <= 1)
		return false;
	if (nu->type != CU_NURBS)
		return true;           /* not a nurb, lets assume its valid */

	if (nu->pntsu < nu->orderu) return false;
	if (((nu->flag & CU_NURB_CYCLIC) == 0) && (nu->flagu & CU_NURB_BEZIER)) { /* Bezier U Endpoints */
		if (nu->orderu == 4) {
			if (nu->pntsu < 5)
				return false;  /* bezier with 4 orderu needs 5 points */
		}
		else {
			if (nu->orderu != 3)
				return false;  /* order must be 3 or 4 */
		}
	}
	return true;
}
bool BKE_nurb_check_valid_v(struct Nurb *nu)
{
	if (nu->pntsv <= 1)
		return false;
	if (nu->type != CU_NURBS)
		return true;           /* not a nurb, lets assume its valid */

	if (nu->pntsv < nu->orderv)
		return false;
	if (((nu->flag & CU_NURB_CYCLIC) == 0) && (nu->flagv & CU_NURB_BEZIER)) { /* Bezier V Endpoints */
		if (nu->orderv == 4) {
			if (nu->pntsv < 5)
				return false;  /* bezier with 4 orderu needs 5 points */
		}
		else {
			if (nu->orderv != 3)
				return false;  /* order must be 3 or 4 */
		}
	}
	return true;
}

bool BKE_nurb_check_valid_uv(struct Nurb *nu)
{
	if (!BKE_nurb_check_valid_u(nu))
		return false;
	if ((nu->pntsv > 1) && !BKE_nurb_check_valid_v(nu))
		return false;

	return true;
}

bool BKE_nurb_order_clamp_u(struct Nurb *nu)
{
	bool changed = false;
	if (nu->pntsu < nu->orderu) {
		nu->orderu = max_ii(2, nu->pntsu);
		changed = true;
	}
	if (nu->orderu > NURBS_MAX_ORDER) {
		nu->orderu = NURBS_MAX_ORDER;
		changed = true;
	}
	if (((nu->flagu & CU_NURB_CYCLIC) == 0) && (nu->flagu & CU_NURB_BEZIER)) {
		CLAMP(nu->orderu, 3, 4);
		changed = true;
	}
	return changed;
}

bool BKE_nurb_order_clamp_v(struct Nurb *nu)
{
	bool changed = false;
	if (nu->pntsv < nu->orderv) {
		nu->orderv = max_ii(2, nu->pntsv);
		changed = true;
	}
	if (nu->orderv > NURBS_MAX_ORDER) {
		nu->orderv = NURBS_MAX_ORDER;
		changed = true;
	}
	if (((nu->flagv & CU_NURB_CYCLIC) == 0) && (nu->flagv & CU_NURB_BEZIER)) {
		CLAMP(nu->orderv, 3, 4);
		changed = true;
	}
	return changed;
}

/**
 * \note caller must ensure active vertex remains valid.
 */
bool BKE_nurb_type_convert(Nurb *nu, const short type, const bool use_handles)
{
	BezTriple *bezt;
	BPoint *bp;
	int a, c, nr;

	if (nu->type == CU_POLY) {
		if (type == CU_BEZIER) {  /* to Bezier with vecthandles  */
			nr = nu->pntsu;
			bezt = (BezTriple *)MEM_callocN(nr * sizeof(BezTriple), "setsplinetype2");
			nu->bezt = bezt;
			a = nr;
			bp = nu->bp;
			while (a--) {
				copy_v3_v3(bezt->vec[1], bp->vec);
				bezt->f1 = bezt->f2 = bezt->f3 = bp->f1;
				bezt->h1 = bezt->h2 = HD_VECT;
				bezt->weight = bp->weight;
				bezt->radius = bp->radius;
				bp++;
				bezt++;
			}
			MEM_freeN(nu->bp);
			nu->bp = NULL;
			nu->pntsu = nr;
			nu->type = CU_BEZIER;
			BKE_nurb_handles_calc(nu);
		}
		else if (type == CU_NURBS) {
			nu->type = CU_NURBS;
			nu->orderu = 4;
			nu->flagu &= CU_NURB_CYCLIC; /* disable all flags except for cyclic */
			BKE_nurb_knot_calc_u(nu);
			a = nu->pntsu * nu->pntsv;
			bp = nu->bp;
			while (a--) {
				bp->vec[3] = 1.0;
				bp++;
			}
		}
	}
	else if (nu->type == CU_BEZIER) {   /* Bezier */
		if (type == CU_POLY || type == CU_NURBS) {
			nr = use_handles ? (3 * nu->pntsu) : nu->pntsu;
			nu->bp = (BPoint*)MEM_callocN(nr * sizeof(BPoint), "setsplinetype");
			a = nu->pntsu;
			bezt = nu->bezt;
			bp = nu->bp;
			while (a--) {
				if ((type == CU_POLY && bezt->h1 == HD_VECT && bezt->h2 == HD_VECT) || (use_handles == false)) {
					/* vector handle becomes 1 poly vertice */
					copy_v3_v3(bp->vec, bezt->vec[1]);
					bp->vec[3] = 1.0;
					bp->f1 = bezt->f2;
					if (use_handles) nr -= 2;
					bp->radius = bezt->radius;
					bp->weight = bezt->weight;
					bp++;
				}
				else {
					const char *f = &bezt->f1;
					for (c = 0; c < 3; c++, f++) {
						copy_v3_v3(bp->vec, bezt->vec[c]);
						bp->vec[3] = 1.0;
						bp->f1 = *f;
						bp->radius = bezt->radius;
						bp->weight = bezt->weight;
						bp++;
					}
				}
				bezt++;
			}
			MEM_freeN(nu->bezt);
			nu->bezt = NULL;
			nu->pntsu = nr;
			nu->pntsv = 1;
			nu->orderu = 4;
			nu->orderv = 1;
			nu->type = type;

#if 0       /* UNUSED */
			if (nu->flagu & CU_NURB_CYCLIC) c = nu->orderu - 1;
			else c = 0;
#endif

			if (type == CU_NURBS) {
				nu->flagu &= CU_NURB_CYCLIC; /* disable all flags except for cyclic */
				nu->flagu |= CU_NURB_BEZIER;
				BKE_nurb_knot_calc_u(nu);
			}
		}
	}
	else if (nu->type == CU_NURBS) {
		if (type == CU_POLY) {
			nu->type = CU_POLY;
			if (nu->knotsu) MEM_freeN(nu->knotsu);  /* python created nurbs have a knotsu of zero */
			nu->knotsu = NULL;
			if (nu->knotsv) MEM_freeN(nu->knotsv);
			nu->knotsv = NULL;
		}
		else if (type == CU_BEZIER) {     /* to Bezier */
			nr = nu->pntsu / 3;

			if (nr < 2) {
				return false;  /* conversion impossible */
			}
			else {
				bezt = (BezTriple*)MEM_callocN(nr * sizeof(BezTriple), "setsplinetype2");
				nu->bezt = bezt;
				a = nr;
				bp = nu->bp;
				while (a--) {
					copy_v3_v3(bezt->vec[0], bp->vec);
					bezt->f1 = bp->f1;
					bp++;
					copy_v3_v3(bezt->vec[1], bp->vec);
					bezt->f2 = bp->f1;
					bp++;
					copy_v3_v3(bezt->vec[2], bp->vec);
					bezt->f3 = bp->f1;
					bezt->radius = bp->radius;
					bezt->weight = bp->weight;
					bp++;
					bezt++;
				}
				MEM_freeN(nu->bp);
				nu->bp = NULL;
				MEM_freeN(nu->knotsu);
				nu->knotsu = NULL;
				nu->pntsu = nr;
				nu->type = CU_BEZIER;
			}
		}
	}

	return true;
}

/* Get edit nurbs or normal nurbs list */
ListBase *BKE_curve_nurbs_get(Curve *cu)
{
	if (cu->editnurb) {
		return BKE_curve_editNurbs_get(cu);
	}

	return &cu->nurb;
}

void BKE_curve_nurb_active_set(Curve *cu, Nurb *nu)
{
	if (nu == NULL) {
		cu->actnu = CU_ACT_NONE;
	}
	else {
		ListBase *nurbs = BKE_curve_editNurbs_get(cu);
		cu->actnu = BLI_findindex(nurbs, nu);
	}
}

Nurb *BKE_curve_nurb_active_get(Curve *cu)
{
	ListBase *nurbs = BKE_curve_editNurbs_get(cu);
	return (Nurb*)BLI_findlink(nurbs, cu->actnu);
}

/* Get active vert for curve */
void *BKE_curve_vert_active_get(Curve *cu)
{
	Nurb *nu = NULL;
	void *vert = NULL;

	BKE_curve_nurb_vert_active_get(cu, &nu, &vert);
	return vert;
}

/* Set active nurb and active vert for curve */
void BKE_curve_nurb_vert_active_set(Curve *cu, Nurb *nu, void *vert)
{
	if (nu) {
		BKE_curve_nurb_active_set(cu, nu);

		if (vert) {
			if (nu->type == CU_BEZIER) {
				BLI_assert(ARRAY_HAS_ITEM((BezTriple *)vert, nu->bezt, nu->pntsu));
				cu->actvert = (BezTriple *)vert - nu->bezt;
			}
			else {
				BLI_assert(ARRAY_HAS_ITEM((BPoint *)vert, nu->bp, nu->pntsu * nu->pntsv));
				cu->actvert = (BPoint *)vert - nu->bp;
			}
		}
		else {
			cu->actvert = CU_ACT_NONE;
		}
	}
	else {
		cu->actnu = cu->actvert = CU_ACT_NONE;
	}
}

/* Get points to active active nurb and active vert for curve */
bool BKE_curve_nurb_vert_active_get(Curve *cu, Nurb **r_nu, void **r_vert)
{
	Nurb *nu = NULL;
	void *vert = NULL;

	if (cu->actvert != CU_ACT_NONE) {
		ListBase *nurbs = BKE_curve_editNurbs_get(cu);
		nu = (Nurb*)BLI_findlink(nurbs, cu->actnu);

		if (nu) {
			if (nu->type == CU_BEZIER) {
				BLI_assert(nu->pntsu > cu->actvert);
				vert = &nu->bezt[cu->actvert];
			}
			else {
				BLI_assert((nu->pntsu * nu->pntsv) > cu->actvert);
				vert = &nu->bp[cu->actvert];
			}
		}
		/* get functions should never set! */
#if 0
		else {
			cu->actnu = cu->actvert = CU_ACT_NONE;
		}
#endif
	}

	*r_nu = nu;
	*r_vert = vert;

	return (*r_vert != NULL);
}

void BKE_curve_nurb_vert_active_validate(Curve *cu)
{
	Nurb *nu;
	void *vert;

	if (BKE_curve_nurb_vert_active_get(cu, &nu, &vert)) {
		if (nu->type == CU_BEZIER) {
			if ((((BezTriple *)vert)->f1 & SELECT) == 0) {
				cu->actvert = CU_ACT_NONE;
			}
		}
		else {
			if ((((BPoint *)vert)->f1 & SELECT) == 0) {
				cu->actvert = CU_ACT_NONE;
			}
		}
	}
}

/* basic vertex data functions */
bool BKE_curve_minmax(Curve *cu, bool use_radius, float min[3], float max[3])
{
	ListBase *nurb_lb = BKE_curve_nurbs_get(cu);
	Nurb *nu;

	for (nu = (Nurb*)nurb_lb->first; nu; nu = nu->next)
		BKE_nurb_minmax(nu, use_radius, min, max);

	return (BLI_listbase_is_empty(nurb_lb) == false);
}

/* Fills cent with the median of the control points. Does not alter cu. */
bool BKE_curve_center_median(Curve *cu, float cent[3])
{
	ListBase *nurb_lb = BKE_curve_nurbs_get(cu);
	Nurb *nu;
	int total = 0;

	zero_v3(cent);

	for (nu = (Nurb*)nurb_lb->first; nu; nu = nu->next) {
		int i;

		if (nu->type == CU_BEZIER) {
			BezTriple *bezt;
			i = nu->pntsu;
			total += i * 3;
			for (bezt = nu->bezt; i--; bezt++) {
				add_v3_v3(cent, bezt->vec[0]);
				add_v3_v3(cent, bezt->vec[1]);
				add_v3_v3(cent, bezt->vec[2]);
			}
		}
		else {
			BPoint *bp;
			i = nu->pntsu * nu->pntsv;
			total += i;
			for (bp = nu->bp; i--; bp++) {
				add_v3_v3(cent, bp->vec);
			}
		}
	}

	if (total) {
		mul_v3_fl(cent, 1.0f / (float)total);
	}

	return (total != 0);
}

/* Fills cent with the center of the AABB. Does not alter cu. */
bool BKE_curve_center_bounds(Curve *cu, float cent[3])
{
	float min[3], max[3];
	INIT_MINMAX(min, max);
	if (BKE_curve_minmax(cu, false, min, max)) {
		mid_v3_v3v3(cent, min, max);
		return true;
	}

	return false;
}

void BKE_curve_translate(Curve *cu, float offset[3], const bool do_keys)
{
	ListBase *nurb_lb = BKE_curve_nurbs_get(cu);
	Nurb *nu;
	int i;

	for (nu = (Nurb*)nurb_lb->first; nu; nu = nu->next) {
		BezTriple *bezt;
		BPoint *bp;

		if (nu->type == CU_BEZIER) {
			i = nu->pntsu;
			for (bezt = nu->bezt; i--; bezt++) {
				add_v3_v3(bezt->vec[0], offset);
				add_v3_v3(bezt->vec[1], offset);
				add_v3_v3(bezt->vec[2], offset);
			}
		}
		else {
			i = nu->pntsu * nu->pntsv;
			for (bp = nu->bp; i--; bp++) {
				add_v3_v3(bp->vec, offset);
			}
		}
	}

	if (do_keys && cu->key) {
		KeyBlock *kb;
		for (kb = (KeyBlock*)cu->key->block.first; kb; kb = kb->next) {
			float *fp = (float*)kb->data;
			for (i = kb->totelem; i--; fp += 3) {
				add_v3_v3(fp, offset);
			}
		}
	}
}

void BKE_curve_material_index_remove(Curve *cu, int index)
{
	const int curvetype = BKE_curve_type_get(cu);

	if (curvetype == OB_FONT) {
		struct CharInfo *info = cu->strinfo;
		int i;
		for (i = cu->len_wchar - 1; i >= 0; i--, info++) {
			if (info->mat_nr && info->mat_nr >= index) {
				info->mat_nr--;
			}
		}
	}
	else {
		Nurb *nu;

		for (nu = (Nurb*)cu->nurb.first; nu; nu = nu->next) {
			if (nu->mat_nr && nu->mat_nr >= index) {
				nu->mat_nr--;
				if (curvetype == OB_CURVE) {
					nu->charidx--;
				}
			}
		}
	}
}

void BKE_curve_material_index_clear(Curve *cu)
{
	const int curvetype = BKE_curve_type_get(cu);

	if (curvetype == OB_FONT) {
		struct CharInfo *info = cu->strinfo;
		int i;
		for (i = cu->len_wchar - 1; i >= 0; i--, info++) {
			info->mat_nr = 0;
		}
	}
	else {
		Nurb *nu;

		for (nu = (Nurb*)cu->nurb.first; nu; nu = nu->next) {
			nu->mat_nr = 0;
			if (curvetype == OB_CURVE) {
				nu->charidx = 0;
			}
		}
	}
}

int BKE_curve_material_index_validate(Curve *cu)
{
	const int curvetype = BKE_curve_type_get(cu);
	bool is_valid = true;

	if (curvetype == OB_FONT) {
		CharInfo *info = cu->strinfo;
		const int max_idx = max_ii(0, cu->totcol);  /* OB_FONT use 1 as first mat index, not 0!!! */
		int i;
		for (i = cu->len_wchar - 1; i >= 0; i--, info++) {
			if (info->mat_nr > max_idx) {
				info->mat_nr = 0;
				is_valid = false;
			}
		}
	}
	else {
		Nurb *nu;
		const int max_idx = max_ii(0, cu->totcol - 1);
		for (nu = (Nurb*)cu->nurb.first; nu; nu = nu->next) {
			if (nu->mat_nr > max_idx) {
				nu->mat_nr = 0;
				if (curvetype == OB_CURVE) {
					nu->charidx = 0;
				}
				is_valid = false;
			}
		}
	}

	if (!is_valid) {
		DAG_id_tag_update(&cu->id, OB_RECALC_DATA);
		return true;
	}
	else {
		return false;
	}
}

void BKE_curve_rect_from_textbox(const struct Curve *cu, const struct TextBox *tb, struct rctf *r_rect)
{
	r_rect->xmin = cu->xof + tb->x;
	r_rect->ymax = cu->yof + tb->y + cu->fsize;

	r_rect->xmax = r_rect->xmin + tb->w;
	r_rect->ymin = r_rect->ymax - tb->h;
}

void BKE_nurbs_uvbounds(struct Nurb *nu, float *umin, float *umax, float *vmin, float *vmax) {
	*umin = nu->knotsu[0];
	*umax = nu->knotsu[KNOTSU(nu)-1];
	if (nu->pntsv==1) { /* Curve, does not have v breakpoints */
		*vmin = *vmax = 0;
	} else { /* Surface, has full set of v breakpoints */
		*vmin = nu->knotsv[0];
		*vmax = nu->knotsv[KNOTSV(nu)-1];
	}
}

void BKE_nurbs_domain(struct Nurb *nu, float *umin, float *umax, float *vmin, float *vmax) {
	int pu = nu->orderu-1; /* p{u,v} is the degree of the curve in the {u,v} direction */
	int pv = nu->orderv-1;
	*umin = nu->knotsu[pu];
	*umax = nu->knotsu[KNOTSU(nu)-pu-1];
	if (nu->knotsv && nu->pntsv>1) {
		if (vmin) *vmin = nu->knotsv[pv];
		if (vmax) *vmax = nu->knotsv[KNOTSV(nu)-pv-1];
	} else {
		if (vmin) *vmin = 0;
		if (vmax) *vmax = 0;
	}
}

void BKE_nurbs_printknots(struct Nurb *nu) {
	int i,numknot;
	float umin, umax, vmin, vmax;
	printf("knotsu = {");
	for (i=0,numknot=KNOTSU(nu); i<numknot; i++) {
		printf((i==numknot-1)?"%3f}\n":"%3f, ",nu->knotsu[i]);
	}
	printf("knotsv = {");
	for (i=0,numknot=KNOTSV(nu); i<numknot; i++) {
		printf((i==numknot-1)?"%3f}\n":"%3f, ",nu->knotsv[i]);
	}
	BKE_nurbs_domain(nu, &umin, &umax, &vmin, &vmax);
	printf("domain = [%3f,%3f]x[%3f,%3f]\n",umin,umax,vmin,vmax);
	BKE_nurbs_uvbounds(nu, &umin, &umax, &vmin, &vmax);
	printf("uvbounds = [%3f,%3f]x[%3f,%3f]\n",umin,umax,vmin,vmax);
}

GridMesh *BKE_nurb_compute_trimmed_GridMesh(struct Nurb* nu) {
	// Figure out the domain
	float ustart,uend,vstart,vend;
	BKE_nurbs_domain(nu,&ustart,&uend,&vstart,&vend);
	int totu_knot = round(uend-ustart)*nu->resolu;
	int totv_knot = round(vend-vstart)*nu->resolv;
	int totu_geom = (nu->pntsu-1)*nu->resolu;
	int totv_geom = (nu->pntsv-1)*nu->resolv;
	int totu=totu_knot, totv=totv_knot;
	if (totu<2 || totv<2 || totu_knot>totu_geom || totv_knot>totv_geom) {
		printf("Nonsensical NURBS tessellation level. Using geometric fallback.\n");
		totu = totu_geom;
		totv = totv_geom;
	}
	
	// Trim the uniform grid in 2D UV space
	GridMesh *gm = new GridMesh();
	gm->set_ll_ur(ustart,vstart,uend,vend);
	gm->init_grid(totu,totv);
	
	// Trim
//	gm->begin_recording();
	for (NurbTrim *nt=(NurbTrim*)nu->trims.first; nt; nt=nt->next) {
		float (*trim_uv_pts)[2];
		int num_trimpts = BKE_nurbTrim_tess(nt, &trim_uv_pts);
		int trim_poly = gm->poly_new((float*)trim_uv_pts, num_trimpts*2);
		switch (nt->type) {
			case CU_TRIM_AND:
				gm->bool_AND(trim_poly);
				break;
			case CU_TRIM_SUB:
				gm->bool_SUB(trim_poly);
				break;
			default:
				fprintf(stderr,"WARNING: invalid trim type %i!\n",nt->type);
				gm->bool_SUB(trim_poly);
		}
		MEM_freeN(trim_uv_pts);
	}
//	static ThreadMutex *mutex = NULL;
//	if (!mutex) {
//		mutex = (ThreadMutex*)MEM_callocN(sizeof(ThreadMutex), "ThreadMutex");
//		BLI_mutex_init(mutex);
//	}
//	BLI_mutex_lock(mutex);
//	gm->dump_recording();
//	BLI_mutex_unlock(mutex);
	return gm;
}

void BKE_nurb_compute_trimmed_UV_mesh(struct Nurb* nu) {
	GridMesh *gm = BKE_nurb_compute_trimmed_GridMesh(nu);
	
	// Extract the results
	int totu=gm->nx, totv=gm->ny;
	std::map<int,int> *used_idxs;
	GridMeshCoord *coords = gm->coords;
	float *final_coords;
	int coords_len = (totu+1)*(totv+1)*2;
	used_idxs = new std::map<int,int>();
	int idxs_len = 4 * 3*sizeof(int)*totu*totv;
	int *idxs = (int*)MEM_mallocN(sizeof(int)*idxs_len, "NURBS_tess_2.0");
	int ii=0; // Index index
#define TESS_MAX_POLY_VERTS 2048
	float coords_tmp[TESS_MAX_POLY_VERTS][2];
	int reindex_tmp[TESS_MAX_POLY_VERTS];
	unsigned idx_tmp[TESS_MAX_POLY_VERTS][3];
	std::vector<int> degenerate_polys;

	// Loop through every cell, triangulate its polys, push them onto idxs
	for (int j=0; j<totv; j++) {
		for (int i=0; i<totu; i++) {
			int cell = gm->poly_for_cell(i, j);
			GridMeshVert *v = &gm->v[0];
			
			for (int poly=cell; poly; poly=v[poly].next_poly) {
				if (!v[poly].next) {
					continue;
				}
				if (v[poly].is_pristine) {
					int ll_gp = gm->gridpt_for_cell(i, j);
					if (ii+6>=idxs_len) {
						//printf("1Growing idxs %i to %i\n",idxs_len,idxs_len*2);
						idxs_len *= 2;
						idxs = (int*)MEM_reallocN_id(idxs, sizeof(int)*idxs_len, "NURBS_tess_2.1");
					}
					int ll=ll_gp, lr=ll_gp+1, ur=ll_gp+(totu+1)+1, ul=ll_gp+(totu+1);
					idxs[ii++]=ll; idxs[ii++]=lr; idxs[ii++]=ur;
					idxs[ii++]=ur; idxs[ii++]=ul; idxs[ii++]=ll;
					(*used_idxs)[ll]=0; (*used_idxs)[lr]=0;
					(*used_idxs)[ul]=0; (*used_idxs)[ur]=0;
				} else {
					int num_verts=0; int vert=poly;
					do {
						int idx = v[vert].coord_idx;
						GridMeshCoord& crd = coords[idx];
						coords_tmp[num_verts][0] = crd.x;
						coords_tmp[num_verts][1] = crd.y;
						reindex_tmp[num_verts] = idx;
						num_verts++;
						vert=v[vert].next;
						if (num_verts>=TESS_MAX_POLY_VERTS) {
							printf("WARNING: maximum tessellation polygon verts (%i) exceeded. %i<-%i->%i\n",TESS_MAX_POLY_VERTS,v[vert].prev,vert,v[vert].next);
							break;
						}
					} while (vert!=poly);
					BLI_polyfill_calc(coords_tmp,
									  num_verts,
									  0, // tell polyfill to do concave check
									  idx_tmp);
					if (ii+(num_verts-2)*3>=idxs_len) {
						int new_idxlen = idxs_len*2 + (num_verts-2)*3;
						//printf("2Growing idxs %i to %i\n",idxs_len,new_idxlen);
						idxs = (int*)MEM_reallocN_id(idxs, sizeof(int)*new_idxlen, "NURBS_tess_2.2");
						idxs_len = new_idxlen;
					}
					for (int z=0; z<num_verts-2; z++) {
						int i1=reindex_tmp[idx_tmp[z][0]];
						int i2=reindex_tmp[idx_tmp[z][1]];
						int i3=reindex_tmp[idx_tmp[z][2]];
						idxs[ii++]=i1; idxs[ii++]=i2; idxs[ii++]=i3;
						(*used_idxs)[i1]=0;
						(*used_idxs)[i2]=0;
						(*used_idxs)[i3]=0;
					}
				}
			}
		}
	}
	/* Remap Coords */
	int num_used_idxs = int(used_idxs->size());
	final_coords = (float*)MEM_mallocN(2*sizeof(float)*num_used_idxs, "NURBS_tess_2.3");
	coords_len = num_used_idxs;
	int newidx=0;
	std::map<int,int>::iterator it=used_idxs->begin(),end=used_idxs->end();
	for (; it!=end; it++) {
		int old_idx = it->first;
		final_coords[2*newidx+0] = gm->coords[old_idx].x;
		final_coords[2*newidx+1] = gm->coords[old_idx].y;
		it->second = newidx;
		newidx += 1;
	}
	// ii, the index index, points to the end of idxs, the index array
	for (int i=0; i<ii; i++) {
		idxs[i] = (*used_idxs)[idxs[i]];
	}
	/* End remap coords */
	
	nu->UV_verts_count = coords_len;
	nu->UV_tri_count = ii/3;
	if (nu->UV_verts) MEM_freeN(nu->UV_verts);
	nu->UV_verts = final_coords;
	if (nu->UV_idxs) MEM_freeN(nu->UV_idxs);
	nu->UV_idxs = idxs;
	
	delete gm;
}

void BKE_nurbs_cached_UV_mesh_clear(struct Nurb* nu, bool free_mem) {
	nu->UV_verts_count = nu->UV_tri_count = 0;
	if (free_mem) {
		if (nu->UV_verts) MEM_freeN(nu->UV_verts);
		if (nu->UV_idxs) MEM_freeN(nu->UV_idxs);
	}
	nu->UV_verts = NULL;
	nu->UV_idxs = NULL;
}

void BKE_nurb_make_displist(struct Nurb *nu, struct DispList *dl) {
	if (!nu->UV_verts) BKE_nurb_compute_trimmed_UV_mesh(nu);
	
	float (*coords)[3] = (float(*)[3])MEM_mallocN(sizeof(*coords)*nu->UV_verts_count, "NURBS_tess_3dpts_1");
	float (*nors)[3] = (float(*)[3])MEM_mallocN(sizeof(*nors)*nu->UV_verts_count, "NURBS_tess_3dpts_2");
	int (*idxs)[3] = (int(*)[3])MEM_mallocN(sizeof(*idxs)*nu->UV_tri_count, "NURBS_tess_3dpts_3");
	memcpy(idxs, nu->UV_idxs, sizeof(*idxs)*nu->UV_tri_count);
	
	// Push trimmed UV mesh forward through the NURBS map
	BSplineCacheU cacheU; // Make repeated evals at same u coord efficient
	cacheU.u = 1.0/0; // First eval should always miss
	for (int coord=0; coord<nu->UV_verts_count; coord++) {
		float *uv_in = (float*)&nu->UV_verts[2*coord];
		float *xyz_out = (float*)&coords[coord];
		float *norm_out = (float*)&nors[coord];
		BPoint out[3]; // { surf_pt, u_partial_deriv, v_partal_deriv }
		BKE_nurbs_surf_eval(uv_in[0], uv_in[1],
							nu->pntsu, nu->orderu, nu->knotsu,
							nu->pntsv, nu->orderv, nu->knotsv,
							nu->bp, 1, out, &cacheU);
		copy_v3_v3(xyz_out, out[0].vec);
		cross_v3_v3v3(norm_out, out[1].vec, out[2].vec);
	}
	
	dl->col = nu->mat_nr;
	dl->charidx = nu->charidx;
	dl->rt = nu->flag & ~CU_2D;
	dl->verts = (float*)coords;
	dl->index = (int*)idxs;
	dl->nors = (float*)nors;
	dl->type = DL_INDEX3;
	dl->parts = nu->UV_tri_count;
	dl->nr = nu->UV_verts_count;
	if (nu->flag&CU_SMOOTH)
		dl->rt |= CU_SMOOTH;
}

/* Helper for BKE_surf_to_mesh */
struct NurbsMeshInfo {
	/* Output from Pass 1 */
	int num_vert, num_edge, num_loop, num_poly;
	GridMesh *gm;
	std::map<int,int> remap_vert;
	std::map<std::pair<int,int>,int> remap_edge;
	
	/* Input to Pass 2 */
	int base_loopid;
	MVert *verts;
	MEdge *edges;
	MLoop *loops;
	MLoopUV *uvs;
	MPoly *polys;
};

/* Helper for BKE_surf_to_mesh */
static void nurbs_meshinfo_pass_1(Nurb *nu, NurbsMeshInfo &nmi) {
	GridMesh *gm = BKE_nurb_compute_trimmed_GridMesh(nu);
	nmi.gm = gm;
	std::map<int,int> &remap_vert = nmi.remap_vert;
	std::map<std::pair<int,int>,int> &remap_edge = nmi.remap_edge;
	int vertid=0, edgeid=0, loopid=0, polyid=0;
	for (GridMeshIterator gmi=gm->begin(); !gmi.done(); gmi.next()) {
		int poly = gmi.poly;
		int vert=poly; do {
			int coordidx = gm->v[vert].coord_idx;
			std::map<int,int>::iterator crd = remap_vert.find(coordidx);
			if (crd==remap_vert.end()) {
				remap_vert[coordidx] = vertid++;
			}
			int nextvert = gm->v[vert].next;
			int c1=coordidx, c2=gm->v[nextvert].coord_idx;
			std::pair<int,int> thisedge = (c1<c2)? std::make_pair(c1,c2) : std::make_pair(c2,c1);
			std::map<std::pair<int,int>,int>::iterator edg = remap_edge.find(thisedge);
			if (edg==remap_edge.end()) {
				remap_edge[thisedge] = edgeid++;
			}
			loopid++;
			vert = nextvert;
		} while (vert!=poly);
		polyid++;
	}
	nmi.num_vert = vertid;
	nmi.num_edge = edgeid;
	nmi.num_loop = loopid;
	nmi.num_poly = polyid;
}

/* Helper for BKE_surf_to_mesh */
static void nurbs_meshinfo_pass_2(Nurb *nu, NurbsMeshInfo &nmi) {
	/* Cache some data we'll need for surface eval */
	int pntsu=nu->pntsu, orderu=nu->orderu, pntsv=nu->pntsv, orderv=nu->orderv;
	float *U=nu->knotsu, *V=nu->knotsv;
	BPoint *bp=nu->bp;

	/* Fill the newly allocated storage */
	int loopnum=0, polynum=0;
	for (GridMeshIterator gmi=nmi.gm->begin(); !gmi.done(); gmi.next()) {
		int poly = gmi.poly;
		int vert=poly;
		GridMeshVert *gmvert = &nmi.gm->v[vert];
		int coordidx = gmvert->coord_idx;
		int remapped_vert_idx = nmi.remap_vert[coordidx];
		int num_edge_in_poly=0, first_loop_of_poly=loopnum;
		/* Loop over each edge {vert,nextvert} in gmi.poly */
		do {
			num_edge_in_poly++;
			int nextvert = gmvert->next;
			GridMeshVert *gmnextvert = &nmi.gm->v[nextvert];
			int nextcoordidx = gmnextvert->coord_idx;
			int remapped_nextvert_idx = nmi.remap_vert[nextcoordidx];

			/* Get pointers to the structures at this poly edge */
			MVert *mesh_v = nmi.verts + remapped_vert_idx;
			GridMeshCoord *gmcoord = &nmi.gm->coords[coordidx];
			int c1=coordidx, c2=nextcoordidx;
			std::pair<int,int> thisedge = (c1<c2)? std::make_pair(c1,c2) : std::make_pair(c2,c1);
			int remapped_edge_idx = nmi.remap_edge[thisedge];
			MEdge *mesh_e = nmi.edges + remapped_edge_idx;
			MLoopUV *mesh_luv = nmi.uvs + (loopnum);
			MLoop *mesh_l = nmi.loops + (loopnum++);

			/* MVert (location, normal) */
			BPoint surfpt[3]; // Coord, d/du, d/dv. The latter two are actually vectors.
			BKE_nurbs_surf_eval(gmcoord->x, gmcoord->y,
								pntsu,orderu,U, pntsv,orderv,V,
								bp, 1, surfpt);
			copy_v3_v3(mesh_v->co, surfpt[0].vec);
			float norm[3];
			cross_v3_v3v3(norm, surfpt[1].vec, surfpt[2].vec);
			normalize_v3(norm);
			normal_float_to_short_v3(mesh_v->no, norm);

			/* MEdge, MLoop, MLoopUV */
			mesh_e->v1 = remapped_vert_idx;
			mesh_e->v2 = remapped_nextvert_idx;
			mesh_l->v = remapped_vert_idx;
			mesh_l->e = remapped_edge_idx;
			mesh_luv->uv[0] = gmcoord->x;
			mesh_luv->uv[1] = gmcoord->y;

			vert = nextvert;
			gmvert = gmnextvert;
			remapped_vert_idx = remapped_nextvert_idx;
			coordidx = gmvert->coord_idx;
		} while (vert!=poly);
		MPoly *mesh_p = nmi.polys + (polynum++);
		mesh_p->loopstart = nmi.base_loopid + first_loop_of_poly;
		mesh_p->totloop = num_edge_in_poly;
		if (nu->flag&ME_SMOOTH) mesh_p->flag |= ME_SMOOTH;
	}
}

/* Transforms the surface object ob into a mesh representation. */
void BKE_surf_to_mesh(Object *ob) {
	BLI_assert(ob->type == OB_SURF);
	Curve *cu = (Curve*)ob->data;
	BLI_assert(cu->type == OB_SURF);

	/* Pass 0: Count # of surfaces in the Curve object */
	int num_nurbs=0;
	for (Nurb *nu = (Nurb*)cu->nurb.first; nu; nu=nu->next) num_nurbs++;
	std::vector<NurbsMeshInfo> NMIs(num_nurbs);

	/* Pass 1, for each NURBS surface,
	 *   a. invokes the GridMesh surface tessellator
	 *   b. counts the number of verts, edges, loops, and polys outputted by #1
	 *   c. computes de-duplicated and compacted indices for verts and edges
	 *   d. stores the output in a NurbsMeshInfo object for subsequent passes
	 *   e. keeps track of totals because all surfs in the new mesh share vert/edge/etc buffers
	 */
	int totvert=0, totedge=0, totloop=0, totpoly=0;
	Nurb *nu = (Nurb*)cu->nurb.first;
	for (int i=0; i<num_nurbs; i++) {
		NurbsMeshInfo &nmi = NMIs[i];

		nurbs_meshinfo_pass_1(nu, nmi);

		totvert += nmi.num_vert;
		totloop += nmi.num_loop;
		totedge += nmi.num_edge;
		totpoly += nmi.num_poly;
		nu=nu->next;
	}
	
	/* Allocate mesh storage now that we know total counts */
	MVert *allvert = (MVert*)MEM_callocN(sizeof(MVert)*totvert+4, "NURBS2mesh_verts");
	MEdge *alledge = (MEdge*)MEM_callocN(sizeof(MEdge)*totedge+4, "NURBS2mesh_edges");
	MLoop *allloop = (MLoop*)MEM_callocN(sizeof(MLoop)*totloop+4, "NURBS2mesh_loop");
	MPoly *allpoly = (MPoly*)MEM_callocN(sizeof(MPoly)*totpoly+4, "NURBS2mesh_poly");
	MLoopUV *alluv = (MLoopUV*)MEM_callocN(sizeof(MLoopUV)*totloop+4, "NURBS2mesh_uv");

	/* Pass 2 fills the newly allocated buffers */
	nu = (Nurb*)cu->nurb.first;
	int filled_v=0, filled_e=0, filled_l=0, filled_p=0;
	int base_loopid=0;
	for (int i=0; i<num_nurbs; i++) {
		NurbsMeshInfo &nmi = NMIs[i];
		nmi.verts = allvert+filled_v;
		nmi.edges = alledge+filled_e;
		nmi.loops = allloop+filled_l;
		nmi.polys = allpoly+filled_p;
		nmi.uvs = alluv+filled_l;
		nmi.base_loopid = base_loopid;
	
		nurbs_meshinfo_pass_2(nu, nmi);
	
		filled_v += nmi.num_vert;
		filled_e += nmi.num_edge;
		filled_l += nmi.num_loop;
		filled_p += nmi.num_poly;
		base_loopid += nmi.num_loop;
		nu=nu->next;
	}

	/* Put all the mesh data together in a Mesh object */
	Mesh *me = BKE_mesh_add(G.main, "Mesh");
	me->totvert=totvert;
	me->totedge=totedge;
	me->totloop=totloop;
	me->totpoly=totpoly;
	me->mvert = (MVert*)CustomData_add_layer(&me->vdata, CD_MVERT, CD_ASSIGN, allvert, me->totvert);
	me->medge = (MEdge*)CustomData_add_layer(&me->edata, CD_MEDGE, CD_ASSIGN, alledge, me->totedge);
	me->mloop = (MLoop*)CustomData_add_layer(&me->ldata, CD_MLOOP, CD_ASSIGN, allloop, me->totloop);
	me->mpoly = (MPoly*)CustomData_add_layer(&me->pdata, CD_MPOLY, CD_ASSIGN, allpoly, me->totpoly);
	me->mtpoly = (MTexPoly*)CustomData_add_layer_named(&me->pdata, CD_MTEXPOLY, CD_DEFAULT, NULL, me->totpoly, "Orco");
	me->mloopuv = (MLoopUV*)CustomData_add_layer_named(&me->ldata, CD_MLOOPUV, CD_ASSIGN, alluv, me->totloop, "Orco");

	/* Switch out the Curve datablock for a Mesh datablock */
	BKE_mesh_texspace_calc(me);
	me->mat = cu->mat; cu->mat = NULL;
	if (ob->data) {
		BKE_libblock_free(G.main, ob->data);
	}
	ob->data = me;
	ob->type = OB_MESH;
	BKE_object_free_curve_cache(ob);
	/* other users */
	Object *ob1 = (Object*)G.main->object.first;
	while (ob1) {
		if (ob1->data == cu) {
			ob1->type = OB_MESH;
			
			ob1->data = ob->data;
			id_us_plus((ID *)ob->data);
		}
		ob1 = (Object*)ob1->id.next;
	}
}
