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
#include "BKE_texture.h"
#include "BKE_colortools.h"

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
		const float min = FLT_EPSILON * 100.0f;
		j = 3;
		while (j--) {
			if (r_size[j] <= FLT_EPSILON) {
				r_size[j] = min;
			}
		}
	}

	if (ob && ob_center) {
		/* Translate our coordinates so that center of obj_center is at (0, 0, 0). */
		float mat[4][4];

		/* Get ob_center coordinates in ob local coordinates. */
		invert_m4_m4(mat, ob_center->obmat);
		mul_m4_m4m4(mat, mat, ob->obmat);
		copy_v3_v3(diff, mat[3]);

		print_v3("ob center", ob->obmat[3]);
		print_v3("ob_center center", ob_center->obmat[3]);
		print_v3("ob_center diff", diff);

		do_diff = true;
	}
	else if (use_bbox_center) {
		/* Translate our coordinates so that center of bounding box is at (0, 0, 0). */

		/* Compute bbox center in local coordinates. */
		add_v3_v3v3(diff, min_co, max_co);
		mul_v3_fl(diff, -0.5f);

		print_v3("bbox diff", diff);

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

static bool affect_vertex(MDeformVert *dvert, const int index, const int defgrp_index) {
	if (!dvert || defgrp_index == -1) {
		return true;
	}

	return (defvert_find_weight(&dvert[index], defgrp_index) > 0.0f);
}

static void setSplitNormalModifier_do(SetSplitNormalModifierData *smd, Object *ob, DerivedMesh *dm)
{
	MLoop *mloop;
	float (*clnors)[2];
	const float autosmooth_angle = (ob->type == OB_MESH) ? ((Mesh *)ob->data)->smoothresh : (float)M_PI;
	int num_verts = dm->getNumVerts(dm);
	int num_loops = dm->getNumLoops(dm);
	int i;

	bool use_bbox_center = ((smd->flags & MOD_SETSPLITNORMAL_CENTER_BBOX) != 0) && (smd->object_center == NULL);

	int defgrp_index;
	MDeformVert *dvert;

	mloop = dm->getLoopArray(dm);

	clnors = dm->getLoopDataArray(dm, CD_CUSTOMLOOPNORMAL);
	if (!clnors) {
		DM_add_loop_layer(dm, CD_CUSTOMLOOPNORMAL, CD_CALLOC, NULL);
		clnors = dm->getLoopDataArray(dm, CD_CUSTOMLOOPNORMAL);
	}

	modifier_get_vgroup(ob, dm, smd->defgrp_name, &dvert, &defgrp_index);

	if (smd->mode == MOD_SETSPLITNORMAL_BOX) {
		float (*cos)[3] = MEM_mallocN(sizeof(*cos) * num_verts, __func__);
		float size[3];

		generate_vert_coordinates(dm, ob, smd->object_center, use_bbox_center, num_verts, cos, size);

		MEM_SAFE_FREE(cos);
	}
	else if (smd->mode == MOD_SETSPLITNORMAL_ELLIPSOID) {
		float (*cos)[3] = MEM_mallocN(sizeof(*cos) * num_verts, __func__);
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
				if (affect_vertex(dvert, i, defgrp_index)) {
					float *co = cos[i];
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
					/* NULL value is NOP when defining custom normals... */
					zero_v3(cos[i]);
				}
			}
		}

		{
			/* And now, apply those normals to lnors! */
			MLoopsNorSpaces lnors_spaces = {NULL};
			dm->calcLoopNormalsSpaces(dm, autosmooth_angle, &lnors_spaces);

			i = num_loops;
			while (i--) {
				BKE_lnor_space_custom_normal_to_data(lnors_spaces.lspaces[i], cos[mloop[i].v], clnors[i]);
			}

			BKE_free_loops_normal_spaces(&lnors_spaces);
		}

		MEM_SAFE_FREE(cos);
	}
	else if (smd->object_geometry) {
	}

#if 0



	float obinv[4][4];
	float mat_from[4][4];
	float mat_from_inv[4][4];
	float mat_to[4][4];
	float mat_unit[4][4];
	float mat_final[4][4];

	float tmat[4][4];

	float strength = wmd->strength;
	float fac = 1.0f, weight;
	int i;
	int defgrp_index;
	MDeformVert *dvert, *dv = NULL;

	float (*tex_co)[3] = NULL;

	if (!(wmd->object_from && wmd->object_to))
		return;

	modifier_get_vgroup(ob, dm, wmd->defgrp_name, &dvert, &defgrp_index);

	if (wmd->curfalloff == NULL) /* should never happen, but bad lib linking could cause it */
		wmd->curfalloff = curvemapping_add(1, 0.0f, 0.0f, 1.0f, 1.0f);

	if (wmd->curfalloff) {
		curvemapping_initialize(wmd->curfalloff);
	}

	invert_m4_m4(obinv, ob->obmat);

	mul_m4_m4m4(mat_from, obinv, wmd->object_from->obmat);
	mul_m4_m4m4(mat_to, obinv, wmd->object_to->obmat);

	invert_m4_m4(tmat, mat_from); // swap?
	mul_m4_m4m4(mat_final, tmat, mat_to);

	invert_m4_m4(mat_from_inv, mat_from);

	unit_m4(mat_unit);

	if (strength < 0.0f) {
		float loc[3];
		strength = -strength;

		/* inverted location is not useful, just use the negative */
		copy_v3_v3(loc, mat_final[3]);
		invert_m4(mat_final);
		negate_v3_v3(mat_final[3], loc);

	}
	weight = strength;

	if (wmd->texture) {
		tex_co = MEM_mallocN(sizeof(*tex_co) * numVerts, "warpModifier_do tex_co");
		get_texture_coords((MappingInfoModifierData *)wmd, ob, dm, vertexCos, tex_co, numVerts);

		modifier_init_texture(wmd->modifier.scene, wmd->texture);
	}

	for (i = 0; i < numVerts; i++) {
		float *co = vertexCos[i];

		if (wmd->falloff_type == eWarp_Falloff_None ||
		    ((fac = len_v3v3(co, mat_from[3])) < wmd->falloff_radius &&
		     (fac = (wmd->falloff_radius - fac) / wmd->falloff_radius)))
		{
			/* skip if no vert group found */
			if (dvert && defgrp_index != -1) {
				dv = &dvert[i];

				if (dv) {
					weight = defvert_find_weight(dv, defgrp_index) * strength;
					if (weight <= 0.0f) /* Should never occure... */
						continue;
				}
			}


			/* closely match PROP_SMOOTH and similar */
			switch (wmd->falloff_type) {
				case eWarp_Falloff_None:
					fac = 1.0f;
					break;
				case eWarp_Falloff_Curve:
					fac = curvemapping_evaluateF(wmd->curfalloff, 0, fac);
					break;
				case eWarp_Falloff_Sharp:
					fac = fac * fac;
					break;
				case eWarp_Falloff_Smooth:
					fac = 3.0f * fac * fac - 2.0f * fac * fac * fac;
					break;
				case eWarp_Falloff_Root:
					fac = (float)sqrt(fac);
					break;
				case eWarp_Falloff_Linear:
					/* pass */
					break;
				case eWarp_Falloff_Const:
					fac = 1.0f;
					break;
				case eWarp_Falloff_Sphere:
					fac = (float)sqrt(2 * fac - fac * fac);
					break;
			}

			fac *= weight;

			if (tex_co) {
				TexResult texres;
				texres.nor = NULL;
				BKE_texture_get_value(wmd->modifier.scene, wmd->texture, tex_co[i], &texres, false);
				fac *= texres.tin;
			}

			/* into the 'from' objects space */
			mul_m4_v3(mat_from_inv, co);

			if (fac >= 1.0f) {
				mul_m4_v3(mat_final, co);
			}
			else if (fac > 0.0f) {
				if (wmd->flag & MOD_WARP_VOLUME_PRESERVE) {
					/* interpolate the matrix for nicer locations */
					blend_m4_m4m4(tmat, mat_unit, mat_final, fac);
					mul_m4_v3(tmat, co);
				}
				else {
					float tvec[3];
					mul_v3_m4v3(tvec, mat_final, co);
					interp_v3_v3v3(co, co, tvec, fac);
				}
			}

			/* out of the 'from' objects space */
			mul_m4_v3(mat_from, co);
		}
	}

	if (tex_co)
		MEM_freeN(tex_co);
#endif
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
