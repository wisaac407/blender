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
 * The Original Code is Copyright (C) 2006-2007 Blender Foundation.
 * All rights reserved.
 *
 * The Original Code is: all of this file.
 *
 * Contributor(s): none yet.
 *
 * ***** END GPL LICENSE BLOCK *****
 *
 */

/** \file blender/blenkernel/intern/icons.c
 *  \ingroup bke
 */


#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "MEM_guardedalloc.h"

#include "DNA_lamp_types.h"
#include "DNA_material_types.h"
#include "DNA_texture_types.h"
#include "DNA_world_types.h"
#include "DNA_brush_types.h"

#include "BLI_utildefines.h"
#include "BLI_ghash.h"
#include "BLI_string.h"

#include "BKE_icons.h"
#include "BKE_global.h" /* only for G.background test */

#include "BLI_sys_types.h" // for intptr_t support

#include "GPU_extensions.h"

#include "IMB_imbuf.h"
#include "IMB_imbuf_types.h"
#include "IMB_thumbs.h"

/* GLOBALS */

static GHash *gIcons = NULL;

static int gNextIconId = 1;

static int gFirstIconId = 1;

static GHash *gCachedPreviews = NULL;

static void icon_free(void *val)
{
	Icon *icon = val;

	if (icon) {
		if (icon->drawinfo_free) {
			icon->drawinfo_free(icon->drawinfo);
		}
		else if (icon->drawinfo) {
			MEM_freeN(icon->drawinfo);
		}
		MEM_freeN(icon);
	}
}

/* create an id for a new icon and make sure that ids from deleted icons get reused
 * after the integer number range is used up */
static int get_next_free_id(void)
{
	int startId = gFirstIconId;

	/* if we haven't used up the int number range, we just return the next int */
	if (gNextIconId >= gFirstIconId)
		return gNextIconId++;
	
	/* now we try to find the smallest icon id not stored in the gIcons hash */
	while (BLI_ghash_lookup(gIcons, SET_INT_IN_POINTER(startId)) && startId >= gFirstIconId)
		startId++;

	/* if we found a suitable one that isn't used yet, return it */
	if (startId >= gFirstIconId)
		return startId;

	/* fail */
	return 0;
}

void BKE_icons_init(int first_dyn_id)
{
	gNextIconId = first_dyn_id;
	gFirstIconId = first_dyn_id;

	if (!gIcons)
		gIcons = BLI_ghash_int_new(__func__);

	if (!gCachedPreviews) {
		gCachedPreviews = BLI_ghash_str_new(__func__);
	}
}

void BKE_icons_free(void)
{
	if (gIcons) {
		BLI_ghash_free(gIcons, NULL, icon_free);
		gIcons = NULL;
	}

	if (gCachedPreviews) {
		BLI_ghash_free(gCachedPreviews, MEM_freeN, BKE_previewimg_freefunc);
		gCachedPreviews = NULL;
	}
}

PreviewImage *BKE_previewimg_create(void)
{
	PreviewImage *prv_img = NULL;
	int i;

	prv_img = MEM_callocN(sizeof(PreviewImage), "img_prv");

	for (i = 0; i < NUM_ICON_SIZES; ++i) {
		prv_img->flag[i] |= CHANGED;
		prv_img->changed_timestamp[i] = 0;
	}
	return prv_img;
}

void BKE_previewimg_freefunc(void *link)
{
	PreviewImage *prv = (PreviewImage *)link;
	if (prv) {
		int i;

		for (i = 0; i < NUM_ICON_SIZES; ++i) {
			if (prv->rect[i]) {
				MEM_freeN(prv->rect[i]);
				prv->rect[i] = NULL;
			}
			if (prv->gputexture[i])
				GPU_texture_free(prv->gputexture[i]);
		}
		
		MEM_freeN(prv);
	}
}

void BKE_previewimg_free(PreviewImage **prv)
{
	if (prv && (*prv)) {
		BKE_previewimg_freefunc(*prv);
		*prv = NULL;
	}
}

void BKE_previewimg_clear(struct PreviewImage *prv, enum eIconSizes size)
{
	if (prv->rect[size]) {
		MEM_freeN(prv->rect[size]);
		prv->rect[size] = NULL;
	}
	if (prv->gputexture[size]) {
		GPU_texture_free(prv->gputexture[size]);
	}
	prv->h[size] = prv->w[size] = 0;
	prv->flag[size] |= CHANGED;
	prv->changed_timestamp[size] = 0;
}

PreviewImage *BKE_previewimg_copy(PreviewImage *prv)
{
	PreviewImage *prv_img = NULL;
	int i;

	if (prv) {
		prv_img = MEM_dupallocN(prv);
		for (i = 0; i < NUM_ICON_SIZES; ++i) {
			if (prv->rect[i]) {
				prv_img->rect[i] = MEM_dupallocN(prv->rect[i]);
			}
			else {
				prv_img->rect[i] = NULL;
			}
			prv_img->gputexture[i] = NULL;
		}
	}
	return prv_img;
}

void BKE_previewimg_id_free(ID *id)
{
	if (GS(id->name) == ID_MA) {
		Material *mat = (Material *)id;
		BKE_previewimg_free(&mat->preview);
	}
	else if (GS(id->name) == ID_TE) {
		Tex *tex = (Tex *)id;
		BKE_previewimg_free(&tex->preview);
	}
	else if (GS(id->name) == ID_WO) {
		World *wo = (World *)id;
		BKE_previewimg_free(&wo->preview);
	}
	else if (GS(id->name) == ID_LA) {
		Lamp *la  = (Lamp *)id;
		BKE_previewimg_free(&la->preview);
	}
	else if (GS(id->name) == ID_IM) {
		Image *img  = (Image *)id;
		BKE_previewimg_free(&img->preview);
	}
	else if (GS(id->name) == ID_BR) {
		Brush *br  = (Brush *)id;
		BKE_previewimg_free(&br->preview);
	}
}

PreviewImage *BKE_previewimg_id_get(ID *id)
{
	PreviewImage *prv_img = NULL;

	if (GS(id->name) == ID_MA) {
		Material *mat = (Material *)id;
		if (!mat->preview) mat->preview = BKE_previewimg_create();
		prv_img = mat->preview;
	}
	else if (GS(id->name) == ID_TE) {
		Tex *tex = (Tex *)id;
		if (!tex->preview) tex->preview = BKE_previewimg_create();
		prv_img = tex->preview;
	}
	else if (GS(id->name) == ID_WO) {
		World *wo = (World *)id;
		if (!wo->preview) wo->preview = BKE_previewimg_create();
		prv_img = wo->preview;
	}
	else if (GS(id->name) == ID_LA) {
		Lamp *la  = (Lamp *)id;
		if (!la->preview) la->preview = BKE_previewimg_create();
		prv_img = la->preview;
	}
	else if (GS(id->name) == ID_IM) {
		Image *img  = (Image *)id;
		if (!img->preview) img->preview = BKE_previewimg_create();
		prv_img = img->preview;
	}
	else if (GS(id->name) == ID_BR) {
		Brush *br  = (Brush *)id;
		if (!br->preview) br->preview = BKE_previewimg_create();
		prv_img = br->preview;
	}

	return prv_img;
}

/**
 * Generate an empty PreviewImage, if not yet existing.
 */
PreviewImage *BKE_previewimg_cached_get(const char *name)
{
	PreviewImage *prv = NULL;
	void **prv_v;

	prv_v = BLI_ghash_lookup_p(gCachedPreviews, name);

	if (prv_v) {
		prv = *prv_v;
		BLI_assert(prv);
	}

	if (!prv) {
		prv = BKE_previewimg_create();
	}

	if (prv_v) {
		*prv_v = prv;
	}
	else {
		BLI_ghash_insert(gCachedPreviews, (void *)BLI_strdup(name), prv);
	}

	return prv;
}

/**
 * Generate a PreviewImage from given file path, using thumbnails management, if not yet existing.
 */
PreviewImage *BKE_previewimg_cached_thumbnail_get(
        const char *name, const char *path, const int source, bool force_update)
{
	PreviewImage *prv = NULL;
	void **prv_v;
	int icon_w, icon_h;

	prv_v = BLI_ghash_lookup_p(gCachedPreviews, name);

	if (prv_v) {
		prv = *prv_v;
		BLI_assert(prv);
	}

	if (prv && force_update) {
		BKE_previewimg_clear(prv, ICON_SIZE_ICON);
		BKE_previewimg_clear(prv, ICON_SIZE_PREVIEW);
	}
	else if (!prv) {
		prv = BKE_previewimg_create();
		force_update = true;
	}

	if (force_update) {
		ImBuf *thumb = IMB_thumb_manage(path, THB_LARGE, source);

		if (thumb) {
			prv->w[ICON_SIZE_PREVIEW] = thumb->x;
			prv->h[ICON_SIZE_PREVIEW] = thumb->y;
			prv->rect[ICON_SIZE_PREVIEW] = MEM_dupallocN(thumb->rect);
			prv->flag[ICON_SIZE_PREVIEW] &= ~(CHANGED | USER_EDITED);

			if (thumb->x > thumb->y) {
				icon_w = ICON_RENDER_DEFAULT_HEIGHT;
				icon_h = (thumb->y * icon_w) / thumb->x + 1;
			}
			else if (thumb->x < thumb->y) {
				icon_h = ICON_RENDER_DEFAULT_HEIGHT;
				icon_w = (thumb->x * icon_h) / thumb->y + 1;
			}
			else {
				icon_w = icon_h = ICON_RENDER_DEFAULT_HEIGHT;
			}

			IMB_scaleImBuf(thumb, icon_w, icon_h);
			prv->w[ICON_SIZE_ICON] = icon_w;
			prv->h[ICON_SIZE_ICON] = icon_h;
			prv->rect[ICON_SIZE_ICON] = MEM_dupallocN(thumb->rect);
			prv->flag[ICON_SIZE_ICON] &= ~(CHANGED | USER_EDITED);

			IMB_freeImBuf(thumb);
		}

		if (prv_v) {
			*prv_v = prv;
		}
		else {
			BLI_ghash_insert(gCachedPreviews, (void *)BLI_strdup(name), prv);
		}
	}

	return prv;
}

void BKE_previewimg_cached_release(const char *name)
{
	PreviewImage *prv = BLI_ghash_popkey(gCachedPreviews, (void *)name, MEM_freeN);

	if (prv) {
		if (prv->icon_id) {
			BKE_icon_delete(prv->icon_id);
		}
		BKE_previewimg_freefunc(prv);
	}
}

void BKE_icon_changed(int id)
{
	Icon *icon = NULL;
	
	if (!id || G.background) return;

	icon = BLI_ghash_lookup(gIcons, SET_INT_IN_POINTER(id));
	
	if (icon) {
		PreviewImage *prv = BKE_previewimg_id_get((ID *)icon->obj);

		/* all previews changed */
		if (prv) {
			int i;
			for (i = 0; i < NUM_ICON_SIZES; ++i) {
				prv->flag[i] |= CHANGED;
				prv->changed_timestamp[i]++;
			}
		}
	}
}

int BKE_icon_id_get(struct ID *id)
{
	Icon *new_icon = NULL;

	if (!id || G.background)
		return 0;

	if (id->icon_id)
		return id->icon_id;

	id->icon_id = get_next_free_id();

	if (!id->icon_id) {
		printf("%s: Internal error - not enough IDs\n", __func__);
		return 0;
	}

	new_icon = MEM_mallocN(sizeof(Icon), __func__);

	new_icon->obj = id;
	new_icon->type = GS(id->name);
	
	/* next two lines make sure image gets created */
	new_icon->drawinfo = NULL;
	new_icon->drawinfo_free = NULL;

	BLI_ghash_insert(gIcons, SET_INT_IN_POINTER(id->icon_id), new_icon);
	
	return id->icon_id;
}

/**
 * Return icon id of given preview, or create new icon if not found.
 */
int BKE_icon_preview_get(PreviewImage *preview)
{
	Icon *new_icon = NULL;

	if (!preview || G.background)
		return 0;

	if (preview->icon_id)
		return preview->icon_id;

	preview->icon_id = get_next_free_id();

	if (!preview->icon_id) {
		printf("%s: Internal error - not enough IDs\n", __func__);
		return 0;
	}

	new_icon = MEM_mallocN(sizeof(Icon), __func__);

	new_icon->obj = preview;
	new_icon->type = 0;  /* Special, tags as non-ID icon/preview. */

	/* next two lines make sure image gets created */
	new_icon->drawinfo = NULL;
	new_icon->drawinfo_free = NULL;

	BLI_ghash_insert(gIcons, SET_INT_IN_POINTER(preview->icon_id), new_icon);

	return preview->icon_id;
}

Icon *BKE_icon_get(int icon_id)
{
	Icon *icon = NULL;

	icon = BLI_ghash_lookup(gIcons, SET_INT_IN_POINTER(icon_id));
	
	if (!icon) {
		printf("%s: Internal error, no icon for icon ID: %d\n", __func__, icon_id);
		return NULL;
	}

	return icon;
}

void BKE_icon_set(int icon_id, struct Icon *icon)
{
	void **val_p;

	if (BLI_ghash_ensure_p(gIcons, SET_INT_IN_POINTER(icon_id), &val_p)) {
		printf("%s: Internal error, icon already set: %d\n", __func__, icon_id);
		return;
	}

	*val_p = icon;
}

void BKE_icon_id_delete(struct ID *id)
{
	if (!id->icon_id) return;  /* no icon defined for library object */

	BLI_ghash_remove(gIcons, SET_INT_IN_POINTER(id->icon_id), NULL, icon_free);
	id->icon_id = 0;
}

/**
 * Remove icon and free data.
 */
void BKE_icon_delete(int icon_id)
{
	Icon *icon;

	if (!icon_id) return;  /* no icon defined for library object */

	icon = BLI_ghash_popkey(gIcons, SET_INT_IN_POINTER(icon_id), NULL);

	if (icon) {
		if (icon->type) {
			((ID *)(icon->obj))->icon_id = 0;
		}
		else {
			((PreviewImage *)(icon->obj))->icon_id = 0;
		}
		icon_free(icon);
	}
}
