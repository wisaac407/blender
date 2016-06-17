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
 * The Original Code is Copyright (C) 2016 Blender Foundation.
 * All rights reserved.
 *
 * Contributor(s): none yet.
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/blenkernel/intern/layer.c
 *  \ingroup bke
 *
 * \brief Functions for a generic layer managment system.
 *
 * TODO sorting, renaming, drawing, filtering
 */

#include <stdlib.h>

#include "BKE_context.h"
#include "BKE_layer.h" /* own include */

#include "BLI_listbase.h"
#include "BLI_string.h"

#include "DNA_defs.h"
#include "DNA_space_types.h"

#include "MEM_guardedalloc.h"

static void layeritem_free(LayerTreeItem *litem);


/* -------------------------------------------------------------------- */
/** \name Layer Tree
 *
 * A layer tree is the container for the tree/list of layers and layer groups that is displayed in the GUI later.
 *
 * \{ */

LayerTree *BKE_layertree_new(const eLayerTree_Type type)
{
	LayerTree *ltree = MEM_callocN(sizeof(LayerTree), __func__);
	ltree->type = type;
	return ltree;
}

void BKE_layertree_delete(LayerTree *ltree)
{
	BKE_LAYERTREE_ITER_START(ltree, 0, i, litem)
	{
		/* layeritem_free does all we need in this case. No un-registering needed */
		layeritem_free(litem);
	}
	BKE_LAYERTREE_ITER_END;

	if (ltree->items_all) {
		MEM_freeN(ltree->items_all);
	}
	MEM_freeN(ltree);
}

/**
 * Iterate over \a itemlist and all of its children, wrapped by #BKE_layertree_iterate.
 * \note Recursive
 */
static bool layertree_iterate_list(
        const ListBase *itemlist, LayerTreeIterFunc foreach, void *customdata,
        const bool inverse)
{
	for (LayerTreeItem *litem = (inverse ? itemlist->last : itemlist->first), *litem_next;
	     litem;
	     litem = litem_next)
	{
		litem_next = inverse ? litem->prev : litem->next; /* in case list order is changed in callback */
		if (foreach(litem, customdata) == false || /* execute callback for current item */
		    layertree_iterate_list(&litem->childs, foreach, customdata, inverse) == false) /* iterate over childs */
		{
			return false;
		}
	}
	return true;
}

/**
 * Iterate over all items (including children) in the layer tree, executing \a foreach callback for each element.
 * (Pre-order traversal)
 *
 * \param foreach: Callback that can return false to stop the iteration.
 * \return if the iteration has been stopped because of a callback returning false.
 */
bool BKE_layertree_iterate(const LayerTree *ltree, LayerTreeIterFunc foreach, void *customdata, const bool inverse)
{
	return layertree_iterate_list(&ltree->items, foreach, customdata, inverse);
}

int BKE_layertree_get_totitems(const LayerTree *ltree)
{
	return ltree->tot_items;
}

/** \} */ /* Layer Tree */


/* -------------------------------------------------------------------- */
/** \name Layer Tree Item
 *
 * An item of the layer tree (layer, layer group, compositing layer, etc).
 * Although the technical precise term is "layer tree item", we usually just call it "layer item".
 *
 * \{ */

/**
 * Register an already allocated \a litem.
 *
 * \note Reallocates memory for item storage array, if you want to add many items at once,
 * better do differently (e.g. _ex version that allows reserving memory)
 */
void BKE_layeritem_register(
        LayerTree *tree, LayerTreeItem *litem, LayerTreeItem *parent,
        const eLayerTreeItem_Type type, const char *name,
        const LayerItemPollFunc poll, LayerItemDrawFunc draw, LayerItemDrawSettingsFunc draw_settings)
{
	litem->type = type;
	litem->index = tree->tot_items;
	litem->tree = tree;
	BLI_strncpy(litem->name, name, sizeof(litem->name));

	/* callbacks */
	litem->poll = poll;
	litem->draw = draw;
	litem->draw_settings = draw_settings;

	/* add to item array */
	tree->items_all = MEM_reallocN(tree->items_all, sizeof(*tree->items_all) * ++tree->tot_items);
	tree->items_all[tree->tot_items - 1] = litem;

	if (parent) {
		BLI_assert(ELEM(parent->type, LAYER_ITEMTYPE_GROUP));
		BLI_assert(parent->tree == tree);

		litem->parent = parent;
		/* add to child list of parent, not to item list of tree */
		BLI_addtail(&parent->childs, litem);
	}
	else {
		BLI_addhead(&tree->items, litem);
	}
}

/**
 * Allocate a new layer item of \a type and add it to the layer tree \a tree. Sorting happens later.
 *
 * \param parent: The parent layer group of the new item. NULL for ungrouped items
 * \return The newly created layer item.
 */
LayerTreeItem *BKE_layeritem_add(
        LayerTree *tree, LayerTreeItem *parent,
        const eLayerTreeItem_Type type, const char *name,
        const LayerItemPollFunc poll, LayerItemDrawFunc draw, LayerItemDrawSettingsFunc draw_settings)
{
	LayerTreeItem *litem = MEM_callocN(sizeof(LayerTreeItem), __func__);
	BKE_layeritem_register(tree, litem, parent, type, name, poll, draw, draw_settings);
	return litem;
}

static void layeritem_free(LayerTreeItem *litem)
{
	if (litem->free) {
		litem->free(litem);
	}
	MEM_freeN(litem);
}


/**
 * Recursive function to remove \a litem. Used to avoid multiple realloc's
 * for LayerTree.items_all, instead caller can simply realloc once (afterwards!).
 *
 * \param remove_children: Free and unlink all children (and their children, etc) of \a litem as well.
 */
static void layeritem_remove_ex(LayerTreeItem *litem, const bool remove_children)
{
	BLI_remlink(litem->parent ? &litem->parent->childs : &litem->tree->items, litem);

	for (int i = litem->index + 1; i < litem->tree->tot_items; i++) {
		litem->tree->items_all[i - 1] = litem->tree->items_all[i];
		litem->tree->items_all[i - 1]->index--;
	}
	litem->tree->tot_items--;

	if (remove_children) {
		for (LayerTreeItem *child = litem->childs.first, *child_next; child; child = child_next) {
			child_next = child->next;
			layeritem_remove_ex(child, true);
		}
		BLI_assert(BLI_listbase_is_empty(&litem->childs));
	}
	layeritem_free(litem);
}

/**
 * Free and unlink \a litem from the list and the array it's stored in.
 *
 * \param remove_children: Free and unlink all children (and their children, etc) of \a litem as well.
 * \note Calls recursive #layeritem_remove_ex.
 */
void BKE_layeritem_remove(LayerTreeItem *litem, const bool remove_children)
{
	LayerTree *ltree = litem->tree; /* store before deleting litem */
	layeritem_remove_ex(litem, remove_children);
	ltree->items_all = MEM_reallocN(ltree->items_all, sizeof(*ltree->items_all) * ltree->tot_items);
}

/**
 * Move \a litem that's already in the layer tree to slot \a newidx.
 */
void BKE_layeritem_move(LayerTreeItem *litem, const int newidx)
{
	const bool is_higher = litem->index < newidx;

	/* Already where we want to move it to. */
	if (litem->index == newidx)
		return;

	for (int i = is_higher ? litem->index + 1 : litem->index - 1;
	     i < litem->tree->tot_items && i >= 0;
	     is_higher ? i++ : i--)
	{
		const int iter_new_idx = i + (is_higher ? -1 : 1);
		litem->tree->items_all[iter_new_idx] = litem->tree->items_all[i];
		litem->tree->items_all[iter_new_idx]->index = iter_new_idx;
		if (i == newidx) {
			litem->tree->items_all[i] = litem;
			litem->index = i;
			break;
		}
	}
}

/**
 * Assign \a item to \a group.
 */
void BKE_layeritem_group_assign(LayerTreeItem *group, LayerTreeItem *item)
{
	ListBase *oldlist = item->parent ? &item->parent->childs : &item->tree->items;

	BLI_assert(group->type == LAYER_ITEMTYPE_GROUP);
	BLI_assert(BLI_findindex(oldlist, item) != -1);

	item->parent = group;
	BLI_remlink(oldlist, item);
	BLI_addtail(&group->childs, item);
}

/**
 * Iterate over all children (and their children, etc) of \a litem, executing \a foreach callback for each element.
 * (Pre-order traversal)
 *
 * \param foreach: Callback that can return false to stop the iteration.
 * \return if the iteration has been stopped because of a callback returning false.
 */
bool BKE_layeritem_iterate_childs(
        LayerTreeItem *litem, LayerTreeIterFunc foreach, void *customdata,
        const bool inverse)
{
	return layertree_iterate_list(&litem->childs, foreach, customdata, inverse);
}

/** \} */ /* Layer Tree Item */
