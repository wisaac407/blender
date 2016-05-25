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
 * Original Author: Sergey Sharybin
 * Contributor(s): None Yet
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/depsgraph/intern/build/deg_builder.cc
 *  \ingroup depsgraph
 */

// TODO(sergey): Use own wrapper over STD.
#include <stack>

#include "builder/deg_builder.h"

#include "DNA_anim_types.h"

#include "BLI_string.h"

#include "intern/depsgraph.h"
#include "intern/depsgraph_types.h"
#include "intern/depsnode.h"
#include "intern/depsnode_component.h"
#include "intern/depsnode_operation.h"

#include "depsgraph_util_foreach.h"

namespace DEG {

string deg_fcurve_id_name(const FCurve *fcu)
{
	char index_buf[32];
	// TODO(sergey): Use int-to-string utility or so.
	BLI_snprintf(index_buf, sizeof(index_buf), "[%d]", fcu->array_index);
	return string(fcu->rna_path) + index_buf;
}

void deg_graph_build_finalize(Depsgraph *graph)
{
	std::stack<OperationDepsNode *> stack;

	foreach (OperationDepsNode *node, graph->operations) {
		node->done = 0;
		node->num_links_pending = 0;
		foreach (DepsRelation *rel, node->inlinks) {
			if ((rel->from->type == DEPSNODE_TYPE_OPERATION) &&
			    (rel->flag & DEPSREL_FLAG_CYCLIC) == 0)
			{
				++node->num_links_pending;
			}
		}
		if (node->num_links_pending == 0) {
			stack.push(node);
		}
		IDDepsNode *id_node = node->owner->owner;
		id_node->id->tag |= LIB_TAG_DOIT;
	}

	while (!stack.empty()) {
		OperationDepsNode *node = stack.top();
		if (node->done == 0 && node->outlinks.size() != 0) {
			foreach (DepsRelation *rel, node->outlinks) {
				if (rel->to->type == DEPSNODE_TYPE_OPERATION) {
					OperationDepsNode *to = (OperationDepsNode *)rel->to;
					if ((rel->flag & DEPSREL_FLAG_CYCLIC) == 0) {
						BLI_assert(to->num_links_pending > 0);
						--to->num_links_pending;
					}
					if (to->num_links_pending == 0) {
						stack.push(to);
					}
				}
			}
			node->done = 1;
		}
		else {
			stack.pop();
			IDDepsNode *id_node = node->owner->owner;
			foreach (DepsRelation *rel, node->outlinks) {
				if (rel->to->type == DEPSNODE_TYPE_OPERATION) {
					OperationDepsNode *to = (OperationDepsNode *)rel->to;
					IDDepsNode *id_to = to->owner->owner;
					id_node->layers |= id_to->layers;
				}
			}
		}
	}

	/* Re-tag IDs for update if it was tagged before the relations update tag. */
	for (Depsgraph::IDNodeMap::const_iterator it = graph->id_hash.begin();
	     it != graph->id_hash.end();
	     ++it)
	{
		IDDepsNode *id_node = it->second;
		ID *id = id_node->id;
		if (id->tag & LIB_TAG_ID_RECALC_ALL &&
		    id->tag & LIB_TAG_DOIT)
		{
			id_node->tag_update(graph);
			id->tag &= ~LIB_TAG_DOIT;
		}
	}
}

}  // namespace DEG
