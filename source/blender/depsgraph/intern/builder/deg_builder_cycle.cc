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
 * The Original Code is Copyright (C) 2015 Blender Foundation.
 * All rights reserved.
 *
 * Contributor(s): Sergey Sharybin
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/depsgraph/intern/builder/deg_builder_cycle.cc
 *  \ingroup depsgraph
 */

#include "intern/builder/deg_builder_cycle.h"

// TOO(sergey): Use some wrappers over those?
#include <cstdio>
#include <cstdlib>
#include <stack>

extern "C" {
#include "BLI_utildefines.h"
}

#include "util/depsgraph_util_foreach.h"

#include "intern/nodes/deg_node.h"
#include "intern/nodes/deg_node_component.h"
#include "intern/nodes/deg_node_operation.h"

#include "intern/depsgraph.h"

namespace DEG {

struct StackEntry {
	OperationDepsNode *node;
	StackEntry *from;
	DepsRelation *via_relation;
};

void deg_graph_detect_cycles(Depsgraph *graph)
{
	/* Not is not visited at all during traversal. */
	const int NODE_NOT_VISITED = 0;
	/* Node has been visited during traversal and not in current stack. */
	const int NODE_VISITED = 1;
	/* Node has been visited during traversal and is in current stack. */
	const int NODE_IN_STACK = 2;

	std::stack<StackEntry> traversal_stack;
	foreach (OperationDepsNode *node, graph->operations) {
		bool has_inlinks = false;
		foreach (DepsRelation *rel, node->inlinks) {
			if (rel->from->type == DEPSNODE_TYPE_OPERATION) {
				has_inlinks = true;
			}
		}
		if (has_inlinks == false) {
			StackEntry entry;
			entry.node = node;
			entry.from = NULL;
			entry.via_relation = NULL;
			traversal_stack.push(entry);
			node->done = NODE_IN_STACK;
		}
		else {
			node->done = NODE_NOT_VISITED;
		}
	}

	while (!traversal_stack.empty()) {
		StackEntry &entry = traversal_stack.top();
		OperationDepsNode *node = entry.node;
		bool all_child_traversed = true;
		foreach (DepsRelation *rel, node->outlinks) {
			if (rel->to->type == DEPSNODE_TYPE_OPERATION) {
				OperationDepsNode *to = (OperationDepsNode *)rel->to;
				if (to->done == NODE_IN_STACK) {
					printf("Dependency cycle detected:\n");
					printf("  '%s' depends on '%s' through '%s'\n",
					       to->full_identifier().c_str(),
					       node->full_identifier().c_str(),
					       rel->name);

					StackEntry *current = &entry;
					while (current->node != to) {
						BLI_assert(current != NULL);
						printf("  '%s' depends on '%s' through '%s'\n",
						       current->node->full_identifier().c_str(),
						       current->from->node->full_identifier().c_str(),
						       current->via_relation->name);
						current = current->from;
					}
					/* TODO(sergey): So called roussian rlette cycle solver. */
					rel->flag |= DEPSREL_FLAG_CYCLIC;
				}
				else if (to->done == NODE_NOT_VISITED) {
					StackEntry new_entry;
					new_entry.node = to;
					new_entry.from = &entry;
					new_entry.via_relation = rel;
					traversal_stack.push(new_entry);
					to->done = NODE_IN_STACK;
					all_child_traversed = false;
					break;
				}
			}
		}
		if (all_child_traversed) {
			node->done = NODE_VISITED;
			traversal_stack.pop();
		}
	}
}

}  // namespace DEG
