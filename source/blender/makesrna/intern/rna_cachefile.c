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
 * Contributor(s): Kevin Dietrich.
 *
 * ***** END GPL LICENSE BLOCK *****
 */

#include "DNA_cachefile_types.h"
#include "DNA_scene_types.h"

#include "RNA_access.h"
#include "RNA_define.h"
#include "RNA_enum_types.h"

#include "rna_internal.h"

#ifdef RNA_RUNTIME

#ifdef WITH_ALEMBIC
#  include "../../../alembic/ABC_alembic.h"
#endif

static void rna_CacheFile_update_handle(Main *bmain, Scene *scene, PointerRNA *ptr)
{
#ifdef WITH_ALEMBIC
	CacheFile *cache_file = (CacheFile *)ptr->data;

	ABC_free_handle(cache_file->handle);
	cache_file->handle = ABC_create_handle(cache_file->filepath);
#else
	UNUSED_VARS(ptr);
#endif

	UNUSED_VARS(bmain, scene);
}

#else

static void rna_def_cachefile(BlenderRNA *brna)
{
	StructRNA *srna = RNA_def_struct(brna, "CacheFile", "ID");
	RNA_def_struct_sdna(srna, "CacheFile");
	RNA_def_struct_ui_text(srna, "CacheFile", "");
	RNA_def_struct_ui_icon(srna, ICON_FILE);

	PropertyRNA *prop = RNA_def_property(srna, "filepath", PROP_STRING, PROP_FILEPATH);
	RNA_def_property_ui_text(prop, "File Path", "Path to external displacements file");
	RNA_def_property_update(prop, 0, "rna_CacheFile_update_handle");

	prop = RNA_def_property(srna, "is_sequence", PROP_BOOLEAN, PROP_NONE);
	RNA_def_property_ui_text(prop, "Sequence", "Whether the cache is separated in a series of files");
	RNA_def_property_update(prop, 0, NULL);

	/* ----------------- For Scene time ------------------- */

	prop = RNA_def_property(srna, "frame_start", PROP_FLOAT, PROP_NONE);
	RNA_def_property_float_sdna(prop, NULL, "frame_start");
	RNA_def_property_range(prop, -MAXFRAME, MAXFRAME);
	RNA_def_property_ui_text(prop, "Frame Start", "Add this to the start frame");
	RNA_def_property_update(prop, 0, NULL);

	prop = RNA_def_property(srna, "frame_scale", PROP_FLOAT, PROP_NONE);
	RNA_def_property_float_sdna(prop, NULL, "frame_scale");
	RNA_def_property_range(prop, 0.0f, 100.0f);
	RNA_def_property_ui_text(prop, "Frame Scale", "Evaluation time in seconds");
	RNA_def_property_update(prop, 0, NULL);

	/* ----------------- Axis Conversion ----------------- */

	prop = RNA_def_property(srna, "forward_axis", PROP_ENUM, PROP_NONE);
	RNA_def_property_enum_sdna(prop, NULL, "forward_axis");
	RNA_def_property_enum_items(prop, rna_enum_object_axis_items);
	RNA_def_property_ui_text(prop, "Forward", "");
	RNA_def_property_update(prop, 0, NULL);

	prop = RNA_def_property(srna, "up_axis", PROP_ENUM, PROP_NONE);
	RNA_def_property_enum_sdna(prop, NULL, "up_axis");
	RNA_def_property_enum_items(prop, rna_enum_object_axis_items);
	RNA_def_property_ui_text(prop, "Up", "");
	RNA_def_property_update(prop, 0, NULL);

	prop = RNA_def_property(srna, "scale", PROP_FLOAT, PROP_NONE);
	RNA_def_property_float_sdna(prop, NULL, "scale");
	RNA_def_property_range(prop, 0.0f, 1.0f);
	RNA_def_property_ui_text(prop, "Scale", "");
	RNA_def_property_update(prop, 0, NULL);
}

void RNA_def_cachefile(BlenderRNA *brna)
{
	rna_def_cachefile(brna);
}

#endif