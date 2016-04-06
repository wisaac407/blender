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
 * Contributor(s): Esteban Tovagliari, Cedric Paille, Kevin Dietrich
 *
 * ***** END GPL LICENSE BLOCK *****
 */

#ifndef __ABC_SHAPE_WRITER_H__
#define __ABC_SHAPE_WRITER_H__

#include "abc_object.h"

#include <Alembic/AbcGeom/All.h>

struct Scene;

class AbcTransformWriter;

class AbcShapeWriter : public AbcObjectWriter {
protected:
	Scene *m_scene;
	uint32_t m_time_sampling;

public:
    AbcShapeWriter(Scene *sce,
	               Object *obj,
	               AbcTransformWriter *parent,
	               uint32_t timeSampling,
	               AbcExportOptions &opts);

protected:
	void calcBounds(const std::vector<float> &points);
};

#endif  /* __ABC_SHAPE_WRITER_H__ */
