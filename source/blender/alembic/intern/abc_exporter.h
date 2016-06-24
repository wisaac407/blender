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

#pragma once

#include <Alembic/Abc/All.h>
#include <map>
#include <set>
#include <vector>

class AbcObjectWriter;
class AbcTransformWriter;

struct EvaluationContext;
struct Main;
struct Object;
struct Scene;

struct ExportSettings {
	ExportSettings();

	Scene *scene;

	bool selected_only;
	bool visible_layers_only;
	bool renderable_only;

	double startframe, endframe;
	double xform_frame_step;
	double shape_frame_step;
	double shutter_open;
	double shutter_close;
	float global_scale;

	bool flatten_hierarchy;

	bool export_normals;
	bool export_uvs;
	bool export_vcols;
	bool export_face_sets;
	bool export_vweigths;

	bool apply_subdiv;
	bool use_subdiv_schema;
	bool export_child_hairs;
	bool export_ogawa;
	bool pack_uv;

	bool do_convert_axis;
	float convert_matrix[3][3];
};

class AbcExporter {
	ExportSettings &m_settings;

	const char *m_filename;

	Alembic::Abc::OArchive m_archive;
	unsigned int m_trans_sampling_index, m_shape_sampling_index;

	Scene *m_scene;

	std::map<std::string, AbcTransformWriter *> m_xforms;
	std::vector<AbcObjectWriter *> m_shapes;

public:
	AbcExporter(Scene *scene, const char *filename, ExportSettings &settings);
	~AbcExporter();

	void operator()(Main *bmain, float &progress, bool &was_canceled);

private:
	void getShutterSamples(double step, bool time_relative, std::vector<double> &samples);

	Alembic::Abc::TimeSamplingPtr createTimeSampling(double step);

	void getFrameSet(double step, std::set<double> &frames);

	void createTransformWritersHierarchy(EvaluationContext *eval_ctx);
	void createTransformWritersFlat();
	void createTransformWriter(Object *ob,  Object *parent, Object *dupliObParent);
	void exploreTransform(EvaluationContext *eval_ctx, Object *ob, Object *parent, Object *dupliObParent = NULL);
	void exploreObject(EvaluationContext *eval_ctx, Object *ob, Object *dupliObParent);
	void createShapeWriters(EvaluationContext *eval_ctx);
	void createShapeWriter(Object *ob, Object *dupliObParent);

	AbcTransformWriter *getXForm(const std::string &name);

	void setCurrentFrame(Main *bmain, double t);
};