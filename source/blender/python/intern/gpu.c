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
 * The Original Code is Copyright (C) 2006 Blender Foundation.
 * All rights reserved.
 *
 * The Original Code is: all of this file.
 *
 * Contributor(s): Benoit Bolsee.
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/python/intern/gpu.c
 *  \ingroup pythonintern
 *
 * This file defines the 'gpu' module, used to get GLSL shader code and data
 * from blender materials.
 */

/* python redefines */
#ifdef _POSIX_C_SOURCE
#undef _POSIX_C_SOURCE
#endif

#include <Python.h>

#include "DNA_scene_types.h"
#include "DNA_material_types.h"
#include "DNA_ID.h"
#include "DNA_customdata_types.h"

#include "BLI_listbase.h"
#include "BLI_utildefines.h"

#include "RNA_access.h"

#include "bpy_rna.h"

#include "../generic/py_capi_utils.h"

#include "GPU_extensions.h"
#include "GPU_material.h"

#include "gpu.h"

#define PY_MODULE_ADD_CONSTANT(module, name) PyModule_AddIntConstant(module, # name, name)

PyDoc_STRVAR(M_gpu_doc,
"This module provides access to the GLSL shader."
);
static struct PyModuleDef gpumodule = {
	PyModuleDef_HEAD_INIT,
	"gpu",     /* name of module */
	M_gpu_doc, /* module documentation */
	-1,        /* size of per-interpreter state of the module,
	            *  or -1 if the module keeps state in global variables. */
	NULL, NULL, NULL, NULL, NULL
};

static PyObject *PyInit_gpu(void)
{
	PyObject *m;

	m = PyModule_Create(&gpumodule);
	if (m == NULL)
		return NULL;


	/* Take care to update docs when editing: 'doc/python_api/rst/gpu.rst' */


	/* -------------------------------------------------------------------- */
	/* GPUDynamicType */

	/* device constant groups */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_MISC);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_LAMP);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_OBJECT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_SAMPLER);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_MIST);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_WORLD);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_GROUP_MAT);

	/* device constants */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_NONE);
	/* GPU_DYNAMIC_GROUP_OBJECT */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_VIEWMAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_MAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_VIEWIMAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_IMAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_COLOR);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_OBJECT_AUTOBUMPSCALE);
	/* GPU_DYNAMIC_GROUP_LAMP */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNVEC);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNCO);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNIMAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNPERSMAT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNENERGY);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DYNCOL);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_ATT1);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_ATT2);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_DISTANCE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_SPOTSIZE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_LAMP_SPOTBLEND);
	/* GPU_DYNAMIC_GROUP_SAMPLER */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_SAMPLER_2DBUFFER);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_SAMPLER_2DIMAGE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_SAMPLER_2DSHADOW);
	/* GPU_DYNAMIC_GROUP_MIST */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_ENABLE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_START);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_DISTANCE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_INTENSITY);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_TYPE);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MIST_COLOR);
	/* GPU_DYNAMIC_GROUP_WORLD */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_HORIZON_COLOR);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_AMBIENT_COLOR);
	/* GPU_DYNAMIC_GROUP_MAT */
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_DIFFRGB);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_REF);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_SPECRGB);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_SPEC);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_HARD);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_EMIT);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_AMB);
	PY_MODULE_ADD_CONSTANT(m, GPU_DYNAMIC_MAT_ALPHA);


	/* -------------------------------------------------------------------- */
	/* GPUDataType */

	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_1I);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_1F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_2F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_3F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_4F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_9F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_16F);
	PY_MODULE_ADD_CONSTANT(m, GPU_DATA_4UB);


	/* -------------------------------------------------------------------- */
	/* CustomDataType
	 *
	 * Intentionally only include the subset used by the GPU API.
	 */
	PY_MODULE_ADD_CONSTANT(m, CD_MTFACE);
	PY_MODULE_ADD_CONSTANT(m, CD_ORCO);
	PY_MODULE_ADD_CONSTANT(m, CD_TANGENT);
	PY_MODULE_ADD_CONSTANT(m, CD_MCOL);
	return m;
}

#define PY_DICT_ADD_STRING(d, s, f)      \
	val = PyUnicode_FromString(s->f);    \
	PyDict_SetItemString(d, # f, val);   \
	Py_DECREF(val)

#define PY_DICT_ADD_LONG(d, s, f)        \
	val = PyLong_FromLong(s->f);         \
	PyDict_SetItemString(d, # f, val);   \
	Py_DECREF(val)

#define PY_DICT_ADD_ID(d, s, f)                      \
	RNA_id_pointer_create((struct ID *)s->f, &tptr); \
	val = pyrna_struct_CreatePyObject(&tptr);        \
	PyDict_SetItemString(d, # f, val);               \
	Py_DECREF(val)

#if 0  /* UNUSED */
#define PY_OBJ_ADD_ID(d, s, f)                      \
	val = PyUnicode_FromString(&s->f->id.name[2]);  \
	PyObject_SetAttrString(d, # f, val);            \
	Py_DECREF(val)

#define PY_OBJ_ADD_LONG(d, s, f)         \
	val = PyLong_FromLong(s->f);         \
	PyObject_SetAttrString(d, # f, val); \
	Py_DECREF(val)

#define PY_OBJ_ADD_STRING(d, s, f)       \
	val = PyUnicode_FromString(s->f);    \
	PyObject_SetAttrString(d, # f, val); \
	Py_DECREF(val)
#endif

PyDoc_STRVAR(GPU_export_shader_doc,
"export_shader(scene, material)\n"
"\n"
"   Returns the GLSL shader that produces the visual effect of material in scene.\n"
"\n"
"   :return: Dictionary defining the shader, uniforms and attributes.\n"
"   :rtype: Dict"
);
static PyObject *GPU_export_shader(PyObject *UNUSED(self), PyObject *args, PyObject *kwds)
{
	PyObject *pyscene;
	PyObject *pymat;
	PyObject *result;
	PyObject *dict;
	PyObject *val;
	PyObject *seq;

	int i;
	Scene *scene;
	PointerRNA tptr;
	Material *material;
	GPUShaderExport *shader;
	GPUInputUniform *uniform;
	GPUInputAttribute *attribute;

	static const char *kwlist[] = {"scene", "material", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "OO:export_shader", (char **)(kwlist), &pyscene, &pymat))
		return NULL;

	scene = (Scene *)PyC_RNA_AsPointer(pyscene, "Scene");
	if (scene == NULL) {
		return NULL;
	}

	material = (Material *)PyC_RNA_AsPointer(pymat, "Material");
	if (material == NULL) {
		return NULL;
	}

	/* we can call our internal function at last: */
	shader = GPU_shader_export(scene, material);
	if (!shader) {
		PyErr_SetString(PyExc_RuntimeError, "cannot export shader");
		return NULL;
	}
	/* build a dictionary */
	result = PyDict_New();
	if (shader->fragment) {
		PY_DICT_ADD_STRING(result, shader, fragment);
	}
	if (shader->vertex) {
		PY_DICT_ADD_STRING(result, shader, vertex);
	}
	seq = PyList_New(BLI_listbase_count(&shader->uniforms));
	for (i = 0, uniform = shader->uniforms.first; uniform; uniform = uniform->next, i++) {
		dict = PyDict_New();
		PY_DICT_ADD_STRING(dict, uniform, varname);
		PY_DICT_ADD_LONG(dict, uniform, datatype);
		PY_DICT_ADD_LONG(dict, uniform, type);
		if (uniform->lamp) {
			PY_DICT_ADD_ID(dict, uniform, lamp);
		}
		if (uniform->image) {
			PY_DICT_ADD_ID(dict, uniform, image);
		}
		if (uniform->type == GPU_DYNAMIC_SAMPLER_2DBUFFER ||
		    uniform->type == GPU_DYNAMIC_SAMPLER_2DIMAGE ||
		    uniform->type == GPU_DYNAMIC_SAMPLER_2DSHADOW)
		{
			PY_DICT_ADD_LONG(dict, uniform, texnumber);
		}
		if (uniform->texpixels) {
			val = PyByteArray_FromStringAndSize((const char *)uniform->texpixels, uniform->texsize * 4);
			PyDict_SetItemString(dict, "texpixels", val);
			Py_DECREF(val);
			PY_DICT_ADD_LONG(dict, uniform, texsize);
		}
		PyList_SET_ITEM(seq, i, dict);
	}
	PyDict_SetItemString(result, "uniforms", seq);
	Py_DECREF(seq);

	seq = PyList_New(BLI_listbase_count(&shader->attributes));
	for (i = 0, attribute = shader->attributes.first; attribute; attribute = attribute->next, i++) {
		dict = PyDict_New();
		PY_DICT_ADD_STRING(dict, attribute, varname);
		PY_DICT_ADD_LONG(dict, attribute, datatype);
		PY_DICT_ADD_LONG(dict, attribute, type);
		PY_DICT_ADD_LONG(dict, attribute, number);
		if (attribute->name) {
			if (attribute->name[0] != 0) {
				PY_DICT_ADD_STRING(dict, attribute, name);
			}
			else {
				val = PyLong_FromLong(0);
				PyDict_SetItemString(dict, "name", val);
				Py_DECREF(val);
			}
		}
		PyList_SET_ITEM(seq, i, dict);
	}
	PyDict_SetItemString(result, "attributes", seq);
	Py_DECREF(seq);

	GPU_free_shader_export(shader);

	return result;
}

static PyMethodDef meth_export_shader[] = {
	{"export_shader", (PyCFunction)GPU_export_shader, METH_VARARGS | METH_KEYWORDS, GPU_export_shader_doc}
};

/* -------------------------------------------------------------------- */
/* GPU Offscreen PyObject */

/* annoying since arg parsing won't check overflow */
#define UINT_IS_NEG(n) ((n) > INT_MAX)

typedef struct {
	PyObject_HEAD
	GPUOffScreen *ofs;
} PyGPUOffscreen;

PyDoc_STRVAR(GPUOffscreen_width_doc, "Texture width.\n\n:type: GLsizei");
static PyObject *GPUOffscreen_width_get(PyGPUOffscreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_width(self->ofs));
}

PyDoc_STRVAR(GPUOffscreen_height_doc, "Texture height.\n\n:type: GLsizei");
static PyObject *GPUOffscreen_height_get(PyGPUOffscreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_height(self->ofs));
}

PyDoc_STRVAR(GPUOffscreen_framebuffer_object_doc, "Framebuffer object.\n\n:type: GLuint");
static PyObject *GPUOffscreen_framebuffer_object_get(PyGPUOffscreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_fb_object(self->ofs));
}

PyDoc_STRVAR(GPUOffscreen_color_object_doc, "Color object.\n\n:type: GLuint");
static PyObject *GPUOffscreen_color_object_get(PyGPUOffscreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_color_object(self->ofs));
}

static PyGetSetDef GPUOffscreen_getseters[] = {
	{(char *)"color_object", (getter)GPUOffscreen_color_object_get, (setter)NULL, GPUOffscreen_color_object_doc, NULL},
	{(char *)"framebuffer_object", (getter)GPUOffscreen_framebuffer_object_get, (setter)NULL, GPUOffscreen_framebuffer_object_doc, NULL},
	{(char *)"width", (getter)GPUOffscreen_width_get, (setter)NULL, GPUOffscreen_width_doc, NULL},
	{(char *)"height", (getter)GPUOffscreen_height_get, (setter)NULL, GPUOffscreen_height_doc, NULL},
	{NULL, NULL, NULL, NULL, NULL}  /* Sentinel */
};

static int PyGPUOffscreen__tp_init(PyGPUOffscreen *self, PyObject *args, PyObject *kwargs)
{
	unsigned int width, height;
	const char *keywords[] = {"width", "height",  NULL};
	char err_out[256];

	if (!PyArg_ParseTupleAndKeywords(args, kwargs, "ii:GPUOffscreen", (char **)keywords, &width, &height)) {
		return -1;
	}

	if (UINT_IS_NEG(width)) {
		PyErr_SetString(PyExc_ValueError, "negative 'width' given");
		return -1;
	}

	if (UINT_IS_NEG(height)) {
		PyErr_SetString(PyExc_ValueError, "negative 'height' given");
		return -1;
	}

	self->ofs = GPU_offscreen_create(width, height, err_out);
	return 0;
}

static void PyGPUOffscreen__tp_dealloc(PyGPUOffscreen *self)
{
	if (self->ofs)
		GPU_offscreen_free(self->ofs);
	Py_TYPE(self)->tp_free((PyObject *)self);
}

PyDoc_STRVAR(py_GPUOffscreen_doc,
"GPUOffscreen(width, height) -> new GPU Offscreen object"
"initialized to hold a framebuffer object of ``width`` x ``height``.\n"
""
);
PyTypeObject PyGPUOffscreen_Type = {
	PyVarObject_HEAD_INIT(NULL, 0)
	"GPUOffscreen",                              /* tp_name */
	sizeof(PyGPUOffscreen),                      /* tp_basicsize */
	0,                                           /* tp_itemsize */
	/* methods */
	(destructor)PyGPUOffscreen__tp_dealloc,      /* tp_dealloc */
	NULL,                                        /* tp_print */
	NULL,                                        /* tp_getattr */
	NULL,                                        /* tp_setattr */
	NULL,                                        /* tp_compare */
	NULL,                                        /* tp_repr */
	NULL,                                        /* tp_as_number */
	NULL,                                        /* tp_as_sequence */
	NULL,                                        /* tp_as_mapping */
	NULL,                                        /* tp_hash */
	NULL,                                        /* tp_call */
	NULL,                                        /* tp_str */
	NULL,                                        /* tp_getattro */
	NULL,                                        /* tp_setattro */
	NULL,                                        /* tp_as_buffer */
	Py_TPFLAGS_DEFAULT,                          /* tp_flags */
	py_GPUOffscreen_doc,                         /* Documentation string */
	NULL,                                        /* tp_traverse */
	NULL,                                        /* tp_clear */
	NULL,                                        /* tp_richcompare */
	0,                                           /* tp_weaklistoffset */
	NULL,                                        /* tp_iter */
	NULL,                                        /* tp_iternext */
	NULL,                                        /* tp_methods */
	NULL,                                        /* tp_members */
	GPUOffscreen_getseters,                      /* tp_getset */
	NULL,                                        /* tp_base */
	NULL,                                        /* tp_dict */
	NULL,                                        /* tp_descr_get */
	NULL,                                        /* tp_descr_set */
	0,                                           /* tp_dictoffset */
	(initproc)PyGPUOffscreen__tp_init,           /* tp_init */
	(allocfunc)PyType_GenericAlloc,              /* tp_alloc */
	(newfunc)PyType_GenericNew,                  /* tp_new */
	(freefunc)0,                                 /* tp_free */
	NULL,                                        /* tp_is_gc */
	NULL,                                        /* tp_bases */
	NULL,                                        /* tp_mro */
	NULL,                                        /* tp_cache */
	NULL,                                        /* tp_subclasses */
	NULL,                                        /* tp_weaklist */
	(destructor) NULL                            /* tp_del */
};

/* -------------------------------------------------------------------- */
/* GPU offscreen methods */

PyDoc_STRVAR(GPU_offscreen_object_bind_doc,
"offscreen_object_bind(offscreen_object, use_save)\n"
"\n"
"   Bind an offscreen object.\n"
"\n"
"   :param offscreen_object: offscreen object\n"
"   :type offscreen_object: :class:`gpu.OffScreenObject`\n"
"   :param use_save: save OpenGL current states\n"
"   :type use_save: bool"
);
static PyObject *GPU_offscreen_object_bind(PyObject *UNUSED(self), PyObject *args, PyObject *kwds)
{
	PyGPUOffscreen *PyOfs;
	int use_save;

	static const char *kwlist[] = {"offscreen_object", "use_save", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "Oi:offscreen_object_bind", (char **)(kwlist), &PyOfs, &use_save))
		return NULL;

	GPU_offscreen_bind(PyOfs->ofs, use_save);
	Py_RETURN_NONE;
}

static PyMethodDef meth_offscreen_object_bind[] = {
	{"offscreen_object_bind", (PyCFunction)GPU_offscreen_object_bind, METH_VARARGS | METH_KEYWORDS, GPU_offscreen_object_bind_doc}
};

PyDoc_STRVAR(GPU_offscreen_object_create_doc,
"offscreen_object_create(width, height)\n"
"\n"
"   Return a GPUOffScreen.\n"
"\n"
"   :return: struct with GPUFrameBuffer, GPUTexture, GPUTexture.\n"
"   :rtype: :class:`gpu.OffScreenObject`"
);
static PyObject *GPU_offscreen_object_create(PyObject *UNUSED(self), PyObject *args, PyObject *kwds)
{
	int width;
	int height;

	static const char *kwlist[] = {"width", "height", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "ii:offscreen_object_create", (char **)(kwlist), &width, &height))
		return NULL;

	return PyObject_CallObject((PyObject *) &PyGPUOffscreen_Type, args);
}

static PyMethodDef meth_offscreen_object_create[] = {
	{"offscreen_object_create", (PyCFunction)GPU_offscreen_object_create, METH_VARARGS | METH_KEYWORDS, GPU_offscreen_object_create_doc}
};

PyDoc_STRVAR(GPU_offscreen_object_free_doc,
"offscreen_object_free(offscreen_object)\n"
"\n"
"   Free an offscreen object\n"
"   The framebuffer, texture and render objects will no longer be accessible.\n"
"\n"
"   :param offscreen_object: offscreen object\n"
"   :type offscreen_object: :class:`gpu.OffScreenObject`"
);
static PyObject *GPU_offscreen_object_free(PyObject *UNUSED(self), PyObject *args, PyObject *kwds)
{
	PyGPUOffscreen *PyOfs;

	static const char *kwlist[] = {"offscreen_object", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "O:offscreen_object_unbind", (char **)(kwlist), &PyOfs))
		return NULL;

	GPU_offscreen_free(PyOfs->ofs);
	PyOfs->ofs = NULL;
	Py_RETURN_NONE;
}

static PyMethodDef meth_offscreen_object_free[] = {
	{"offscreen_object_free", (PyCFunction)GPU_offscreen_object_free, METH_VARARGS | METH_KEYWORDS, GPU_offscreen_object_free_doc}
};

PyDoc_STRVAR(GPU_offscreen_object_unbind_doc,
"offscreen_object_unbind(offscreen_object, use_restore)\n"
"\n"
"   Unbind an offscreen object.\n"
"\n"
"   :param offscreen_object: offscreen object\n"
"   :type offscreen_object: :class:`gpu.OffScreenObject`\n"
"   :param use_restore: restore OpenGL previous states\n"
"   :type use_restore: bool"
);
static PyObject *GPU_offscreen_object_unbind(PyObject *UNUSED(self), PyObject *args, PyObject *kwds)
{
	PyGPUOffscreen *PyOfs;
	int use_restore;

	static const char *kwlist[] = {"offscreen_object", "use_restore", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "Oi:offscreen_object_unbind", (char **)(kwlist), &PyOfs, &use_restore))
		return NULL;

	GPU_offscreen_unbind(PyOfs->ofs, use_restore);
	Py_RETURN_NONE;
}

static PyMethodDef meth_offscreen_object_unbind[] = {
	{"offscreen_object_unbind", (PyCFunction)GPU_offscreen_object_unbind, METH_VARARGS | METH_KEYWORDS, GPU_offscreen_object_unbind_doc}
};

/* -------------------------------------------------------------------- */
/* Initialize Module */

PyObject *GPU_initPython(void)
{
	PyObject *module;

	/* Register the 'GPUOffscreen' class */
	if (PyType_Ready(&PyGPUOffscreen_Type)) {
		return NULL;
	}

	module = PyInit_gpu();

	PyModule_AddObject(module, "export_shader", (PyObject *)PyCFunction_New(meth_export_shader, NULL));

	PyModule_AddObject(module, "OffscreenObject", (PyObject *) &PyGPUOffscreen_Type);

	PyModule_AddObject(module, "offscreen_object_bind", (PyObject *)PyCFunction_New(meth_offscreen_object_bind, NULL));
	PyModule_AddObject(module, "offscreen_object_create", (PyObject *)PyCFunction_New(meth_offscreen_object_create, NULL));
	PyModule_AddObject(module, "offscreen_object_free", (PyObject *)PyCFunction_New(meth_offscreen_object_free, NULL));
	PyModule_AddObject(module, "offscreen_object_unbind", (PyObject *)PyCFunction_New(meth_offscreen_object_unbind, NULL));

	PyDict_SetItemString(PyImport_GetModuleDict(), "gpu", module);

	return module;
}

