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
 * The Original Code is: all of this file.
 *
 * Contributor(s): Dalai Felinto
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/python/intern/gpu_offscreen.c
 *  \ingroup pythonintern
 *
 * This file defines the offscreen functionalities of the 'gpu' module
 * used for offline rendering
 */

/* python redefines */
#ifdef _POSIX_C_SOURCE
#undef _POSIX_C_SOURCE
#endif

#include <Python.h>

#include "DNA_object_types.h"

#include "BLI_utildefines.h"

#include "BKE_context.h"

#include "WM_types.h"

#include "ED_screen.h"

#include "GPU_extensions.h"
#include "GPU_compositing.h"

#include "../mathutils/mathutils.h"

#include "gpu.h"

/* -------------------------------------------------------------------- */
/* GPU Offscreen PyObject */

/* annoying since arg parsing won't check overflow */
#define UINT_IS_NEG(n) ((n) > INT_MAX)

typedef struct {
	PyObject_HEAD
	GPUOffScreen *ofs;
} PyGPUOffScreen;

PyDoc_STRVAR(GPUOffScreen_width_doc, "Texture width.\n\n:type: GLsizei");
static PyObject *GPUOffScreen_width_get(PyGPUOffScreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_width(self->ofs));
}

PyDoc_STRVAR(GPUOffScreen_height_doc, "Texture height.\n\n:type: GLsizei");
static PyObject *GPUOffScreen_height_get(PyGPUOffScreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_height(self->ofs));
}

PyDoc_STRVAR(GPUOffScreen_framebuffer_object_doc, "Framebuffer object.\n\n:type: GLuint");
static PyObject *GPUOffScreen_framebuffer_object_get(PyGPUOffScreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_fb_object(self->ofs));
}

PyDoc_STRVAR(GPUOffScreen_color_object_doc, "Color object.\n\n:type: GLuint");
static PyObject *GPUOffScreen_color_object_get(PyGPUOffScreen *self, void *UNUSED(type))
{
	return PyLong_FromLong(GPU_offscreen_color_object(self->ofs));
}

static PyGetSetDef GPUOffScreen_getseters[] = {
	{(char *)"color_object", (getter)GPUOffScreen_color_object_get, (setter)NULL, GPUOffScreen_color_object_doc, NULL},
	{(char *)"framebuffer_object", (getter)GPUOffScreen_framebuffer_object_get, (setter)NULL, GPUOffScreen_framebuffer_object_doc, NULL},
	{(char *)"width", (getter)GPUOffScreen_width_get, (setter)NULL, GPUOffScreen_width_doc, NULL},
	{(char *)"height", (getter)GPUOffScreen_height_get, (setter)NULL, GPUOffScreen_height_doc, NULL},
	{NULL, NULL, NULL, NULL, NULL}  /* Sentinel */
};

static int PyGPUOffScreen__tp_init(PyGPUOffScreen *self, PyObject *args, PyObject *kwargs)
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

static void PyGPUOffScreen__tp_dealloc(PyGPUOffScreen *self)
{
	if (self->ofs)
		GPU_offscreen_free(self->ofs);
	Py_TYPE(self)->tp_free((PyObject *)self);
}

PyDoc_STRVAR(py_GPUOffScreen_doc,
"GPUOffscreen(width, height) -> new GPU Offscreen object"
"initialized to hold a framebuffer object of ``width`` x ``height``.\n"
""
);
PyTypeObject PyGPUOffScreen_Type = {
	PyVarObject_HEAD_INIT(NULL, 0)
	"GPUOffScreen",                              /* tp_name */
	sizeof(PyGPUOffScreen),                      /* tp_basicsize */
	0,                                           /* tp_itemsize */
	/* methods */
	(destructor)PyGPUOffScreen__tp_dealloc,      /* tp_dealloc */
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
	py_GPUOffScreen_doc,                         /* Documentation string */
	NULL,                                        /* tp_traverse */
	NULL,                                        /* tp_clear */
	NULL,                                        /* tp_richcompare */
	0,                                           /* tp_weaklistoffset */
	NULL,                                        /* tp_iter */
	NULL,                                        /* tp_iternext */
	NULL,                                        /* tp_methods */
	NULL,                                        /* tp_members */
	GPUOffScreen_getseters,                      /* tp_getset */
	NULL,                                        /* tp_base */
	NULL,                                        /* tp_dict */
	NULL,                                        /* tp_descr_get */
	NULL,                                        /* tp_descr_set */
	0,                                           /* tp_dictoffset */
	(initproc)PyGPUOffScreen__tp_init,           /* tp_init */
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
	PyGPUOffScreen *PyOfs;
	int use_save;

	static const char *kwlist[] = {"offscreen_object", "use_save", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "Oi:offscreen_object_bind", (char **)(kwlist), &PyOfs, &use_save))
		return NULL;

	GPU_offscreen_bind(PyOfs->ofs, use_save);
	Py_RETURN_NONE;
}

PyMethodDef meth_offscreen_object_bind[] = {
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

	return PyObject_CallObject((PyObject *) &PyGPUOffScreen_Type, args);
}

PyMethodDef meth_offscreen_object_create[] = {
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
	PyGPUOffScreen *PyOfs;

	static const char *kwlist[] = {"offscreen_object", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "O:offscreen_object_unbind", (char **)(kwlist), &PyOfs))
		return NULL;

	GPU_offscreen_free(PyOfs->ofs);
	PyOfs->ofs = NULL;
	Py_RETURN_NONE;
}

PyMethodDef meth_offscreen_object_free[] = {
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
	PyGPUOffScreen *PyOfs;
	int use_restore;

	static const char *kwlist[] = {"offscreen_object", "use_restore", NULL};

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "Oi:offscreen_object_unbind", (char **)(kwlist), &PyOfs, &use_restore))
		return NULL;

	GPU_offscreen_unbind(PyOfs->ofs, use_restore);
	Py_RETURN_NONE;
}

PyMethodDef meth_offscreen_object_unbind[] = {
	{"offscreen_object_unbind", (PyCFunction)GPU_offscreen_object_unbind, METH_VARARGS | METH_KEYWORDS, GPU_offscreen_object_unbind_doc}
};
