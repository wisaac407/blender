# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# <pep8 compliant>

"""
This module contains utility functions to handle custom previews.

It behaves as a high-level 'cached' previews manager.

This allows addons to generate their own previews, and use them as icons in UI widgets
('icon_value' of UILayout functions).
"""

__all__ = (
    "new",
    "delete",
    "clear",
    )


# High-level previews manager.
class BPyPreviewsCollection(dict):
    """
    Dict-like class of previews.
    """
    from _bpy import app as _app

    def __init__(self, name):
        super().__init__()
        self._coll_name = name

    def __del__(self):
        app = self.__class__._app
        for name in self.keys():
            app._previews.release(self._gen_key(name))

    def _gen_key(self, name):
        return self._coll_name + name

    def new(self, name):
        app = self.__class__._app
        return self.setdefault(name, app._previews.new(self._gen_key(name)))
    new.__doc__ = _app._previews.new.__doc__

    def load(self, name, path, path_type, force_reload=False):
        app = self.__class__._app
        pkey = self._gen_key(name)
        if force_reload:
            self[name] = p = app._previews.load(pkey, path, path_type, True)
            return p
        else:
            return self.setdefault(name, app._previews.load(pkey, path, path_type, False))
    load.__doc__ = _app._previews.load.__doc__

    def release(self, name):
        app = self.__class__._app
        p = self.pop(name, None)
        if p is not None:
            del p
            app._previews.release(self._gen_key(name))
    release.__doc__ = _app._previews.release.__doc__

    def clear(self):
        app = self.__class__._app
        for name in self.keys():
            app._previews.release(self._gen_key(name))
        super().clear()

    def __delitem__(self, key):
        return self.release(key)

    def __repr__(self):
        return "<PreviewsCollection '%s'>\n\tPreviews: %s" % (self._coll_name, super().__repr__())


_previews_collections = {}


def new(name):
    """
    Return a new preview collection, or existing one if 'name' already exists.
    """
    return _previews_collections.setdefault(name, BPyPreviewsCollection(name))


def _remove(name):
    return _previews_collections.pop(name, None)


def delete(name):
    """
    Delete specified previews collection.
    """
    pcoll = _remove(name)
    if pcoll is not None:
        del pcoll


def clear(self):
    """
    Delete all previews collections.
    """
    for pcoll in _previews_collections.values():
        del pcoll
    _previews_collections.clear()
