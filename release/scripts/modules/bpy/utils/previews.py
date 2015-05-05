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
    "remove",
    "clear",
    )

from bpy.app import _previews


# High-level previews manager.
class BPyPreviewsCollection(dict):
    """
    Dict-like class of previews.
    """

    # Internal notes:
    # - keys in the dict are stored by name
    # - values are instances of bpy.types.Preview
    # - Blender's internal 'PreviewImage' struct uses 'self._coll_name' prefix.

    def __init__(self, name):
        super().__init__()
        self._coll_name = name
        _previews_names_used.add(name)

    def __del__(self):
        if self._coll_name not in _previews_names_used:
            return

        raise Warning("%r: left open, remove with 'bpy.utils.previews.remove()'")
        self.close()

    def _gen_key(self, name):
        return ":".join((self._coll_name, name))

    def new(self, name):
        return self.setdefault(name, _previews.new(self._gen_key(name)))
    new.__doc__ = _previews.new.__doc__

    def load(self, name, path, path_type, force_reload=False):
        pkey = self._gen_key(name)
        if force_reload:
            self[name] = p = _previews.load(pkey, path, path_type, True)
            return p
        else:
            return self.setdefault(name, _previews.load(pkey, path, path_type, False))
    load.__doc__ = _previews.load.__doc__

    def release(self, name):
        p = self.pop(name, None)
        if p is not None:
            _previews.release(self._gen_key(name))
    release.__doc__ = _previews.release.__doc__

    def clear(self):
        for name in self.keys():
            _previews.release(self._gen_key(name))
        super().clear()

    def close(self):
        self.clear()
        _previews_names_used.remove(self._coll_name)

    def __delitem__(self, key):
        return self.release(key)

    def __repr__(self):
        return "<PreviewsCollection '%s'>\n\tPreviews: %s" % (self._coll_name, super().__repr__())


_previews_names_used = set()


def new(name):
    """
    Return a new preview collection, or existing one if 'name' already exists.
    """

    if name in _previews_names_used:
        raise Exception("Preview %r already exists!" % name)

    p = BPyPreviewsCollection(name)
    return p


def remove(p):
    """
    Remove the specified previews collection.
    """
    p.close()

