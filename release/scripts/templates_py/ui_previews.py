import os
import bpy

# This sample script demonstrates a dynamic EnumProperty with custom icons.
# The EnumProperty is populated dynamically with thumbnails of the contents of
# a chosen directory in 'get_previews_from_folder'.
# Then, the same enum is displayed with different interfaces. Note that the
# generated icon previews do not have Blender IDs, which means that they can
# not be used with UILayout templates that require IDs, such as template_list
# and template_ID_preview.

# Other use cases:
# - make a fixed list of enum_items instead of calculating them in a function.
# - generate a single thumbnail to pass in a UILayout function as icon_value
#
# Example:
#    my_addon_icons = bpy.utils.previews.new("MyAddonIcons")
#    myicon = my_addon_icons.load("myicon", "/path/to/icon-image.png", 'IMAGE')
#    layout.operator("render.render", icon_value=int(myicon.icon_id))
#
# You can generate thumbnails of your own made icons to associate with an action


def get_previews_from_folder(self, context):
    wm = context.window_manager

    enum_items = []
    folder_path = wm.my_previews_folderpath

    # gets the already existing preview collection (defined in register func).
    pcoll = bpy.utils.previews.new("PreviewsInDirectory")

    if folder_path == pcoll.my_previews_folderpath:
        return pcoll.my_previews

    print("Scanning folder: %s" % folder_path)

    if folder_path and os.path.exists(folder_path):
        # scan the directory for png files
        dir_contents = os.listdir(folder_path)
        image_paths = []
        for c in dir_contents:
            if c.lower().endswith(".png"):
                image_paths.append(c)

        for idx, img_name in enumerate(image_paths):
            # generates a thumbnail preview for a file.
            # Also works with previews for 'MOVIE', 'BLEND' and 'FONT'
            path = folder_path + img_name
            thumb = pcoll.load(path, path, 'IMAGE')
            # enum item: (identifier, name, description, icon, number)
            enum_items.append((img_name, img_name, img_name, int(thumb.icon_id), idx))

    pcoll.my_previews = tuple(enum_items)
    pcoll.my_previews_folderpath = folder_path
    return pcoll.my_previews


class PreviewsExamplePanel(bpy.types.Panel):
    """Creates a Panel in the Object properties window"""
    bl_label = "Previews Example Panel"
    bl_idname = "OBJECT_PT_previews"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "object"

    def draw(self, context):
        layout = self.layout
        wm = context.window_manager

        row = layout.row()
        row.prop(wm, "my_previews_folderpath")

        row = layout.row()
        row.template_icon_view(wm, "my_previews")

        row = layout.row()
        row.prop(wm, "my_previews")


def register():
    bpy.types.WindowManager.my_previews_folderpath = \
        bpy.props.StringProperty(name="Folder Path", subtype='DIR_PATH')

    bpy.types.WindowManager.my_previews = \
        bpy.props.EnumProperty(items=get_previews_from_folder)

    # Note that preview collections returned by bpy.utils.previews are regular py objects - you can use them
    # to store custom data.
    #
    # This is especially useful here, since:
    # - It avoids us regenerating the whole enum over and over.
    # - It can store enumitems' strings (remember you have to keep those strings somewhere in py, else they get
    #   freed and blender references invalid memory!).
    pcoll = bpy.utils.previews.new("PreviewsInDirectory")
    pcoll.my_previews_folderpath = ""
    pcoll.my_previews = ()

    bpy.utils.register_class(PreviewsExamplePanel)


def unregister():
    del bpy.types.WindowManager.my_previews

    bpy.utils.previews.delete("PreviewsInDirectory")

    bpy.utils.unregister_class(PreviewsExamplePanel)


if __name__ == "__main__":
    register()
