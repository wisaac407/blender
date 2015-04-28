import os
import bpy

# This sample script demonstrates a dynamic EnumProperty with custom icons.
# The EnumProperty is populated dynamically with thumbnails of the contents of
# a chosen directory in 'get_previews_from_folder'.
# Then, the same enum is displayed with different interfaces. Note that the
# generated icon previews do not have Blender IDs, which means that they can
# not be used with UILayout templates that require IDs, such as template_list
# and template_ID_preview.

# Other usecases:
# - make a fixed list of enum_items instead of calculating them in a function.
# - generate a single thumbnail to pass in a UILayout function as icon_value
# example:
#    my_addon_icons = bpy.utils.previews.new("MyAddonIcons")
#    myicon = my_addon_icons.load("myicon", "/path/to/icon-image.png", 'IMAGE')
#    layout.operator("render.render", icon_value=int(myicon.icon_id))
# You can generate thumbnails of your own made icons to associate with an action


def get_previews_from_folder(self, context):

    enum_items = []
    folder_path = context.window_manager.my_previews_folderpath
    print("rescanning folder: %s" % folder_path)

    if folder_path:
        # scan the directory for png files
        dir_contents = os.listdir(folder_path)
        image_paths = []
        for c in dir_contents:
            if c.endswith(".png") or c.endswith(".PNG"):
                image_paths.append(c)

        # gets the already existing preview collection, otherwise it is created
        prevs = bpy.utils.previews.new("PreviewsInDirectory")

        for idx, img_name in enumerate(image_paths):
            # generates a thumbnail preview for a file.
            # Also works with previews for 'MOVIE', 'BLEND' and 'FONT'
            thumb = prevs.load(img_name, folder_path + img_name, 'IMAGE')
            enum_items.append(
                # enum item: (identifier, name, description, icon, number)
                (img_name, img_name, img_name, int(thumb.icon_id), idx+1))

    return enum_items


class PreviewsExamplePanel(bpy.types.Panel):
    """Creates a Panel in the Object properties window"""
    bl_label = "Previews Example Panel"
    bl_idname = "OBJECT_PT_previews"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "object"

    def draw(self, context):
        layout = self.layout

        obj = context.object

        row = layout.row()
        row.prop(context.window_manager, "my_previews_folderpath")

        row = layout.row()
        row.template_icon_view(context.window_manager, "my_previews")

        row = layout.row()
        row.prop(context.window_manager, "my_previews")


def register():
    bpy.types.WindowManager.my_previews_folderpath = \
        bpy.props.StringProperty(name="Folder Path", subtype='DIR_PATH')

    bpy.types.WindowManager.my_previews = \
        bpy.props.EnumProperty(items=get_previews_from_folder)

    bpy.utils.previews.new("PreviewsInDirectory")

    bpy.utils.register_class(PreviewsExamplePanel)


def unregister():
    del bpy.types.WindowManager.my_previews

    bpy.utils.previews.delete("PreviewsInDirectory")

    bpy.utils.unregister_class(PreviewsExamplePanel)


if __name__ == "__main__":
    register()
