# This sample script demonstrates how to place a custom icon on a button or
# menu entry.
#
# IMPORTANT NOTE: if you run this sample, there will be no icon in the button
# You need to replace the image path with a real existing one.
# For distributable addons, it is recommended to place the icons inside the
# addon folder and access it with bpy.utils.user_resource for portability
#
#
# Other use cases for UI-previews:
# - provide a fixed list of previews to select from
# - provide a dynamic list of preview (eg. calculated from reading a directory)
#
# For the above use cases, see the template 'ui_previews_dynamic_enum"


import os
import bpy


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
        pcoll = preview_collections["main"]
        my_icon = pcoll.get("my_icon")
        row.operator("render.render", icon_value=my_icon.icon_id)

        # my_icon.icon_id can be used in any UI function that accepts icon_value
        # try also setting text="" to get an icon only operator button


# We can store multiple preview collections here,
# however in this example we only store "main"
preview_collections = {}


def register():

    # Note that preview collections returned by bpy.utils.previews
    # are regular py objects - you can use them to store custom data.
    import bpy.utils.previews
    pcoll = bpy.utils.previews.new()
    pcoll.my_icons_dir = bpy.utils.user_resource('SCRIPTS', "addons") \
                        + "/my_addon_folder/icons/"
    # load a preview thumbnail of a file and store in the previews collection
    pcoll.load(
        # identifier
        "my_icon",
        # path to image
        pcoll.my_icons_dir + "icon-image.png",
        # file type to generate preview from. others are: MOVIE, FONT, BLEND
        'IMAGE')

    preview_collections["main"] = pcoll

    bpy.utils.register_class(PreviewsExamplePanel)


def unregister():

    for pcoll in preview_collections.values():
        bpy.utils.previews.remove(pcoll)
    preview_collections.clear()

    bpy.utils.unregister_class(PreviewsExamplePanel)


if __name__ == "__main__":
    register()

