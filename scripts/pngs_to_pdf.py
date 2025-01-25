from PIL import Image
import sys

image_list = []
for png_path in sys.argv[1:]:
    png = Image.open(png_path)
    image_list.append(png.convert("RGB"))

if len(image_list) > 0:
    image_list[0].save("merged_images.pdf", save_all=True, append_images=image_list[1:])
