import cv2
import numpy as np
import matplotlib.pyplot as plt
import detect
import os

# load image
os.mkdir("feature-detection/test/", exist_ok=True)
os.chdir("feature-detection/test/")
os.mkdir("gabor/", exist_ok=True)
os.mkdir("lbp/", exist_ok=True)
os.mkdir("canny/", exist_ok=True)
image = cv2.imread("image.png")

skin_mask = detect.get_skin_mask(image)
skin_region = detect.get_skin_region(image, skin_mask)

plt.imsave("skin_mask.png", skin_mask)
plt.imsave("skin_region.png", skin_region)

imag, texture_map = detect.get_textures_gabor(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)

plt.imsave("gabor/imag.png", imag)
plt.imsave("gabor/texture_map.png", texture_map)
plt.imsave("gabor/final_skin_mask.png", final_skin_mask)

lbp, texture_map = detect.get_textures_lbp(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)

plt.imsave("lbp/lbp.png", lbp)
plt.imsave("lbp/texture_map.png", texture_map)
plt.imsave("lbp/final_skin_mask.png", final_skin_mask)

edges, texture_map = detect.get_textures_canny(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)

plt.imsave("canny/edges.png", edges)
plt.imsave("canny/texture_map.png", texture_map)
plt.imsave("canny/final_skin_mask.png", final_skin_mask)