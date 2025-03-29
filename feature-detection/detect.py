import cv2
import numpy as np
import matplotlib.pyplot as plt
from skimage.feature import local_binary_pattern
from skimage.filters import gabor

def get_skin_mask(image):
  # bounds of what pixels are considered skin
  min_HSV = np.array([0, 20, 70], dtype = "uint8")
  max_HSV = np.array([20, 255, 255], dtype = "uint8")

  # convert to HSV
  imageHSV = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)
  return cv2.inRange(imageHSV, min_HSV, max_HSV)

def get_skin_region(image, skin_mask):
    return cv2.bitwise_and(image, image, mask = skin_mask)

def get_textures_canny(skin_region):
  image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
  edges = cv2.Canny(image_gray, threshold1=100, threshold2=200)

  texture_map = np.zeros_like(image_gray)
  texture_map[edges > 0] = 1
  return edges, texture_map

def get_textures_gabor(skin_region):
  image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
  _, imag = gabor(image_gray, frequency=0.6)

  texture_map = np.zeros_like(image_gray)
  texture_map[imag > 0] = 1
  return imag, texture_map

def get_textures_lbp(skin_region):
  image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
  lbp = local_binary_pattern(image_gray, P=8, R=1, method='uniform')

  texture_map = np.zeros_like(image_gray)
  texture_map[lbp > 0] = 1
  return lbp, texture_map

def final_mask(skin_mask, texture_map):
  return cv2.bitwise_and(skin_mask, texture_map)