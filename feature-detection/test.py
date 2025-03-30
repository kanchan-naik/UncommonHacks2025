import cv2
import numpy as np
import matplotlib.pyplot as plt
import detect
import os

# Define paths and create necessary directories
base_dir = os.path.dirname(os.path.abspath(__file__))
test_dir = os.path.join(base_dir, "test")
gabor_dir = os.path.join(test_dir, "gabor")
lbp_dir = os.path.join(test_dir, "lbp")
canny_dir = os.path.join(test_dir, "canny")

# Create output folders if they don't exist
os.makedirs(test_dir, exist_ok=True)
os.makedirs(gabor_dir, exist_ok=True)
os.makedirs(lbp_dir, exist_ok=True)
os.makedirs(canny_dir, exist_ok=True)

# Load image
image_path = os.path.join(base_dir, "image.png")
image = cv2.imread(image_path)

# Check if image is loaded successfully
if image is None:
    print(f"❌ Error: Could not load image at {image_path}")
    exit()

# --- Step 1: Get refined skin mask ---
skin_mask = detect.get_skin_mask(image)
skin_region = detect.get_skin_region(image, skin_mask)

# Save initial skin mask and skin region
plt.imsave(os.path.join(test_dir, "skin_mask.png"), skin_mask, cmap="gray")
plt.imsave(os.path.join(test_dir, "skin_region.png"), cv2.cvtColor(skin_region, cv2.COLOR_BGR2RGB))

# --- Step 2: Detect facial landmarks and define face boundary ---
face_points = detect.get_face_landmarks(image)

if face_points:
    contour_overlay, hull = detect.draw_face_contour(image, face_points)
    plt.imsave(os.path.join(test_dir, "contour_overlay.png"), contour_overlay)

    # Create eye mask using detected landmarks
    left_eye_contour, right_eye_contour = detect.get_eye_contours(face_points)
    eye_mask = detect.create_eye_mask(image, left_eye_contour, right_eye_contour)
    plt.imsave(os.path.join(test_dir, "eye_mask.png"), eye_mask, cmap="gray")

    # --- Step 3: Apply foundation with adjustable opacity, excluding eyes ---
    foundation_color = (70, 130, 180)  # Foundation color in BGR
    alpha_value = 0.2  # Adjust this value between 0 and 1 for desired opacity
    foundation_overlay = detect.apply_seamless_foundation(image, skin_mask, foundation_color, hull, alpha=alpha_value, eye_mask=eye_mask)
    plt.imsave(os.path.join(test_dir, "foundation_overlay2.png"), cv2.cvtColor(foundation_overlay, cv2.COLOR_BGR2RGB))

# --- Step 4: Process and save Gabor textures ---
imag, texture_map = detect.get_textures_gabor(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)
plt.imsave(os.path.join(gabor_dir, "imag.png"), imag, cmap="gray")
plt.imsave(os.path.join(gabor_dir, "texture_map.png"), texture_map, cmap="gray")
plt.imsave(os.path.join(gabor_dir, "final_skin_mask.png"), final_skin_mask, cmap="gray")

# --- Step 5: Process and save LBP textures ---
lbp, texture_map = detect.get_textures_lbp(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)
plt.imsave(os.path.join(lbp_dir, "lbp.png"), lbp, cmap="gray")
plt.imsave(os.path.join(lbp_dir, "texture_map.png"), texture_map, cmap="gray")
plt.imsave(os.path.join(lbp_dir, "final_skin_mask.png"), final_skin_mask, cmap="gray")

# --- Step 6: Process and save Canny textures ---
edges, texture_map = detect.get_textures_canny(skin_region)
final_skin_mask = detect.final_mask(skin_mask, texture_map)
plt.imsave(os.path.join(canny_dir, "edges.png"), edges, cmap="gray")
plt.imsave(os.path.join(canny_dir, "texture_map.png"), texture_map, cmap="gray")
plt.imsave(os.path.join(canny_dir, "final_skin_mask.png"), final_skin_mask, cmap="gray")

# --- Apply eye shadow after foundation ---
shadow_color = (0, 0, 255)  # Correct BGR for red
shadow_opacity = 0.2  # Change for intensity
feather_size = 2  # Feather edges smoothly

# Apply eye shadow
eye_shadow_overlay = detect.apply_eye_shadow(foundation_overlay, face_points, shadow_color, alpha=shadow_opacity)
plt.imsave(os.path.join(test_dir, "eye_shadow_overlay.png"), cv2.cvtColor(eye_shadow_overlay, cv2.COLOR_BGR2RGB))

print("✅ All images processed and saved successfully!")
