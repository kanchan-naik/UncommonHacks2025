import cv2
import numpy as np
import matplotlib.pyplot as plt
import detect
import os
import argparse
import sys

# ----------------- Define Paths & Create Necessary Directories -----------------

base_dir = os.path.dirname(os.path.abspath(__file__))
test_dir = os.path.join(base_dir, "test")
gabor_dir = os.path.join(test_dir, "gabor")
lbp_dir = os.path.join(test_dir, "lbp")
canny_dir = os.path.join(test_dir, "canny")

def create_output_dirs(base_dir, dirs):
    for directory in dirs:
        os.makedirs(os.path.join(base_dir, directory), exist_ok=True)

create_output_dirs(base_dir, ["test", "gabor", "lbp", "canny"])

# ----------------- Convert Hex to BGR for OpenCV -----------------
def hex_to_bgr(hex_color):
    hex_color = hex_color.lstrip("#")
    return tuple(int(hex_color[i : i + 2], 16) for i in (4, 2, 0))  # BGR format

# ----------------- Load Image & Validate -----------------

def load_image(image_path):
    image = cv2.imread(image_path)
    if image is None:
        print(f"❌ Error: Could not load image at {image_path}")
        exit()
    return image


# ----------------- Parse CLI Arguments -----------------
def parse_args():
    print("args", sys.argv)
    if len(sys.argv) < 11:  # Update to 11 parameters
        print(
            "❌ Usage: python3 test.py <image_path> <foundation_present> <foundation_color> "
            "<lipstick_present> <lipstick_color> <eyeshadow_present> <eyeshadow_color> "
            "<blush_present> <blush_color> <concealer_present> <concealer_color>"
        )
        exit()

    image_path = sys.argv[1]
    foundation_present = int(sys.argv[2])
    foundation_color = sys.argv[3]
    lipstick_present = int(sys.argv[4])
    lipstick_color = sys.argv[5]
    eyeshadow_present = int(sys.argv[6])
    eyeshadow_color = sys.argv[7]
    blush_present = int(sys.argv[8])
    blush_color = sys.argv[9]
    concealer_present = int(sys.argv[10])
    concealer_color = sys.argv[11]

    return (
        image_path,
        foundation_present,
        foundation_color,
        lipstick_present,
        lipstick_color,
        eyeshadow_present,
        eyeshadow_color,
        blush_present,
        blush_color,
        concealer_present,
        concealer_color,
    )

# ----------------- Step 1: Get Skin Mask -----------------

def process_skin_mask(image):
    skin_mask = detect.get_skin_mask(image)
    skin_region = detect.get_skin_region(image, skin_mask)
    plt.imsave(os.path.join(test_dir, "skin_mask.png"), skin_mask, cmap="gray")
    plt.imsave(os.path.join(test_dir, "skin_region.png"), cv2.cvtColor(skin_region, cv2.COLOR_BGR2RGB))
    return skin_mask, skin_region


#skin_mask, skin_region = process_skin_mask(image)


# ----------------- Step 2: Detect Face & Create Eye Mask -----------------

def get_face_and_eye_mask(image):
    face_points = detect.get_face_landmarks(image)
    if not face_points:
        print("❌ No face detected. Exiting...")
        exit()
    contour_overlay, hull = detect.draw_face_contour(image, face_points)
    plt.imsave(os.path.join(test_dir, "contour_overlay.png"), contour_overlay)

    left_eye_contour, right_eye_contour = detect.get_eye_contours(face_points)
    eye_mask = detect.create_eye_mask(image, left_eye_contour, right_eye_contour)
    plt.imsave(os.path.join(test_dir, "eye_mask.png"), eye_mask, cmap="gray")

    return face_points, eye_mask, hull


#face_points, eye_mask, hull = get_face_and_eye_mask(image)


# ----------------- Step 3: Apply Foundation -----------------

def apply_foundation(image, skin_mask, hull, face_points, eye_mask, foundation_bgr):
    alpha_value = 0.1  # Opacity
    foundation_overlay = detect.apply_seamless_foundation(
        image, skin_mask, foundation_bgr, hull, alpha=alpha_value, eye_mask=eye_mask, face_points=face_points
    )
    plt.imsave(os.path.join(test_dir, "foundation_overlay.png"), cv2.cvtColor(foundation_overlay, cv2.COLOR_BGR2RGB))
    return foundation_overlay


#foundation_overlay = apply_foundation(image, skin_mask, hull, face_points, eye_mask)


# ----------------- Step 4: Apply Eye Shadow -----------------

def apply_eye_shadow(image, face_points, eye_mask, eyeshadow_bgr):
    shadow_opacity = 0.2
    eye_shadow_overlay = detect.apply_eye_shadow(
        image, face_points, eyeshadow_bgr, alpha=shadow_opacity, eye_mask=eye_mask
    )
    plt.imsave(os.path.join(test_dir, "eye_shadow_overlay.png"), cv2.cvtColor(eye_shadow_overlay, cv2.COLOR_BGR2RGB))
    return eye_shadow_overlay


#eye_shadow_overlay = apply_eye_shadow(foundation_overlay, face_points, eye_mask)


# ----------------- Step 5: Apply Lipstick -----------------

def apply_lipstick(image, face_points, lipstick_bgr):
    lip_opacity = 0.5
    lipstick_overlay = detect.apply_lipstick(
        image, face_points, lipstick_bgr, alpha=lip_opacity
    )
    plt.imsave(os.path.join(test_dir, "lipstick_overlay.png"), cv2.cvtColor(lipstick_overlay, cv2.COLOR_BGR2RGB))
    return lipstick_overlay


#lipstick_overlay = apply_lipstick(eye_shadow_overlay, face_points)


# ----------------- Step 6: Apply Blush -----------------

def apply_blush(image, face_points, eye_mask, blush_bgr):
    blush_opacity = 0.4
    blush_overlay = detect.apply_blush(
        image, face_points, blush_bgr, alpha=blush_opacity, eye_mask=eye_mask
    )
    plt.imsave(os.path.join(test_dir, "blush_overlay.png"), cv2.cvtColor(blush_overlay, cv2.COLOR_BGR2RGB))
    return blush_overlay


#blush_overlay = apply_blush(lipstick_overlay, face_points, eye_mask)


# ----------------- Step 7: Apply Concealer/Highlighter -----------------

def apply_concealer(image, face_points, eye_mask, concealer_bgr):
    concealer_opacity = 0.2
    concealer_overlay = detect.apply_concealer(
        image, face_points, concealer_bgr, alpha=concealer_opacity, eye_mask=eye_mask
    )
    plt.imsave(os.path.join(test_dir, "concealer_overlay.png"), cv2.cvtColor(concealer_overlay, cv2.COLOR_BGR2RGB))
    return concealer_overlay


#concealer_overlay = apply_concealer(blush_overlay, face_points, eye_mask)


# ----------------- Main Makeup Pipeline -----------------
if __name__ == "__main__":
    (
        image_path,
        foundation_present,
        foundation_color,
        lipstick_present,
        lipstick_color,
        eyeshadow_present,
        eyeshadow_color,
        blush_present,
        blush_color, 
        concealer_present, 
        concealer_color
    ) = parse_args()

    # Load the image
    image = load_image(image_path)

    # Convert hex colors to BGR
    foundation_bgr = hex_to_bgr(foundation_color)
    lipstick_bgr = hex_to_bgr(lipstick_color)
    eyeshadow_bgr = hex_to_bgr(eyeshadow_color)
    blush_bgr = hex_to_bgr(blush_color)
    concealer_bgr = hex_to_bgr(concealer_color)


    # --- Process Skin Mask ---
    skin_mask, skin_region = process_skin_mask(image)

    # --- Detect Face and Create Eye Mask ---
    face_points, eye_mask, hull = get_face_and_eye_mask(image)

    # --- Apply Foundation if present ---
    if foundation_present:
        foundation_overlay = apply_foundation(
            image, skin_mask, hull, face_points, eye_mask, foundation_bgr
        )
    else:
        foundation_overlay = image

    # --- Apply Eyeshadow if present ---
    if eyeshadow_present:
        eye_shadow_overlay = apply_eye_shadow(
        foundation_overlay, face_points, eye_mask, eyeshadow_bgr
        )
    else:
        eye_shadow_overlay = foundation_overlay

    # --- Apply Lipstick if present ---
    if lipstick_present:
        lipstick_overlay = apply_lipstick(eye_shadow_overlay, face_points, lipstick_bgr)
    else:
        lipstick_overlay = eye_shadow_overlay

    # --- Apply Blush if present ---
    if blush_present:
        blush_overlay = apply_blush(lipstick_overlay, face_points, eye_mask, blush_bgr)
    else:
        blush_overlay = lipstick_overlay

    # --- Apply Concealer if present ---
    if concealer_present:
        final_image = apply_concealer(blush_overlay, face_points, eye_mask, concealer_bgr)
    else:
        final_image = blush_overlay

    # Save the final result
    output_path = os.path.join("test", "final_makeup_result.png")
    plt.imsave(output_path, cv2.cvtColor(final_image, cv2.COLOR_BGR2RGB))
    print(f"✅ Makeup applied and saved to {output_path}")
