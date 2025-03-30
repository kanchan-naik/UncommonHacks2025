import cv2
import numpy as np
import matplotlib.pyplot as plt
from skimage.feature import local_binary_pattern
from skimage.filters import gabor
import dlib

# Get skin mask based on HSV range and refine it using morphological operations
def get_skin_mask(image):
    # Convert to YCrCb for better skin tone detection
    image_YCrCb = cv2.cvtColor(image, cv2.COLOR_BGR2YCrCb)

    # Define Cr and Cb bounds dynamically for various skin tones
    min_YCrCb = np.array([0, 133, 77], dtype=np.uint8)
    max_YCrCb = np.array([255, 173, 127], dtype=np.uint8)

    # Create skin mask
    skin_mask = cv2.inRange(image_YCrCb, min_YCrCb, max_YCrCb)

    # Refine the mask using morphological operations
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (7, 7))
    skin_mask = cv2.morphologyEx(skin_mask, cv2.MORPH_CLOSE, kernel, iterations=3)
    skin_mask = cv2.morphologyEx(skin_mask, cv2.MORPH_OPEN, kernel, iterations=2)

    return skin_mask

def get_face_landmarks(image):
    # Load pre-trained face detector and landmark predictor
    detector = dlib.get_frontal_face_detector()
    predictor_path = "shape_predictor_68_face_landmarks.dat"  # Download this model!
    predictor = dlib.shape_predictor(predictor_path)

    # Convert image to grayscale
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    faces = detector(gray)

    # If no faces detected, return None
    if len(faces) == 0:
        return None

    # Get landmarks for the first face
    landmarks = predictor(gray, faces[0])

    # Extract landmark points
    points = []
    for i in range(68):
        x, y = landmarks.part(i).x, landmarks.part(i).y
        points.append((x, y))

    return points


# Get the region of skin in the image using the refined mask
def get_skin_region(image, skin_mask):
    return cv2.bitwise_and(image, image, mask=skin_mask)


# Apply Canny edge detection to get skin texture
def get_textures_canny(skin_region):
    image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
    edges = cv2.Canny(image_gray, threshold1=100, threshold2=200)

    texture_map = np.zeros_like(image_gray)
    texture_map[edges > 0] = 1
    return edges, texture_map


# Apply Gabor filter to detect skin textures
def get_textures_gabor(skin_region):
    image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
    _, imag = gabor(image_gray, frequency=0.6)

    texture_map = np.zeros_like(image_gray)
    texture_map[imag > 0] = 1
    return imag, texture_map


# Apply Local Binary Pattern (LBP) to get texture pattern
def get_textures_lbp(skin_region):
    image_gray = cv2.cvtColor(skin_region, cv2.COLOR_BGR2GRAY)
    lbp = local_binary_pattern(image_gray, P=8, R=1, method="uniform")

    texture_map = np.zeros_like(image_gray)
    texture_map[lbp > 0] = 1
    return lbp, texture_map


# Combine skin mask with texture map to refine mask boundaries
def final_mask(skin_mask, texture_map):
    # Refine final mask by combining skin and texture map
    refined_mask = cv2.bitwise_and(skin_mask, texture_map.astype(np.uint8) * 255)

    # Apply morphological operations for final refinement
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (5, 5))
    refined_mask = cv2.morphologyEx(refined_mask, cv2.MORPH_CLOSE, kernel, iterations=2)

    return refined_mask

# Function to draw face contour based on landmarks
def draw_face_contour(image, face_points):
    # Add forehead points to the original face points
    extended_points = add_forehead_points(face_points, forehead_scale=0.01)

    # Get the convex hull of the extended face points
    hull = cv2.convexHull(np.array(extended_points, dtype=np.int32))

    # Draw the contour on a copy of the image
    contour_overlay = image.copy()
    cv2.polylines(contour_overlay, [hull], isClosed=True, color=(0, 255, 0), thickness=2)

    return contour_overlay, hull

def apply_seamless_foundation(image, skin_mask, foundation_color, hull, alpha=0.4, eye_mask=None, feather_size=8):
    # Create a foundation-colored overlay
    foundation_layer = np.full_like(image, foundation_color, dtype=np.uint8)

    # Create a mask of the convex hull to restrict where the foundation is applied
    hull_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)
    cv2.fillConvexPoly(hull_mask, hull, 255)

    # --- Feather edges softly without expanding the mask ---
    feather_size = max(1, int(feather_size))  # Ensure feather_size is valid
    if feather_size % 2 == 0:
        feather_size += 1  # Make feather_size odd

    # Erode the mask slightly to avoid expanding the size
    hull_mask_eroded = cv2.erode(hull_mask, np.ones((3, 3), np.uint8), iterations=1)

    # Apply Gaussian blur to create a smooth transition on the edges
    hull_mask_blurred = cv2.GaussianBlur(hull_mask_eroded, (feather_size, feather_size), 0)

    # Normalize to keep alpha blending between 0 and 1
    feather_mask = hull_mask_blurred.astype(np.float32) / 255.0
    feather_mask = np.expand_dims(feather_mask, axis=-1)  # Expand to 3 channels for blending

    # If an eye mask is provided, subtract the eye regions from the mask
    if eye_mask is not None:
        hull_mask = cv2.subtract(hull_mask, eye_mask)

    # Apply foundation where the hull mask is valid
    foundation_masked = cv2.bitwise_and(foundation_layer, foundation_layer, mask=hull_mask)

    # --- Blend the foundation with the original image using alpha and feathered mask ---
    result = image.copy()
    for c in range(3):  # Apply feathered blending for each channel (B, G, R)
        result[:, :, c] = (
            (foundation_masked[:, :, c] * feather_mask[:, :, 0] * alpha)
            + image[:, :, c] * (1 - feather_mask[:, :, 0] * alpha)
        ).astype(np.uint8)

    return result

def add_forehead_points(face_points, forehead_scale=1.5):
    # Get eyebrow points (landmarks 17-26) to estimate forehead position
    left_eyebrow_top = face_points[19]  # Top of left eyebrow
    right_eyebrow_top = face_points[24]  # Top of right eyebrow
    
    # Increase the forehead height by scaling factor
    forehead_height = int(forehead_scale * (right_eyebrow_top[1] - left_eyebrow_top[1]))

    # Center of forehead, extend it upwards
    forehead_center_x = (left_eyebrow_top[0] + right_eyebrow_top[0]) // 2
    forehead_center_y = left_eyebrow_top[1] - forehead_height

    # Define more points along the forehead to avoid a triangular shape
    forehead_points = [
        (left_eyebrow_top[0] - 20, left_eyebrow_top[1] - forehead_height // 2),  # Left edge of forehead
        (left_eyebrow_top[0] + 20, left_eyebrow_top[1] - forehead_height),        # Left center
        (forehead_center_x - 20, forehead_center_y),                              # Slightly left of center
        (forehead_center_x, forehead_center_y - 20),                              # Higher center
        (forehead_center_x + 20, forehead_center_y),                              # Slightly right of center
        (right_eyebrow_top[0] - 20, right_eyebrow_top[1] - forehead_height),      # Right center
        (right_eyebrow_top[0] + 20, right_eyebrow_top[1] - forehead_height // 2), # Right edge of forehead
    ]

    # Add the forehead points to the original face points
    extended_face_points = np.vstack([face_points, np.array(forehead_points, dtype=np.int32)])
    return extended_face_points

def create_eye_mask(image, left_eye_contour, right_eye_contour):
    # Create an empty mask the same size as the image
    eye_mask = np.zeros(image.shape[:2], dtype=np.uint8)

    # Draw filled contours to mask the eye regions
    cv2.fillPoly(eye_mask, [left_eye_contour], 255)
    cv2.fillPoly(eye_mask, [right_eye_contour], 255)

    return eye_mask

def get_eye_contours(face_points):
    # Indices for left and right eyes from the 68-point model
    left_eye_points = face_points[36:42]  # Landmarks 36-41
    right_eye_points = face_points[42:48]  # Landmarks 42-47

    # Create eye contours as numpy arrays
    left_eye_contour = np.array(left_eye_points, dtype=np.int32)
    right_eye_contour = np.array(right_eye_points, dtype=np.int32)

    return left_eye_contour, right_eye_contour

