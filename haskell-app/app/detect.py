import cv2
import numpy as np
import matplotlib.pyplot as plt
from skimage.feature import local_binary_pattern
from skimage.filters import gabor
import dlib
from scipy.spatial import distance as dist

# Get face width (distance between left and right jawline points)
def get_face_width(face_points):
    return dist.euclidean(face_points[0], face_points[16])

# Get eye width
def get_eye_width(face_points, eye='left'):
    if eye == 'left':
        return dist.euclidean(face_points[36], face_points[39])
    else:
        return dist.euclidean(face_points[42], face_points[45])
    
def get_face_height(face_points):
    """
    Calculate the height of the face dynamically.
    Height is measured from the midpoint between the eyebrows (landmark 27)
    to the bottom of the chin (landmark 8).
    """
    # Landmark 27 is the midpoint between the eyebrows
    brow_midpoint = face_points[27]

    # Landmark 8 is the bottom of the chin
    chin_point = face_points[8]

    # Calculate Euclidean distance to determine face height
    face_height = np.linalg.norm(np.array(brow_midpoint) - np.array(chin_point))
    
    return face_height

# Get forehead height (distance between eyebrows and top of forehead)
def get_forehead_height(face_points):
    left_eyebrow_top = face_points[19]
    right_eyebrow_top = face_points[24]
    return dist.euclidean(left_eyebrow_top, right_eyebrow_top) / 2

# Get lip width (distance between corners of lips)
def get_lip_width(face_points):
    return dist.euclidean(face_points[48], face_points[54])

# Get cheek width (distance between cheeks)
def get_cheek_width(face_points):
    return dist.euclidean(face_points[2], face_points[14])

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
    predictor_path = "app/shape_predictor_68_face_landmarks.dat"  # Download this model!
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

def apply_eye_shadow(image, face_points, shadow_color, alpha=0.3, feather_size=None, extend_outer=None, extend_upper=None, eye_mask=None):
    # Define indices for upper eyelid points
    left_upper_lid = face_points[36:42]  # Upper part of left eye (landmarks 36-41)
    right_upper_lid = face_points[42:48]  # Upper part of right eye (landmarks 42-47)

     # --- Dynamically calculate eye width and height ---
    eye_width = np.linalg.norm(np.array(face_points[39]) - np.array(face_points[36]))  # Left eye width
    eye_height = np.linalg.norm(np.array(face_points[37]) - np.array(face_points[41]))  # Left eye height

    # --- Scale feather size, extend_outer, and extend_upper dynamically ---
    if feather_size is None:
        feather_size = max(5, int(0.50 * eye_width))  # Feather 5% of eye width

    if extend_outer is None:
        extend_outer = int(0.5 * eye_width)  # Extend outer wing by 20% of eye width
    if extend_upper is None:
        extend_upper = int(1.5 * eye_height)  # Extend upper height by 50% of eye height

    # Function to shift points upward for the arc and outward for the wing
    def shift_points(points, shift_y=7, shift_x=0):
        return [(x + shift_x, y - shift_y) for (x, y) in points]

    # Apply upward shift to create a lifted arc
    left_upper_lid_shifted = shift_points(left_upper_lid, shift_y=extend_upper, shift_x=-extend_outer)
    right_upper_lid_shifted = shift_points(right_upper_lid, shift_y=extend_upper, shift_x=extend_outer)

    # Extend outer corners for wing effect
    left_outer_corner = (
        left_upper_lid[-1][0] + extend_outer,
        left_upper_lid[-1][1] - extend_upper // 3,
    )
    right_outer_corner = (
        right_upper_lid[0][0] - extend_outer,
        right_upper_lid[0][1] - extend_upper // 3,
    )

    # Add the extended points to the shifted eyelid points
    left_upper_lid_shifted.append(left_outer_corner)
    right_upper_lid_shifted.append(right_outer_corner)

    # Create convex hull for smooth contour of the eyeshadow
    left_shadow_hull = cv2.convexHull(np.array(left_upper_lid_shifted, dtype=np.int32))
    right_shadow_hull = cv2.convexHull(np.array(right_upper_lid_shifted, dtype=np.int32))

    # Create mask for eyeshadow
    shadow_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)
    cv2.fillConvexPoly(shadow_mask, left_shadow_hull, 255)
    cv2.fillConvexPoly(shadow_mask, right_shadow_hull, 255)

    # Subtract the eye mask to avoid overlapping eyes
    if eye_mask is not None:
        shadow_mask = cv2.subtract(shadow_mask, eye_mask)

    # Feather the shadow edges to make it blend smoothly
    if feather_size > 0:
        # Make feather_size odd if it's not
        if feather_size % 2 == 0:
            feather_size += 1
        shadow_mask = cv2.GaussianBlur(shadow_mask, (feather_size, feather_size), 0)

    # Create the shadow overlay
    shadow_layer = np.full_like(image, shadow_color, dtype=np.uint8)
    shadow_masked = cv2.bitwise_and(shadow_layer, shadow_layer, mask=shadow_mask)

    # --- Blend the shadow with the original image using alpha ---
    result = image.copy()
    for c in range(3):  # Blend each channel separately (B, G, R)
        result[:, :, c] = (
            (shadow_masked[:, :, c] * (shadow_mask.astype(np.float32) / 255.0) * alpha)
            + image[:, :, c] * (1 - (shadow_mask.astype(np.float32) / 255.0) * alpha)
        ).astype(np.uint8)


    return result

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

def apply_seamless_foundation(
    image, skin_mask, foundation_color, hull, alpha=0.4, eye_mask=None, 
    feather_size=None, face_points=None
):
    # --- Dynamically adjust feather size based on face width ---
    if face_points is not None and feather_size is None:
        face_width = get_face_width(face_points)
        feather_size = max(5, int(0.02 * face_width))  # Feather size as 2% of face width
    
    # Ensure feather_size is valid and odd (required for GaussianBlur)
    feather_size = max(1, int(feather_size))
    if feather_size % 2 == 0:
        feather_size += 1  # Make feather_size odd for GaussianBlur

    # --- Create a foundation-colored overlay ---
    foundation_layer = np.full_like(image, foundation_color, dtype=np.uint8)

    # Create a mask of the convex hull to restrict where the foundation is applied
    hull_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)
    cv2.fillConvexPoly(hull_mask, hull, 255)

    # --- Erode the mask slightly to avoid expanding the size ---
    erosion_size = max(1, feather_size // 6)  # Erode by a fraction of feather size
    hull_mask_eroded = cv2.erode(hull_mask, np.ones((erosion_size, erosion_size), np.uint8), iterations=1)

    # --- Feather the edges using Gaussian blur ---
    hull_mask_blurred = cv2.GaussianBlur(hull_mask_eroded, (feather_size, feather_size), 0)

    # Normalize feather mask for blending
    feather_mask = hull_mask_blurred.astype(np.float32) / 255.0
    feather_mask = np.expand_dims(feather_mask, axis=-1)  # Add channel for blending

    # --- If eye mask is provided, subtract eye regions from the mask ---
    if eye_mask is not None:
        hull_mask = cv2.subtract(hull_mask, eye_mask)

    # --- Apply foundation only where the hull mask is valid ---
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

def create_eye_mask(image, left_eye_contour, right_eye_contour, feather_size = None):
    # Create an empty mask the same size as the image
    eye_mask = np.zeros(image.shape[:2], dtype=np.uint8)

    # Draw filled contours to mask the eye regions
    cv2.fillPoly(eye_mask, [left_eye_contour], 255)
    cv2.fillPoly(eye_mask, [right_eye_contour], 255)

     # --- Dynamically calculate feather_size based on eye width ---
    if feather_size is None:
        # Estimate eye width as the distance between outer eye corners
        eye_width = np.linalg.norm(np.array(left_eye_contour[3]) - np.array(left_eye_contour[0]))
        
        # Set feather size as ~5% of eye width, ensuring it's at least 5 pixels
        feather_size = max(5, int(0.08 * eye_width))

    # --- Smooth the eye mask by dilating and blurring ---
    if feather_size > 0:
        # Make feather size odd if necessary
        if feather_size % 2 == 0:
            feather_size += 1

        dilation_size = max(3, int(0.07 * eye_width))
        dilation_kernel = np.ones((dilation_size, dilation_size), np.uint8)

        # Slightly dilate before blurring to prevent shrinking the mask
        eye_mask = cv2.dilate(eye_mask, dilation_kernel, iterations=1)
        eye_mask = cv2.GaussianBlur(eye_mask, (feather_size, feather_size), 0)

    return eye_mask

def get_eye_contours(face_points):
    # Indices for left and right eyes from the 68-point model
    left_eye_points = face_points[36:42]  # Landmarks 36-41
    right_eye_points = face_points[42:48]  # Landmarks 42-47

    # Create eye contours as numpy arrays
    left_eye_contour = np.array(left_eye_points, dtype=np.int32)
    right_eye_contour = np.array(right_eye_points, dtype=np.int32)

    return left_eye_contour, right_eye_contour

def get_lip_contours(face_points, lip_shift=-2):
    # Define landmark points for the upper and lower lips
    upper_lip_points = face_points[48:55] + [face_points[48]]  # Landmarks 48-54
    lower_lip_points = face_points[54:61] + [face_points[54]]  # Landmarks 54-60

    # Shift points slightly to adjust for thickness of lipstick
    #def shift_points(points, shift_y=lip_shift):
    #    return [(x, y + shift_y) for (x, y) in points]

    # Apply slight downward shift for a fuller lip effect
    #upper_lip_shifted = shift_points(upper_lip_points, shift_y=-lip_shift)
    #lower_lip_shifted = shift_points(lower_lip_points, shift_y=lip_shift)

    # Create convex hulls for upper and lower lips
    upper_lip_hull = cv2.convexHull(np.array(upper_lip_points, dtype=np.int32))
    lower_lip_hull = cv2.convexHull(np.array(lower_lip_points, dtype=np.int32))

    return upper_lip_hull, lower_lip_hull

def apply_lipstick(image, face_points, lip_color, alpha=0.4, feather_size=None):
    # Get upper and lower lip contours
    upper_lip_hull, lower_lip_hull = get_lip_contours(face_points)

    # Create mask for the lips
    lip_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)
    cv2.fillConvexPoly(lip_mask, upper_lip_hull, 255)
    cv2.fillConvexPoly(lip_mask, lower_lip_hull, 255)

    # --- Dynamically calculate feather_size based on lip width ---
    # Calculate lip width as the distance between the corners of the mouth
    lip_width = np.linalg.norm(np.array(face_points[54]) - np.array(face_points[48]))

    # Set feather size as 5% of lip width, ensuring it's at least 5 pixels
    if feather_size is None:
        feather_size = max(5, int(0.25 * lip_width))

    # --- Dynamically calculate dilation size ---
    # Set dilation size as 3% of lip width, ensuring at least 3 pixels
    dilation_size = max(2, int(0.01 * lip_width))
    dilation_kernel = np.ones((dilation_size, dilation_size), np.uint8)

    # --- Improve mask smoothness with dilation and feathering ---
    if feather_size > 0:
        # Ensure feather_size is odd for Gaussian blur
        if feather_size % 2 == 0:
            feather_size += 1

        # Dilate the mask slightly to smooth sharp edges
        lip_mask = cv2.dilate(lip_mask, dilation_kernel, iterations=1)

        # Feather the mask with Gaussian blur
        lip_mask = cv2.GaussianBlur(lip_mask, (feather_size, feather_size), 0)

    # Create lipstick overlay
    lipstick_layer = np.full_like(image, lip_color, dtype=np.uint8)
    lipstick_masked = cv2.bitwise_and(lipstick_layer, lipstick_layer, mask=lip_mask)

    # --- Blend the lipstick with the original image using alpha ---
    result = image.copy()
    for c in range(3):  # Blend each channel separately (B, G, R)
        result[:, :, c] = (
            (lipstick_masked[:, :, c] * (lip_mask.astype(np.float32) / 255.0) * alpha)
            + image[:, :, c] * (1 - (lip_mask.astype(np.float32) / 255.0) * alpha)
        ).astype(np.uint8)

    return result


def apply_blush(image, face_points, blush_color=(255, 102, 178), alpha=0.3, feather_size=None, eye_mask=None):
    # --- Dynamically calculate face width ---
    face_width = get_face_width(face_points)

    # Set feather size dynamically based on face width (6% of face width)
    if feather_size is None:
        feather_size = max(5, int(0.4 * face_width))  # At least 5 pixels to ensure smooth blending
    
    # Get cheekbone landmarks for defining the curve
    left_cheekbone_start = face_points[1]  # Near top of the ear
    left_cheekbone_mid = face_points[3]  # Mid cheek for curve
    left_cheekbone_end = (
        (face_points[30][0] + face_points[2][0]) // 2,
        (face_points[30][1] + face_points[2][1]) // 2,
    )  # Mid-nose area

    right_cheekbone_start = face_points[15]  # Near top of the ear
    right_cheekbone_mid = face_points[13]  # Mid cheek for curve
    right_cheekbone_end = (
        (face_points[30][0] + face_points[12][0]) // 2,
        (face_points[30][1] + face_points[12][1]) // 2,
    )  # Mid-nose area

    # Create curve points for each cheek
    left_blush_curve = np.array(
        [left_cheekbone_start, left_cheekbone_mid, left_cheekbone_end], dtype=np.int32
    )
    right_blush_curve = np.array(
        [right_cheekbone_start, right_cheekbone_mid, right_cheekbone_end], dtype=np.int32
    )

    # Create a blush mask
    blush_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)

    # Subtract the eye mask to avoid overlapping eyes
    if eye_mask is not None:
        blush_mask = cv2.subtract(blush_mask, eye_mask)

    # Fill a polygon along the curve to define blush area
    cv2.fillPoly(blush_mask, [left_blush_curve], 255)
    cv2.fillPoly(blush_mask, [right_blush_curve], 255)

    # --- Feather the blush mask for smooth blending ---
    if feather_size > 0:
        if feather_size % 2 == 0:
            feather_size += 1  # Ensure feather size is odd
        blush_mask = cv2.GaussianBlur(blush_mask, (feather_size, feather_size), 0)

    # Create blush overlay with the specified color
    blush_layer = np.full_like(image, blush_color, dtype=np.uint8)

    # Apply the blush mask to the overlay
    blush_masked = cv2.bitwise_and(blush_layer, blush_layer, mask=blush_mask)

    # Blend the blush with the original image
    result = image.copy()
    for c in range(3):  # Blend each channel separately (B, G, R)
        result[:, :, c] = (
            (blush_masked[:, :, c] * (blush_mask.astype(np.float32) / 255.0) * alpha)
            + image[:, :, c] * (1 - (blush_mask.astype(np.float32) / 255.0) * alpha)
        ).astype(np.uint8)

    return result

def apply_concealer(
    image, face_points, concealer_color=(255, 255, 255), alpha=0.5, feather_size=None, extend_outer=None, extend_lower=None, eye_mask=None
):
    # --- Dynamically calculate eye width and height ---
    eye_width = np.linalg.norm(np.array(face_points[39]) - np.array(face_points[36]))  # Left eye width
    eye_height = np.linalg.norm(np.array(face_points[37]) - np.array(face_points[41]))  # Left eye height

    # --- Scale feather size, extend_outer, and extend_lower dynamically ---
    if feather_size is None:
        feather_size = max(5, int(0.6 * eye_width))  # Feather 5% of eye width
    if extend_outer is None:
        extend_outer = int(0.7 * eye_width)  # Extend outer edge by 30% of eye width
    if extend_lower is None:
        extend_lower = int(1.5 * eye_height)  # Extend downward by 120% of eye height

    # --- Define indices for lower eyelid points ---
    left_lower_lid = face_points[36:42]  # Lower part of left eye (landmarks 36-41)
    right_lower_lid = face_points[42:48]  # Lower part of right eye (landmarks 42-47)

    # --- Shift points downward and slightly outward for under-eye concealer ---
    def shift_points(points, shift_y=7, shift_x=0):
        return [(x + shift_x, y + shift_y) for (x, y) in points]

    # Apply downward shift to simulate under-eye concealer
    left_lower_lid_shifted = shift_points(left_lower_lid, shift_y=extend_lower, shift_x=-extend_outer)
    right_lower_lid_shifted = shift_points(right_lower_lid, shift_y=extend_lower, shift_x=extend_outer)

    # Extend outer corners to create a lifted effect
    left_outer_corner = (
        left_lower_lid[-1][0] + extend_outer,
        left_lower_lid[-1][1] + extend_lower // 3,
    )
    right_outer_corner = (
        right_lower_lid[0][0] - extend_outer,
        right_lower_lid[0][1] + extend_lower // 3,
    )

    # Add the extended points to the shifted lower lid points
    left_lower_lid_shifted.append(left_outer_corner)
    right_lower_lid_shifted.append(right_outer_corner)

    # --- Create convex hulls for smooth contour of under-eye concealer ---
    left_concealer_hull = cv2.convexHull(np.array(left_lower_lid_shifted, dtype=np.int32))
    right_concealer_hull = cv2.convexHull(np.array(right_lower_lid_shifted, dtype=np.int32))

    # --- Create mask for under-eye concealer ---
    concealer_mask = np.zeros_like(image[:, :, 0], dtype=np.uint8)
    cv2.fillConvexPoly(concealer_mask, left_concealer_hull, 255)
    cv2.fillConvexPoly(concealer_mask, right_concealer_hull, 255)

    # --- Subtract the eye mask to avoid overlapping eyes ---
    if eye_mask is not None:
        concealer_mask = cv2.subtract(concealer_mask, eye_mask)

    # --- Feather the concealer mask for smooth blending ---
    if feather_size > 0:
        if feather_size % 2 == 0:
            feather_size += 1  # Ensure feather size is odd
        concealer_mask = cv2.GaussianBlur(concealer_mask, (feather_size, feather_size), 0)

    # --- Create concealer overlay ---
    concealer_layer = np.full_like(image, concealer_color, dtype=np.uint8)
    concealer_masked = cv2.bitwise_and(concealer_layer, concealer_layer, mask=concealer_mask)

    # --- Blend the concealer with the original image ---
    result = image.copy()
    for c in range(3):  # Blend each channel separately (B, G, R)
        result[:, :, c] = (
            (concealer_masked[:, :, c] * (concealer_mask.astype(np.float32) / 255.0) * alpha)
            + image[:, :, c] * (1 - (concealer_mask.astype(np.float32) / 255.0) * alpha)
        ).astype(np.uint8)

    return result
