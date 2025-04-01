# 💄 Vanity.AI
### AI-Powered Virtual Makeup Visualizer

Vanity.AI is an advanced AI-powered web application that allows users to **try on virtual makeup** in real-time. The platform leverages **computer vision and facial landmark detection** to ensure highly accurate and realistic application of makeup effects across diverse skin tones, lighting conditions, and facial structures.

---

## 🎯 **Project Overview**
Vanity.AI addresses key challenges in the beauty industry by providing a **realistic virtual try-on experience** that enhances customer engagement and reduces product returns. It uses state-of-the-art **computer vision models** and **image processing techniques** to apply makeup effects dynamically.

---

## ✨ **Core Features**
✅ **Virtual Makeup Application**  
- Apply foundation, lipstick, eyeshadow, and blush with seamless blending.  
- Leverages facial landmark detection to adjust makeup based on facial symmetry.  

✅ **Real-Time Facial Landmark Detection**  
- Detects key facial points using **Face++ API** to ensure precise makeup placement.  
- Adapts dynamically to various face angles and lighting conditions.  

✅ **Accurate Skin Tone Matching**  
- Automatically adjusts makeup shades to match the user’s skin tone.  
- Utilizes color correction and tone adaptation to improve realism.  

✅ **Lighting & Texture Adaptation**  
- Corrects lighting inconsistencies to maintain accurate color representation.  
- Enhances realism by preserving natural skin texture under different lighting conditions.  

✅ **Photo & Live Mode Support**  
- Allows users to upload photos or use their webcam for live virtual try-on.  
- Provides side-by-side comparisons of before/after looks.  

---

## 🛠️ **Tech Stack**
### 🎨 **Frontend:**
- **React.js** – For a dynamic and interactive user interface.  
- **WebRTC** – For real-time webcam feed and video processing.  
- **Three.js** – For applying 3D makeup effects with depth and precision.

### 🧠 **AI/Computer Vision:**
- **Face++ API** – For detecting and analyzing facial landmarks.  
- **Sharp.js** – For applying makeup effects with pixel-level accuracy.  
- **OpenCV.js** – For real-time image processing and face detection.  

### ⚙️ **Backend:**
- **Node.js/Express.js** – For handling API requests and model inference.  
- **MongoDB** – To store user preferences, try-on history, and personalized settings.  

---

## 🔥 **How It Works**
1. **Facial Landmark Detection:**  
   - Face++ API detects 106+ facial landmarks to identify eyes, lips, cheeks, and other key regions.  
2. **Makeup Layer Application:**  
   - Sharp.js applies virtual makeup layers to specific facial regions, adapting to the user’s skin tone.  
3. **Real-Time Adjustment:**  
   - Dynamic re-calibration of makeup based on face movements, lighting changes, and angle variations.  
4. **Rendering & Display:**  
   - The rendered image is displayed back to the user with side-by-side comparison capabilities.

---

## 🚀 **Installation & Setup**
### 1. **Clone the Repository**
```bash
git clone https://github.com/yourusername/vanity-ai.git
cd vanity-ai
