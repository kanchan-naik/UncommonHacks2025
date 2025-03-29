// src/FaceDetection.js
import React, { useRef, useState } from "react";
import "./FaceDetection.css";

function FaceDetection() {
  const videoRef = useRef(null);
  const canvasRef = useRef(null);
  const [detectionResult, setDetectionResult] = useState(null);
  /* Static Test Content for Foundation Coverage */
  const [foundationColor, setFoundationColor] = useState({
    r: 255,
    g: 200,
    b: 150,
  }); // Example color
  const [opacity, setOpacity] = useState(0.5); // Example opacity

  // Start the webcam
  const setupWebcam = async () => {
    const stream = await navigator.mediaDevices.getUserMedia({ video: true });
    videoRef.current.srcObject = stream;
  };

  // Capture an image from the webcam and store it locally
  const captureImage = async () => {
    const canvas = canvasRef.current;
    const context = canvas.getContext("2d");
    context.drawImage(videoRef.current, 0, 0, canvas.width, canvas.height);

    const imageData = canvas.toDataURL("image/jpeg");

    try {
      const response = await fetch("http://localhost:4000/save-image", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ imageData }),
      });

      const { imageId } = await response.json();
      console.log("Saved image with imageId:", imageId); // Debug log

      // Check if imageId is valid
      if (!imageId) {
        console.error("No imageId returned!");
        return;
      }

      detectFace(imageData, imageId); // Pass imageId for reference
    } catch (error) {
      console.error("Error saving image:", error);
    }
  };

  // Send image data to backend for face detection
  const detectFace = async (imageData, imageId) => {
    try {
      const response = await fetch("http://localhost:4000/detect-face", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ imageUrl: imageData, imageId }),
      });

      const data = await response.json();
      setDetectionResult(data);

      // Apply foundation if faces are detected
      if (data && data.faces && data.faces.length > 0) {
        applyFoundation(data); // This is where applyFoundation is called
      }
    } catch (error) {
      console.error("Error detecting face:", error);
    }
  };

  // Use backend APIs to apply makeup to the face using standard foundationColor and opacity
  // response image with the makeup applied is stored in faceDetectionResponse and rendered on the
  // frontend just below the webcam
  const applyFoundation = async (faceDetectionResponse) => {
    try {
      const response = await fetch("http://localhost:4000/apply-foundation", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          faceDetectionResponse,
          foundationColor,
          opacity,
        }),
      });

      const imageBlob = await response.blob();
      const imageUrl = URL.createObjectURL(imageBlob);
      // Set the resulting image URL to display
      setDetectionResult((prevState) => ({
        ...prevState,
        imageUrl,
      }));
      console.log("resulting image Url", imageUrl);
    } catch (error) {
      console.error("Error applying foundation:", error);
    }
  };

  return (
    <div id="webcam">
      <video ref={videoRef} autoPlay muted></video>
      <canvas
        ref={canvasRef}
        width="640"
        height="480"
        style={{ display: "none" }}
      ></canvas>
      <div id="controls">
        <button onClick={captureImage}>Capture</button>
        {detectionResult && (
          <pre>{JSON.stringify(detectionResult, null, 2)}</pre>
        )}
        <button onClick={setupWebcam}>Start Webcam</button>
      </div>

      {/* Display the processed image after applying foundation */}
      {detectionResult?.imageUrl && (
        <div>
          <h2>Processed Image with Foundation</h2>
          <img
            src={detectionResult.imageUrl}
            alt="Processed Face"
            width="640"
          />
        </div>
      )}
    </div>
  );
}

export default FaceDetection;
