import React, { useRef, useState } from "react";
import "./FaceDetection.css";

function FaceDetection() {
  const videoRef = useRef(null);
  const canvasRef = useRef(null);
  const [detectionResult, setDetectionResult] = useState(null);

  // Start the webcam
  const setupWebcam = async () => {
    const stream = await navigator.mediaDevices.getUserMedia({ video: true });
    videoRef.current.srcObject = stream;
  };

  // Capture an image from the webcam
  const captureImage = () => {
    const canvas = canvasRef.current;
    const context = canvas.getContext("2d");
    context.drawImage(videoRef.current, 0, 0, canvas.width, canvas.height);

    const imageData = canvas.toDataURL("image/jpeg"); // Capture as base64
    detectFace(imageData);
  };

  // Send image data to backend for face detection
  const detectFace = async (imageData) => {
    try {
      const response = await fetch("http://localhost:4000/detect-face", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ imageUrl: imageData }),
      });

      const data = await response.json();
      setDetectionResult(data);
    } catch (error) {
      console.error("Error detecting face:", error);
    }
  };

  return (
    <div id="webcam">
      <video ref={videoRef} autoPlay muted></video>
      <canvas ref={canvasRef} style={{ display: "none" }}></canvas>
      <div id="controls">
        <button onClick={captureImage}>Capture</button>
        {detectionResult && (
          <pre>{JSON.stringify(detectionResult, null, 2)}</pre>
        )}
        <button onClick={setupWebcam}>Start Webcam</button>
      </div>
    </div>
  );
}

export default FaceDetection;
