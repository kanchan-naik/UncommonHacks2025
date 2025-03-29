// src/FaceDetection.js
import React, { useRef, useState } from 'react';

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
    const context = canvas.getContext('2d');
    context.drawImage(videoRef.current, 0, 0, canvas.width, canvas.height);

    const imageData = canvas.toDataURL('image/jpeg'); // Capture as base64
    detectFace(imageData);
  };

  // Send image data to backend for face detection
  const detectFace = async (imageData) => {
    try {
      const response = await fetch('http://localhost:4000/detect-face', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ imageUrl: imageData }),
      });
  
      const data = await response.json();
      setDetectionResult(data);
    } catch (error) {
      console.error('Error detecting face:', error);
    }
  };  

  return (
    <div>
      <h1>Webcam Face Detection</h1>
      <video ref={videoRef} width="640" height="480" autoPlay muted></video>
      <canvas ref={canvasRef} width="640" height="480" style={{ display: 'none' }}></canvas>
      <button onClick={captureImage}>Capture</button>
      {detectionResult && <pre>{JSON.stringify(detectionResult, null, 2)}</pre>}
      <button onClick={setupWebcam}>Start Webcam</button>
    </div>
  );
}

export default FaceDetection;
