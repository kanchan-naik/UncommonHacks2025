// src/FaceDetection.js
import React, { useEffect, useRef } from "react";
import "./FaceDetection.css";

function FaceDetection({ userSubmitted, onScreenCapture, makeupImage }) {
  const videoRef = useRef(null);
  const canvasRef = useRef(null);
  /* Static Test Content for Foundation Coverage */

  useEffect(() => {
    const setupWebcam = async () => {
      const stream = await navigator.mediaDevices.getUserMedia({ video: true });
      videoRef.current.srcObject = stream;
    };

    setupWebcam();
  });

  useEffect(() => {
    if (!userSubmitted) return;

    const videoRect = videoRef.current.getBoundingClientRect();
    const canvas = canvasRef.current;
    const context = canvas.getContext("2d");
    canvas.width = videoRect.width;
    canvas.height = videoRect.height;
    context.drawImage(videoRef.current, 0, 0, canvas.width, canvas.height);
    const imageData = canvas.toDataURL("image/png");

    onScreenCapture(imageData);
  }, [userSubmitted, onScreenCapture]);

  // Start the webcam

  return (
    <div
      id="webcam"
      style={{
        display: "flex",
        gap: "20px",
      }}
    >
      <video ref={videoRef} autoPlay muted></video>
      <canvas ref={canvasRef} style={{ display: "none" }}></canvas>

      {makeupImage && (
        <img id="processed-face" src={makeupImage} alt="Processed Face" />
      )}
    </div>
  );
}

export default FaceDetection;
