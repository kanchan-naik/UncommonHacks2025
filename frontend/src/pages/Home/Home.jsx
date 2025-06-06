import FaceDetection from "components/FaceDetection/FaceDetection";
import { useState } from "react";
import CameraService from "services/CameraService";
import Products from "./components/Products";
import "./Home.css";

export class MakeupRequest {
  constructor(foundation, eyeshadow, lipstick) {
    this.foundation = foundation;
    this.eyeshadow = eyeshadow;
    this.lipstick = lipstick;
  }
}

export default function Home() {
  const [userSubmitted, setUserSubmitted] = useState();
  const [makeupImage, setMakeupImage] = useState(null);
  const [makeupRequest, setMakeupRequest] = useState(null);

  const submitMakeupRequest = async (imageData) => {
    if (!makeupRequest) return;

    try {
      const imageUrl = await CameraService.applyMakeup(
        imageData,
        makeupRequest,
      );
      setMakeupImage(imageUrl);
      console.log("resulting image Url", imageUrl);
    } catch (error) {
      console.error("Error processing image:", error);
    }

    setUserSubmitted(false);
  };

  const getScreenshot = (makeupRequest) => {
    setMakeupRequest(makeupRequest);
    setUserSubmitted(true);
  };

  return (
    <div id="main-page">
      {/* Vanity background image */}
      <div id="vanity-background" />
      <FaceDetection
        userSubmitted={userSubmitted}
        onScreenCapture={submitMakeupRequest}
        makeupImage={makeupImage}
      />
      <Products onSubmit={getScreenshot} />
    </div>
  );
}
