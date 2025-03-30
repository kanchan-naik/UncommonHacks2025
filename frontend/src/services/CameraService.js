export default class CameraService {
  static async applyMakeup(imageData, makeupRequest) {
    const imageId = await CameraService.saveImage(imageData);
    console.log("Saved image with imageId:", imageId);

    const detectionData = await CameraService.detectFace(imageData, imageId);

    if ((detectionData?.faces?.length ?? 0) > 0) {
      return await CameraService.applyFoundation(
        detectionData,
        makeupRequest.color,
        makeupRequest.opacity,
      );
    }
  }

  static async saveImage(imageData) {
    const response = await fetch("http://localhost:3000/save-image", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ imageData }),
    });

    const file = new Blob([JSON.stringify(imageData)], { type: "text/plain" });

    const { imageId } = await response.json();
    if (!imageId) {
      throw new Error("No imageId returned!");
    }
    return imageId;
  }

  static async detectFace(imageData, imageId) {
    const response = await fetch("http://localhost:3000/detect-face", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ imageUrl: imageData, imageId }),
    });

    return await response.json();
  }

  static async applyFoundation(
    faceDetectionResponse,
    foundationColor,
    opacity,
  ) {
    const response = await fetch("http://localhost:3000/apply-foundation", {
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
    return URL.createObjectURL(imageBlob);
  }
}
