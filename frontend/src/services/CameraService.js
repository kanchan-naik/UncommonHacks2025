export default class CameraService {
  static async applyMakeup(imageData, makeupRequest) {
    const imageId = await CameraService.saveImage(imageData, makeupRequest);
    console.log("Saved image with imageId:", imageId);

    const detectionData = await CameraService.detectFace(imageData, imageId);

    if ((detectionData?.faces?.length ?? 0) > 0) {
      return await CameraService.addMakeupToFace(detectionData, makeupRequest);
    }
  }

  static async saveImage(imageData, makeupRequest) {
    var form = new FormData();
    function dataURLtoBlob(dataURL) {
      const parts = dataURL.split(",");
      const mime = parts[0].match(/:(.*?);/)[1];
      const b64 = atob(parts[1]);
      let u8arr = new Uint8Array(b64.length);
      for (let i = 0; i < b64.length; i++) {
        u8arr[i] = b64.charCodeAt(i);
      }
      return new Blob([u8arr], { type: mime });
    }
    form.append("image", dataURLtoBlob(imageData));
    form.append("bofa", JSON.stringify({ makeupRequest }));
    const response = await fetch("http://localhost:3000/", {
      method: "POST",
      body: form,
      mode: "no-cors",
    });

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
