export default class  CameraService {
  static async applyMakeup(imageData, makeupRequest) {
    const imageId = await CameraService.saveImage(imageData, makeupRequest);
    console.log("Saved image with imageId:", imageId);
    return imageId;
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
    form.append("bofa", JSON.stringify(makeupRequest));
    const response = await fetch("http://localhost:3000/", {
      method: "POST",
      body: form,
    });

    const imageId = await response.blob();
    if (!imageId) {
      throw new Error("No imageId returned!");
    }

    // Create a URL for the image
    const imageUrl = URL.createObjectURL(imageId);

    return imageUrl;
  }
}
