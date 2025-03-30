import { useEffect, useRef, useState } from "react";
import "./ColorPalette.css";

export const PaletteType = Object.freeze({
  FOUNDATION: "/foundation_palette.png",
});

export default function ColorPalette({ type, setSelectedColor, onClose }) {
  const canvasRef = useRef(null);
  const imgRef = useRef(null);
  const paletteRef = useRef(null);
  const [hoverColor, setHoverColor] = useState("transparent");
  const [closeButtonUrl, setCloseButtonUrl] = useState("/close_button.png");
  const width = 400;
  const height = 400;

  useEffect(() => {
    const canvas = canvasRef.current;
    const ctx = canvas.getContext("2d");
    const img = new Image();
    img.src = type;
    img.crossOrigin = "Anonymous";
    img.onload = () => {
      ctx.imageSmoothingEnabled = false;
      ctx.drawImage(img, 0, 0, width, height);
    };
    imgRef.current = img;
  }, [type]);

  const getCurrentPixel = (canvasRef, e) => {
    if (!canvasRef.current) return null;
    const canvas = canvasRef.current;
    const rect = canvas.getBoundingClientRect();
    const x = Math.floor(e.clientX - rect.left);
    const y = Math.floor(e.clientY - rect.top);
    const ctx = canvas.getContext("2d");
    const pixel = ctx.getImageData(x, y, 1, 1).data;
    return pixel;
  };

  const handleMouseMove = (e) => {
    const [r, g, b, a] = getCurrentPixel(canvasRef, e);
    if (!(r === g && g === b)) {
      setHoverColor(`rgba(${r}, ${g}, ${b}, ${a / 255})`);
    } else {
      setHoverColor("transparent");
    }
  };

  const handleMouseDown = (e) => {
    const [r, g, b] = getCurrentPixel(canvasRef, e);
    if (!(r === g && g === b)) {
      setSelectedColor({ r, g, b });
    }
  };

  const closePalette = () => {
    const palette = paletteRef.current;

    if (!palette) return;

    palette.classList.add("exit");
    onClose();
  };

  return (
    <div id="colorPalette" ref={paletteRef}>
      <img
        style={{
          position: "absolute",
          top: "-5px",
          left: "-5px",
          width: "50px",
          aspectRatio: "1",
        }}
        src={closeButtonUrl}
        onMouseEnter={() => setCloseButtonUrl("/close_button_hovered.png")}
        onMouseLeave={() => setCloseButtonUrl("/close_button.png")}
        onMouseDown={closePalette}
        className="closeButton"
        alt="close button"
      ></img>
      <div
        style={{
          display: "flex",
          padding: "10px 20px",
          justifyContent: "space-around",
        }}
      >
        <div id="currentSwatch"></div>
        <div id="closeButton"></div>
      </div>
      <canvas
        ref={canvasRef}
        width={width}
        height={height}
        onMouseMove={handleMouseMove}
        onMouseDown={handleMouseDown}
        style={{
          imageRendering: "pixelated",
          cursor: "crosshair",
        }}
      />
    </div>
  );
}
