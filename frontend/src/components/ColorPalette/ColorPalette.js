import { useEffect, useRef, useState } from "react";
import "./ColorPalette.css";

export const PaletteType = Object.freeze({
  FOUNDATION: "/foundation_palette.png",
});

export default function ColorPalette({ ref, type, setSelectedColor }) {
  const canvasRef = useRef(null);
  const imgRef = useRef(null);
  const [hoverColor, setHoverColor] = useState("transparent");
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

  return (
    <div
      style={{
        backgroundColor: "rgba(0, 0, 0, 0.2)",
        padding: "20px 10px",
      }}
      id="colorPalette"
      ref={ref}
    >
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
      <div
        style={{
          marginTop: "10px",
          padding: "10px",
          backgroundColor: hoverColor,
          color: "#000",
          border: "1px solid #aaa",
        }}
      >
        {hoverColor !== "transparent" ? `Hover Color: ${hoverColor}` : null}
      </div>
    </div>
  );
}
