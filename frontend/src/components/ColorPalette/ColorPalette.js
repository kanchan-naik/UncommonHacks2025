import { useEffect, useRef, useState } from "react";
import "./ColorPalette.css";

export const PaletteType = Object.freeze({
  FOUNDATION: "/foundation_palette.png",
  LIPSTICK: "/lipstick_palette.png",
  EYESHADOW: "/eyeshadow_palette.png",
  BLUSH: "/blush_palette.png",
  CONCEALER: "/concealer_palette.png",
});

export default function ColorPalette({ type, setSelectedColor, onClose }) {
  const canvasRef = useRef(null);
  const imgRef = useRef(null);
  const paletteRef = useRef(null);
  const [hoverColor, setHoverColor] = useState("transparent");
  const [closeButtonUrl, setCloseButtonUrl] = useState("/close_button.png");
  const [didEnterColorFirstTime, setDidEnterColorFirstTime] = useState(false);
  const [resetColorCallback, setResetColorCallback] = useState(() => {});
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
      ctx.drawImage(img, 0, -100, width, height);
    };
    imgRef.current = img;
  }, [type]);

  const replaceColor = (targetRgba, newRgba) => {
    const canvas = canvasRef.current;
    const ctx = canvas.getContext("2d");
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    const data = imageData.data;

    const [tr, tg, tb] = targetRgba;
    const [nr, ng, nb, na] = newRgba;

    for (let i = 0; i < data.length; i += 4) {
      const r = data[i],
        g = data[i + 1],
        b = data[i + 2];

      // Use a tolerance if needed
      if (r === tr && g === tg && b === tb) {
        data[i] = nr;
        data[i + 1] = ng;
        data[i + 2] = nb;
        data[i + 3] = na;
      }
    }

    ctx.putImageData(imageData, 0, 0);
  };

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
      setDidEnterColorFirstTime(true);
      const color = `rgba(${r}, ${g}, ${b}, ${a / 255})`;
      setHoverColor(color);

      if (!didEnterColorFirstTime) {
        replaceColor([r, g, b, a], [r, g, b, a * 0.9]);
        setResetColorCallback(() => {
          return () => replaceColor([r, g, b, a * 0.9], [r, g, b, a]);
        });
      }
    } else {
      if (hoverColor !== "transparent") {
        resetColorCallback();
      }

      setHoverColor("transparent");
      setDidEnterColorFirstTime(false);
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
    <div
      id="colorPalette"
      style={{
        backgroundImage: "url(/palette_bg.png)",
        backgroundSize: "contain",
        backgroundRepeat: "no-repeat",
      }}
      ref={paletteRef}
    >
      <img
        style={{
          position: "absolute",
          top: "-10px",
          right: "-10px",
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
