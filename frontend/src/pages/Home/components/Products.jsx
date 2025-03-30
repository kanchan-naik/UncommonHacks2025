import { useState } from "react";
import Product from "models/Product";
import "./Products.css";
import ColorPalette, {
  PaletteType,
} from "components/ColorPalette/ColorPalette";
import rgbHex from "rgb-hex";

const products = [
  new Product(
    "/lipstick_with_cap.png",
    "lipstick",
    "lipstick",
    PaletteType.LIPSTICK,
  ),
  new Product("/brush.png", "makeup brush", "eyeshadow", PaletteType.EYESHADOW),
  new Product(
    "/foundation.png",
    "liquid foundation container",
    "foundation",
    PaletteType.FOUNDATION,
  ),
  new Product("/blush.png", "blush", "blush", PaletteType.BLUSH),
  new Product(
    "/concealer.png",
    "concealer",
    "concealer",
    PaletteType.CONCEALER,
  ),
];

const defaultRequest = {
  foundation: {
    present: 0,
    color: "#fff",
  },
  lipstick: {
    present: 0,
    color: "#fff",
  },
  eyeshadow: {
    present: 0,
    color: "#fff",
  },
  blush: {
    present: 0,
    color: "#fff",
  },
  concealer: {
    present: 0,
    color: "#fff",
  },
};

export default function Products({ onSubmit }) {
  const [selectedProduct, setSelectedProduct] = useState(null);
  const [makeupRequest, setMakeupRequest] = useState(defaultRequest);

  const colorChanged = (newColor) => {
    if (!selectedProduct) return;
    let newRequest = makeupRequest;

    newRequest[selectedProduct.title].present = 1;
    newRequest[selectedProduct.title].color = rgbHex(
      newColor.r,
      newColor.g,
      newColor.b,
    );
    setMakeupRequest(newRequest);
    onSubmit(newRequest);
  };

  const selectProduct = (product) => {
    setSelectedProduct(product);
    console.log(
      `selected.id: ${selectedProduct?.id}\nproduct.id: ${product.id}`,
    );
  };

  const closePalette = () => {
    setTimeout(() => {
      setSelectedProduct(null);
    }, 500);
  };

  return (
    <>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          position: "absolute",
          bottom: 0,
          left: "50%",
          transform: "translateX(-50%)",
          width: "80%",
          overflowY: "hidden",
        }}
      >
        {products.map((product) => {
          return (
            <img
              className={`product ${selectedProduct?.id === product.id ? "selected" : ""}`}
              key={product.id}
              src={product.imgSrc}
              alt={product.alt}
              onClick={() => selectProduct(product)}
            />
          );
        })}
      </div>
      {selectedProduct && (
        <>
          <ColorPalette
            type={selectedProduct.type}
            setSelectedColor={colorChanged}
            onClose={closePalette}
          />
        </>
      )}
    </>
  );
}
