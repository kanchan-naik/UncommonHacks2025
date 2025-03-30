import { useState } from "react";
import Product from "models/Product";
import "./Products.css";
import ColorPalette, {
  PaletteType,
} from "components/ColorPalette/ColorPalette";
import { MakeupRequest } from "../Home";

const products = [
  new Product("/lipstick_with_cap.png", "lipstick", "lipstick"),
  new Product("/brush.png", "makeup brush", "eyeliner"),
  new Product("/foundation.png", "liquid foundation container", "foundation"),
];

const defaultRequest = {
  foundation: {
    present: 0,
    r: "#fff",
    g: "#fff",
    b: "#fff",
  },
  lipstick: {
    present: 0,
    r: "#fff",
    g: "#fff",
    b: "#fff",
  },
  eyeliner: {
    present: 0,
    r: "#fff",
    g: "#fff",
    b: "#fff",
  },
};

export default function Products({ onSubmit }) {
  const [selectedProduct, setSelectedProduct] = useState(null);
  const [makeupRequest, setMakeupRequest] = useState(defaultRequest);
  const [selectedColor, setSelectedColor] = useState();

  const colorChanged = (newColor) => {
    if (!selectedProduct) return;
    let newRequest = makeupRequest;

    newRequest[selectedProduct.title].present = 1;
    newRequest[selectedProduct.title].r = newColor.r;
    newRequest[selectedProduct.title].g = newColor.g;
    newRequest[selectedProduct.title].b = newColor.b;
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
            type={PaletteType.FOUNDATION}
            setSelectedColor={colorChanged}
            onClose={closePalette}
          />
        </>
      )}
    </>
  );
}
