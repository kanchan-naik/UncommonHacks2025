import { useState } from "react";
import Product from "models/Product";
import "./Products.css";

const products = [
  new Product("/lipstick_with_cap.png", "lipstick"),
  new Product("/brush.png", "makeup brush"),
  new Product("/foundation.png", "liquid foundation container"),
];

export default function Products() {
  const [selectedProduct, setSelectedProduct] = useState(null);

  // TODO: Turn these into model instances
  //
  const selectProduct = (product) => {
    setSelectedProduct(product);
    console.log(
      `selected.id: ${selectedProduct?.id}\nproduct.id: ${product.id}`,
    );
  };

  return (
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
  );
}
