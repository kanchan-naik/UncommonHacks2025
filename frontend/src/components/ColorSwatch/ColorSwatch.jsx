import "./ColorSwatch.css";

export default function ColorSwatch({ product }) {
  return (
    <img
      src="/light.png"
      alt="light"
      className={"colorSwatch " + product.class}
    ></img>
  );
}
