import "./ColorSwatch.css";

export default function ColorSwatch({ product }) {
  return <div className={"colorSwatch " + product.class}></div>;
}
