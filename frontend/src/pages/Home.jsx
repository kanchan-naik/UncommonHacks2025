import ColorSwatch from "../components/ColorSwatch/ColorSwatch";

const product = {
  class: "sephora foundation red",
};

export default function Home() {
  return <ColorSwatch product={product} />;
}
