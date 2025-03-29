import FaceDetection from "components/FaceDetection/FaceDetection";
import LightRow, { Direction } from "./components/LightRow";
import Products from "./components/Products";
import "./Home.css";

export default function Home() {
  return (
    <div id="main-page">
      <LightRow id={"light-row-1"} count={3} direction={Direction.COLUMN} />
      <LightRow id={"light-row-2"} count={5} direction={Direction.ROW} />
      <LightRow id={"light-row-3"} count={3} direction={Direction.COLUMN} />
      <FaceDetection />
      <Products />
    </div>
  );
}
