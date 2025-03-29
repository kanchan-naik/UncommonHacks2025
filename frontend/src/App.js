import logo from "./logo.svg";
import "./App.css";
import { Route, Routes } from "react-router-dom";
import ColorSwatch from "./components/ColorSwatch/ColorSwatch";

const product = {
  class: "sephora foundation red",
};

function App() {
  return (
    <Routes>
      <Route path="/" element={<ColorSwatch product={product} />} />
    </Routes>
  );
}

export default App;
