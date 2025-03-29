import ColorSwatch from "components/ColorSwatch/ColorSwatch";

export const Direction = Object.freeze({
  ROW: "row",
  COLUMN: "column",
});

const light = {
  class: "light",
};

export default function LightRow({ id, count, direction }) {
  return (
    <div
      style={{
        display: "flex",
        flexDirection: direction,
        placeContent: "space-between",
      }}
      id={id}
    >
      {Array.from({ length: count }).map((_, i) => {
        return <ColorSwatch key={i} product={light} />;
      })}
    </div>
  );
}
