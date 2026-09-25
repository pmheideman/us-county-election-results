// "PNG" button on the map card: rasterizes the map exactly as it is framed now (tiles, counties, state lines, title,
// legends) with html-to-image. Zoom buttons, hover tooltips and the button itself are left out.
document.addEventListener("click", function (ev) {
  var btn = ev.target.closest("#download_png");
  if (!btn || btn.disabled) return;
  var map = document.getElementById("map");
  if (!map || !window.htmlToImage) return;

  var skip = ["leaflet-control-zoom", "leaflet-tooltip"];
  var title = map.querySelector(".map-title-main");
  var name = (title ? title.textContent : "map").replace(/[^A-Za-z0-9]+/g, "_").replace(/^_|_$/g, "");

  btn.disabled = true;
  htmlToImage.toPng(map, {
    pixelRatio: 2,
    backgroundColor: "#eeebe4",
    filter: function (node) {
      return !(node.classList && skip.some(function (c) { return node.classList.contains(c); }));
    }
  }).then(function (url) {
    var a = document.createElement("a");
    a.href = url;
    a.download = "us_county_results_" + name + ".png";
    document.body.appendChild(a); a.click(); a.remove();
  }).catch(function (err) {
    console.error("PNG export failed", err);
  }).finally(function () { btn.disabled = false; });
});
