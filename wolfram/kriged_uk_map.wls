#!/usr/bin/env wolframscript
(* kriged_uk_map.wls — UK country-wide kriged diesel-price map using
   the real Fuel-Finder retailer JSON snapshots (~1,373 stations). *)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/Kriging.wl"];

snap = Normal @ Import["data/uk_fuelfinder_snapshot.csv", "Dataset", "HeaderLines" -> 1];
stations = Map[
  <|"lat" -> #["lat"], "lng" -> #["lng"], "price" -> #["price_b7_eur"]|> &,
  Select[snap, NumberQ[#["lat"]] && NumberQ[#["lng"]] &&
               NumberQ[#["price_b7_eur"]] &]];
Print["[krige-uk] valid stations: ", Length[stations]];

priceQ = Quantile[#["price"] & /@ stations, {0.02, 0.5, 0.98}];
Print["[krige-uk] price quantiles (2/50/98 %): ", priceQ];

Print["[krige-uk] fitting variogram ..."];
vario = Kriging`FitVariogram[stations];
Print["[krige-uk] variogram c0=", N @ vario["c0"],
      "  c1=", N @ vario["c1"], "  range a=", N @ vario["a"], " km"];

ukBox = {{49.85, 60.85}, {-8.65, 1.78}};
krige = Kriging`KrigeFunction[stations, vario, "nNearest" -> 24];

(* Cheap point-in-polygon mask: keep cells whose nearest station is
   within ~45 km. Anything beyond that is open sea or inland Europe
   and shouldn't be coloured. The grid is fine (0.10 deg lat) so the
   vertex-colour interpolation in KrigedGeoGraphics reads as smooth. *)
nearestStnKm[lat_, lon_] := Min @ Map[
  Kriging`HaversineKm[{lat, lon}, {#["lat"], #["lng"]}] &, stations];

grid = Kriging`KrigedGrid[krige, ukBox, {0.10, 0.15},
  "Mask" -> (nearestStnKm[#1, #2] <= 45. &)];
Print["[krige-uk] grid: ", Length /@ {grid["Lat"], grid["Lon"]},
      "   valid cells: ", Count[Flatten[grid["Field"]], _?NumberQ]];

priceMin = priceQ[[1]]; priceMax = priceQ[[3]];
priceSpan = Max[priceMax - priceMin, 0.01];
colorFn = Function[p,
  ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]]];

(* SMOOTH render: each cell is a vertex-coloured quad on a real
   GeoGraphics basemap (coastline from GeoBackground), so the field
   blends between grid nodes instead of showing flat blocks. *)
mainMap = Kriging`KrigedGeoGraphics[grid, colorFn,
  GeoRange -> Entity["Country", "UnitedKingdom"],
  GeoProjection -> "Mercator",
  GeoBackground -> GeoStyling["StreetMapNoLabels"],
  "Opacity" -> 0.8,
  PlotLabel -> Style[
    StringTemplate[
      "UK diesel price -- ordinary-kriging map (`1` retailer stations, `2`)"][
      Length[stations], DateString[Now, "ISODate"]],
    18, Bold, GrayLevel[0.1]],
  ImageSize -> 1100];

legend = BarLegend[
  {ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
  LegendLabel -> Style["EUR/L", 13, Bold],
  LabelStyle -> 12, LegendMarkerSize -> 380];

Export["docs/images/uk_kriged_map.png",
  Legended[mainMap, legend], ImageResolution -> 144];

Print["[krige-uk] wrote docs/images/uk_kriged_map.png"];
