#!/usr/bin/env wolframscript
(* kriged_de_map.wls — kriged regional map of the Paderborn 10 km
   cluster.

   We deliberately do NOT call the Tankerkönig live API from this
   script; that call has its own dedicated script which is guarded
   to be opt-in only (wolfram/fetch_tankerkoenig.wls).

   This map uses the 29-station Paderborn feed (real prices, last 24 h
   averaged per station) with ordinary kriging on a fine local grid.
   The output is a high-resolution diesel-price surface for the
   Paderborn region clipped to a 0.4°×0.4° box around the cluster's
   centroid.

   Output:
     docs/images/germany_kriged_map.png
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/Kriging.wl"];
Get["wolfram/PetrolData.wl"];

de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];
latest = SortBy[deDiesel, AbsoluteTime[#TimestampUTC] &][[-1]]["TimestampUTC"];
recent = Select[deDiesel,
  AbsoluteTime[latest] - AbsoluteTime[#TimestampUTC] <= 86400 &];

stationMean = With[{g = GroupBy[recent, #StationID &]},
  KeyValueMap[
    <|"lat" -> First[#2]["Lat"], "lng" -> First[#2]["Lon"],
      "brand" -> First[#2]["Brand"],
      "price" -> Mean[#Price & /@ #2]|> &, g]];

Print["[krige-de] Paderborn stations: ", Length[stationMean]];
priceQ = Quantile[#["price"] & /@ stationMean, {0.0, 0.5, 1.0}];
Print["[krige-de] price range: ", priceQ];

vario = Kriging`FitVariogram[stationMean];
Print["[krige-de] variogram c0=", N @ vario["c0"],
      "  c1=", N @ vario["c1"], "  range a=", N @ vario["a"], " km"];

krige = Kriging`KrigeFunction[stationMean, vario,
  "nNearest" -> Min[18, Length[stationMean]]];

latC = Mean[#["lat"] & /@ stationMean];
lonC = Mean[#["lng"] & /@ stationMean];
pad = 0.18;

nearestStnKm[lat_, lon_] := Min @ Map[
  Kriging`HaversineKm[{lat, lon}, {#["lat"], #["lng"]}] &, stationMean];

(* Fine grid + vertex-colour smoothing (see Kriging`KrigedGeoGraphics). *)
grid = Kriging`KrigedGrid[krige,
  {{latC - pad, latC + pad}, {lonC - pad, lonC + pad}}, {0.006, 0.009},
  "Mask" -> (nearestStnKm[#1, #2] <= 10. &)];
Print["[krige-de] valid cells: ",
      Count[Flatten[grid["Field"]], _?NumberQ]];

priceMin = priceQ[[1]] - 0.005; priceMax = priceQ[[3]] + 0.005;
priceSpan = Max[priceMax - priceMin, 0.01];
colorFn = Function[p,
  ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]]];

mainMap = Kriging`KrigedGeoGraphics[grid, colorFn,
  GeoRange -> {{latC - pad, latC + pad}, {lonC - pad, lonC + pad}},
  GeoProjection -> "Mercator",
  GeoBackground -> GeoStyling["StreetMapNoLabels"],
  "Points" -> stationMean, "Opacity" -> 0.82,
  PlotLabel -> Style[
    StringTemplate[
      "Paderborn 10 km cluster -- kriged diesel surface (`1` real stations, last 24 h)"][
      Length[stationMean]],
    18, Bold, GrayLevel[0.1]],
  ImageSize -> 1100];

legend = BarLegend[
  {ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
  LegendLabel -> Style["EUR/L", 13, Bold],
  LabelStyle -> 12, LegendMarkerSize -> 380];

Export["docs/images/germany_kriged_map.png",
  Legended[mainMap, legend], ImageResolution -> 144];

Print["[krige-de] wrote docs/images/germany_kriged_map.png"];
