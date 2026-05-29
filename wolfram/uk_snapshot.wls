#!/usr/bin/env wolframscript
(* uk_snapshot.wls — country-wide UK live diesel snapshot map.

   Uses data/uk_fuelfinder_snapshot.csv (1373 stations, 5 retailers,
   real prices in EUR/L) collected by fetch_fuelfinder.wls.

   Output:
     docs/images/uk_snapshot.png
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

snap = Normal @ Import["data/uk_fuelfinder_snapshot.csv", "Dataset", "HeaderLines" -> 1];
snap = Select[snap, NumberQ[#["price_b7_eur"]] && NumberQ[#lat] && NumberQ[#lng] &];
Print["[uksnap] stations: ", Length[snap]];

prices = #["price_b7_eur"] & /@ snap;
priceMin = Quantile[prices, 0.02];
priceMax = Quantile[prices, 0.98];
priceSpan = Max[priceMax - priceMin, 0.01];
colorFn = Function[p,
  ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]]];

Print["[uksnap] price range (2%, 50%, 98%): ",
      Quantile[prices, {0.02, 0.5, 0.98}]];

stationMarkers = Map[
  {colorFn[#["price_b7_eur"]], PointSize[0.012],
   Point @ GeoPosition[{#["lat"], #["lng"]}]} &,
  snap];

ukMap = GeoGraphics[
  {FaceForm[Lighter[Gray, 0.92]], EdgeForm[GrayLevel[0.6]],
   CountryData["UnitedKingdom", "Polygon"],
   stationMarkers},
  GeoRange -> Entity["Country", "UnitedKingdom"],
  GeoProjection -> "Mercator",
  GeoBackground -> White,
  PlotLabel -> Style[
    StringTemplate[
      "UK diesel (B7) snapshot · `1` stations · median €`2`/L · `3`"][
      Length[snap],
      ToString @ NumberForm[Median[prices], {4, 3}],
      DateString[Now, "ISODate"]], 16, Bold],
  ImageSize -> 1100];

legend = BarLegend[
  {ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
  LegendLabel -> Style["EUR/L", 12, Bold],
  LabelStyle -> 11,
  LegendMarkerSize -> 350];

combined = Legended[ukMap, legend];

Export["docs/images/uk_snapshot.png", combined, ImageResolution -> 144];
Print["[uksnap] wrote docs/images/uk_snapshot.png"];
