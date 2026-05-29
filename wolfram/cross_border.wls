#!/usr/bin/env wolframscript
(* cross_border.wls — Analysis §7: cross-border station density.

   Note: live-API prices from the demo Tankerkönig key are not real
   (the demo returns placeholder 1.009 EUR/L for every station). The
   coordinates ARE real. So this script visualises station density
   along the western border (NL/BE/LU/FR) and the southern border
   (CH/AT), which is itself an empirical observation about market
   structure — German stations cluster near borders where Germans cross
   to fuel up on the cheaper side.

   Outputs:
     docs/images/cross_border.png      — map highlighting border bands
     data/cross_border_stats.csv       — station density vs border distance
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

snap = Normal @ Import["data/tankerkoenig_snapshot.csv", "Dataset", "HeaderLines" -> 1];
snap = Select[snap, NumberQ[#lat] && NumberQ[#lng] &];
Print["[xb] total stations: ", Length[snap]];

(* Compute approximate distance to nearest non-German country border.
   Use the country polygons from CountryData. *)

neighbours = {"Netherlands", "Belgium", "Luxembourg", "France",
              "Switzerland", "Austria", "CzechRepublic", "Poland",
              "Denmark"};

neighborGeoms = Quiet @ Map[
  Function[c, CountryData[c, "Polygon"]],
  neighbours];

(* haversine in km *)
hav[{lat1_, lon1_}, {lat2_, lon2_}] := Module[
  {dLat = (lat2 - lat1) Pi/180, dLon = (lon2 - lon1) Pi/180,
   l1 = lat1 Pi/180, l2 = lat2 Pi/180},
  6371 * 2 * ArcSin[Sqrt[
    Sin[dLat/2]^2 + Cos[l1] Cos[l2] Sin[dLon/2]^2]]];

(* Sample boundary vertices from each neighbour polygon via "Coordinates" *)
extractCoords[poly_] := Module[{coords},
  coords = Cases[poly, _Real | _Integer, Infinity];
  Partition[coords, 2]];

borderPoints = Catenate[extractCoords /@ neighborGeoms];
borderPoints = Select[borderPoints,
  Length[#] == 2 && 46 < #[[1]] < 56 && 4 < #[[2]] < 17 &];
(* Subsample for speed *)
If[Length[borderPoints] > 5000,
  borderPoints = RandomSample[borderPoints, 5000]];
Print["[xb] border vertex samples: ", Length[borderPoints]];

(* For each station compute min haversine to any border vertex
   (approximation, fine for this study) *)
nearestBorderKm[{stnLat_, stnLon_}] :=
  Min @ Map[hav[{stnLat, stnLon}, {#[[1]], #[[2]]}] &, borderPoints];

Print["[xb] computing border distances (sampling 800 stations) ..."];
sample = RandomSample[snap, UpTo[800]];
sampleWithD = Map[
  Function[s,
    <|s, "BorderKm" -> nearestBorderKm[{s["lat"], s["lng"]}]|>],
  sample];

(* Density vs border distance — binned *)
binEdges = {0, 5, 10, 20, 50, 100, 200, 500, 1000};
binnedCount = Table[
  Length @ Select[sampleWithD,
    binEdges[[i]] <= #BorderKm < binEdges[[i + 1]] &],
  {i, 1, Length[binEdges] - 1}];
binMid = MovingAverage[binEdges, 2];

Print["[xb] station-density by border-distance bin:"];
Do[Print["    ", binEdges[[i]], "-", binEdges[[i+1]], " km: ",
        binnedCount[[i]], " stations"], {i, 1, Length[binEdges] - 1}];

(* Plot *)
densityPlot = BarChart[binnedCount,
  ChartLabels -> Placed[
    Style[#, 12, Bold] & /@ (StringTemplate["``-``"] @@@ Partition[binEdges, 2, 1]),
    Below],
  Frame -> True,
  FrameLabel -> {Style["distance to nearest foreign border [km]", 14, Bold],
                 Style["stations in 800-sample", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    "German station count vs distance to nearest foreign border",
    16, Bold, GrayLevel[0.15]],
  ChartStyle -> RGBColor[0., 0.34, 0.62],
  ImageSize -> 1100, AspectRatio -> 1/2.4];

(* Map with stations color-coded by border distance *)
maxD = Max[#BorderKm & /@ sampleWithD];
colorFn = Function[d, ColorData["TemperatureMap"][Clip[d/maxD, {0, 1}]]];

mapPlot = GeoGraphics[
  {FaceForm[Lighter[Gray, 0.92]], EdgeForm[GrayLevel[0.5]],
   CountryData["Germany", "Polygon"],
   Map[{colorFn[#BorderKm], PointSize[0.008],
        Point @ GeoPosition[{#lat, #lng}]} &, sampleWithD]},
  GeoRange -> Entity["Country", "Germany"],
  GeoProjection -> "Mercator",
  GeoBackground -> White,
  PlotLabel -> Style[
    "Station coordinates colored by distance to nearest foreign border",
    16, Bold, GrayLevel[0.15]],
  ImageSize -> 1100];

Export["docs/images/cross_border.png",
  Column[{mapPlot, densityPlot}, Spacings -> 0.3],
  ImageResolution -> 144];

Export["data/cross_border_stats.csv",
  Dataset @ Table[
    <|"bin_low_km" -> binEdges[[i]],
      "bin_high_km" -> binEdges[[i+1]],
      "n_stations" -> binnedCount[[i]]|>,
    {i, 1, Length[binEdges] - 1}],
  "CSV"];

Print["[xb] done."];
