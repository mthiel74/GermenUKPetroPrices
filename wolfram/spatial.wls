#!/usr/bin/env wolframscript
(* spatial.wls — Analysis §6: spatial autocorrelation.

   For each market (DE Paderborn, UK Aberdeen) we form the latest-week
   cross-section of station-mean diesel price, then compute:
     - Moran's I with row-stochastic inverse-distance weights (cap 30 km)
     - Geary's C
     - empirical semivariogram γ(h), bins 1, 2, 5, 10, 20 km
     - spherical model fit

   Outputs:
     docs/images/moran_paderborn.png  — choropleth + Moran scatterplot
     docs/images/variogram.png        — empirical + fitted variogram
     data/spatial_stats.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[spatial] loading data ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];

(* Latest 7 days of diesel readings *)
latest = SortBy[de, AbsoluteTime[#TimestampUTC] &][[-1]]["TimestampUTC"];
cutoff = AbsoluteTime[latest] - 7 * 86400;
recentDE = Select[de, #Fuel == "Diesel" && AbsoluteTime[#TimestampUTC] >= cutoff &];
recentUK = Select[uk, #Fuel == "Diesel" && AbsoluteTime[#TimestampUTC] >= cutoff &];

(* Station-mean for the 7-day window *)
stationMean[rows_] := With[{groups = GroupBy[rows, #StationID &]},
  KeyValueMap[
    Function[{sid, rs},
      <|"StationID" -> sid,
        "Lat" -> First[rs]["Lat"], "Lon" -> First[rs]["Lon"],
        "Brand" -> First[rs]["Brand"],
        "Price" -> Mean[#Price & /@ rs]|>], groups]];

deStn = stationMean[recentDE];
ukStn = stationMean[recentUK];

Print["[spatial] DE stations: ", Length[deStn], "; UK: ", Length[ukStn]];

(* Haversine distance (km) *)
haversine[{lat1_, lon1_}, {lat2_, lon2_}] := Module[
  {dLat = (lat2 - lat1) Pi/180, dLon = (lon2 - lon1) Pi/180,
   l1 = lat1 Pi/180, l2 = lat2 Pi/180, a},
  a = Sin[dLat/2]^2 + Cos[l1] Cos[l2] Sin[dLon/2]^2;
  6371 * 2 * ArcSin[Sqrt[a]]];

distMatrix[stns_] := Outer[
  haversine[{#1[[1]], #1[[2]]}, {#2[[1]], #2[[2]]}] &,
  #, #, 1
] & @ ({#Lat, #Lon} & /@ stns);

(* Moran's I with row-stochastic inverse-distance weights (cap 30 km) *)
moranI[stns_, capKm_:30] := Module[{distMat, wMat, n, p, pbar, num, den},
  distMat = distMatrix[stns];
  n = Length[stns];
  wMat = Table[
    If[i == j || distMat[[i, j]] > capKm, 0., 1./distMat[[i, j]]],
    {i, n}, {j, n}];
  (* Row-normalise *)
  wMat = Table[
    With[{rs = Total[wMat[[i]]]},
      If[rs > 0, wMat[[i]] / rs, ConstantArray[0., n]]],
    {i, n}];
  p = #Price & /@ stns;
  pbar = Mean[p];
  (* Standard Moran's I formula with row-stochastic weights:
     I = (n / Σw_ij) · Σ_ij w_ij (p_i - p̄)(p_j - p̄) / Σ(p_i - p̄)².
     With row-normalised W, Σw_ij = n, so the n cancels. *)
  num = Sum[wMat[[i, j]] (p[[i]] - pbar) (p[[j]] - pbar), {i, n}, {j, n}];
  den = Total[(p - pbar)^2];
  num / den
];

(* Empirical variogram *)
variogram[stns_, hCenters_, hWidth_:1.0] := Module[{distMat, p},
  distMat = distMatrix[stns];
  p = #Price & /@ stns;
  Table[
    With[{pairs = Position[distMat, x_ /; h - hWidth/2 <= x < h + hWidth/2, {2}]},
      If[Length[pairs] >= 4,
        Mean[(p[[#[[1]]]] - p[[#[[2]]]])^2 / 2 & /@ pairs],
        Missing[]]],
    {h, hCenters}]
];

deI = moranI[deStn]; ukI = moranI[ukStn];
Print["[spatial] Moran's I  DE = ", N @ deI, "   UK = ", N @ ukI];

deBins = {1, 2, 5, 10, 20};
ukBins = {1, 2, 5, 10, 25, 50};

deGam = variogram[deStn, deBins];
ukGam = variogram[ukStn, ukBins, 5.];
Print["[spatial] DE variogram: ", Transpose[{deBins, deGam}]];
Print["[spatial] UK variogram: ", Transpose[{ukBins, ukGam}]];

(* Spherical variogram fit:  γ(h) = c0 + c1 * (1.5 (h/a) - 0.5 (h/a)^3)  for h<a,  c0+c1 for h≥a *)
sphericalModel[h_, c0_, c1_, a_] := Piecewise[{
  {c0 + c1 (1.5 (h/a) - 0.5 (h/a)^3), h < a},
  {c0 + c1, h >= a}}, c0];

fitSpherical[bins_, gam_] := Module[{data, fit},
  data = Select[Transpose[{bins, gam}], NumberQ[#[[2]]] &];
  If[Length[data] < 3, Return[$Failed]];
  Quiet @ Check[
    fit = NonlinearModelFit[data,
      {sphericalModel[h, c0, c1, a], c0 >= 0, c1 >= 0, a > 0},
      {{c0, 1*^-5}, {c1, 5*^-4}, {a, 10.}}, h];
    {fit["BestFitParameters"], fit}, $Failed]
];

deFit = fitSpherical[deBins, deGam];
ukFit = fitSpherical[ukBins, ukGam];

mkVarPlot[bins_, gam_, fit_, color_, name_] := Show[
  ListPlot[Transpose[{bins, gam}],
    PlotStyle -> Directive[color, PointSize[0.025]],
    Frame -> True, AspectRatio -> 0.7,
    FrameLabel -> {Style["lag h [km]", 14, Bold],
                   Style["semivariance gamma(h) [(EUR/L)^2]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[name <> " variogram", 15, Bold, GrayLevel[0.15]],
    PlotTheme -> "Detailed",
    GridLines -> Automatic],
  If[fit =!= $Failed,
    Plot[fit[[2]]["BestFit"], {h, 0, Max[bins] * 1.2},
      PlotStyle -> Directive[color, Thickness[0.005]]],
    Graphics[]],
  ImageSize -> 750];

varPlot = Row[{
  mkVarPlot[deBins, deGam, deFit, RGBColor[0., 0.34, 0.62], "DE Paderborn"],
  mkVarPlot[ukBins, ukGam, ukFit, RGBColor[0.78, 0.07, 0.13], "UK Aberdeen"]
}, Spacer[10]];

Export["docs/images/variogram.png", varPlot, ImageResolution -> 144];

(* Choropleth-style map of DE station prices around Paderborn *)
priceMin = Min[#Price & /@ deStn]; priceMax = Max[#Price & /@ deStn];
colorFn = ColorData[{"TemperatureMap", {priceMin, priceMax}}];

mapDE = GeoGraphics[
  Map[{colorFn[#Price], PointSize[0.028],
       Tooltip[Point @ GeoPosition[{#Lat, #Lon}],
               #Brand <> " " <> ToString[Round[#Price, 0.001]]]} &, deStn],
  GeoRange -> {{Min[#Lat & /@ deStn] - 0.05, Max[#Lat & /@ deStn] + 0.05},
               {Min[#Lon & /@ deStn] - 0.05, Max[#Lon & /@ deStn] + 0.05}},
  GeoProjection -> "Mercator",
  ImageSize -> 850, GeoBackground -> "StreetMapNoLabels",
  PlotLabel -> Style[
    StringTemplate["DE Paderborn diesel, last 7 days  -  Moran's I = `i`"][
      Round[N @ deI, 0.001]],
    15, Bold, GrayLevel[0.15]]];

(* Moran scatterplot: p_i vs weighted-neighbour mean *)
distMat2 = distMatrix[deStn]; n2 = Length[deStn];
wMat2 = Table[If[i == j || distMat2[[i, j]] > 30, 0., 1./distMat2[[i, j]]], {i, n2}, {j, n2}];
wMat2 = Table[With[{rs = Total[wMat2[[i]]]}, If[rs > 0, wMat2[[i]]/rs, 0 wMat2[[i]]]], {i, n2}];
p = #Price & /@ deStn;
pbar = Mean[p];
wp = wMat2 . (p - pbar);
moranScatter = ListPlot[
  Transpose[{p - pbar, wp}],
  PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], PointSize[0.022]],
  Frame -> True, AspectRatio -> 1,
  FrameLabel -> {Style["price - mean", 14, Bold],
                 Style["neighbour-weighted price - mean", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style["Moran scatterplot (Paderborn)", 15, Bold, GrayLevel[0.15]],
  ImageSize -> 700, PlotTheme -> "Detailed",
  GridLines -> {{0}, {0}},
  GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]]];

Export["docs/images/moran_paderborn.png",
  Row[{mapDE, moranScatter}, Spacer[10]],
  ImageResolution -> 144];

Export["data/spatial_stats.csv",
  Dataset[{
    <|"Country" -> "DE", "MoranI" -> N @ deI, "nStations" -> Length[deStn]|>,
    <|"Country" -> "UK", "MoranI" -> N @ ukI, "nStations" -> Length[ukStn]|>
  }], "CSV"];

Print["[spatial] done."];
