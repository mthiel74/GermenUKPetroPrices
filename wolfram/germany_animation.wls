#!/usr/bin/env wolframscript
(* germany_animation.wls — country-wide visualisation, in three panels.

   1. Station-density map of all 8190 German stations whose locations
      we fetched live from creativecommons.tankerkoenig.de. (Live prices
      from the demo API are placeholders; the coordinates are real.)
   2. A static Paderborn-region detail showing actual current diesel
      prices in our 10 km cluster.
   3. An animated weekly evolution of the Paderborn 29-station cluster
      from 2025-09 to 2026-05 — the "supply-shock wave" arriving,
      the 12-Uhr-Regel cut-off marked on each frame.

   Outputs:
     docs/images/germany_snapshot.png        — country station network
     docs/images/paderborn_snapshot.png      — local price detail
     docs/images/paderborn_animation.gif     — weekly evolution
     docs/images/germany_frames/             — per-frame PNGs (committed)
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

(* ---------------------------------------------------------------- *)
(* 1. Country-wide station network (real coordinates only)          *)

snap = Normal @ Import["data/tankerkoenig_snapshot.csv", "Dataset", "HeaderLines" -> 1];
snap = Select[snap, NumberQ[#lat] && NumberQ[#lng] &];
Print["[anim] country stations: ", Length[snap]];

countryMap = GeoGraphics[
  {FaceForm[Lighter[Gray, 0.92]], EdgeForm[GrayLevel[0.5]],
   CountryData["Germany", "Polygon"],
   Black, PointSize[0.003],
   Point @ GeoPosition @ ({#lat, #lng} & /@ snap)},
  GeoRange -> Entity["Country", "Germany"],
  GeoProjection -> "Mercator",
  GeoBackground -> White,
  PlotLabel -> Style[
    StringTemplate["German fuel-station network: `1` stations (Tankerkonig live API)"][
      Length[snap]],
    18, Bold, GrayLevel[0.1]],
  ImageSize -> 1100];

Print["[anim] writing country-wide network map ..."];
Export["docs/images/germany_snapshot.png", countryMap, ImageResolution -> 144];

(* ---------------------------------------------------------------- *)
(* 2. Paderborn — actual recent diesel prices                       *)

Print["[anim] loading Paderborn time series ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];

(* Latest 24 h *)
latest = AbsoluteTime @ SortBy[deDiesel, AbsoluteTime[#TimestampUTC] &][[-1, Key["TimestampUTC"]]];
recentRows = Select[deDiesel, latest - AbsoluteTime[#TimestampUTC] <= 86400 &];
stnLatest = With[{groups = GroupBy[recentRows, #StationID &]},
  KeyValueMap[
    Function[{sid, rs},
      <|"StationID" -> sid, "Lat" -> First[rs]["Lat"],
        "Lon" -> First[rs]["Lon"], "Brand" -> First[rs]["Brand"],
        "Price" -> Mean[#Price & /@ rs]|>], groups]];

priceMin = Min[#Price & /@ stnLatest];
priceMax = Max[#Price & /@ stnLatest];
priceSpan = Max[priceMax - priceMin, 0.01];
colorFn = Function[p,
  ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]]];

makeRegionalMap[stns_, priceCol_, label_] := GeoGraphics[
  Map[{colorFn[#[priceCol]], PointSize[0.035],
       Tooltip[Point @ GeoPosition[{#Lat, #Lon}],
               #Brand <> "  " <> ToString @ NumberForm[#[priceCol], {4, 3}]]} &,
      stns],
  GeoRange -> {{Min[#Lat & /@ stns] - 0.05, Max[#Lat & /@ stns] + 0.05},
               {Min[#Lon & /@ stns] - 0.05, Max[#Lon & /@ stns] + 0.05}},
  GeoProjection -> "Mercator",
  GeoBackground -> "StreetMapNoLabels",
  PlotLabel -> Style[label, 16, Bold, GrayLevel[0.1]],
  ImageSize -> 1000];

paderbornNow = makeRegionalMap[stnLatest, "Price",
  "Paderborn 10 km: diesel last 24 h. " <>
  "Range €" <> ToString @ NumberForm[priceMin, {4, 3}] <>
  "–€" <> ToString @ NumberForm[priceMax, {4, 3}] <> "/L"];

Print["[anim] writing Paderborn snapshot ..."];
Export["docs/images/paderborn_snapshot.png", paderbornNow, ImageResolution -> 144];

(* ---------------------------------------------------------------- *)
(* 3. Weekly animation of Paderborn diesel                          *)

weekStart[r_] := With[
  {d = PetrolData`LocalDate[r]},
  DateString[DateObject[d, "Week"], "ISODate"]];

withWeek = Function[r, Append[r, "WeekKey" -> weekStart[r]]] /@ deDiesel;

(* per-station per-week mean *)
weeklyMeans = With[{groups = GroupBy[withWeek, {#WeekKey, #StationID} &]},
  KeyValueMap[
    Function[{key, rows},
      <|"WeekKey" -> key[[1]], "StationID" -> key[[2]],
        "Lat" -> First[rows]["Lat"], "Lon" -> First[rows]["Lon"],
        "Brand" -> First[rows]["Brand"],
        "Price" -> Mean[#Price & /@ rows]|>], groups]];

weekKeys = SortBy[Union @ (#WeekKey & /@ weeklyMeans), # &];
Print["[anim] ", Length[weekKeys], " weeks; first/last: ",
      {First @ weekKeys, Last @ weekKeys}];

(* Build a consistent palette across all weeks *)
allPrices = #Price & /@ weeklyMeans;
animMin = Quantile[allPrices, 0.02];
animMax = Quantile[allPrices, 0.98];
animSpan = Max[animMax - animMin, 0.01];
animColor = Function[p, ColorData["RedBlueTones"][
  Clip[1. - (p - animMin)/animSpan, {0., 1.}]]];
Print["[anim] price range across all weeks: ", {animMin, animMax}];

ruleStart = DateObject[{2026, 4, 1}, "Day"];

frameForWeek[wk_] := Module[{rows, ruleBanner, weekDate, isPost},
  rows = Select[weeklyMeans, #WeekKey == wk &];
  weekDate = DateObject[wk];
  isPost = AbsoluteTime[weekDate] >= AbsoluteTime[ruleStart];
  ruleBanner = If[isPost,
    Style["12-Uhr-Regel ACTIVE", 11, Bold, RGBColor[0.78, 0.07, 0.13]],
    Style["pre 12-Uhr-Regel", 11, Italic, GrayLevel[0.4]]];
  GeoGraphics[
    Map[{animColor[#Price], PointSize[0.035],
         Point @ GeoPosition[{#Lat, #Lon}]} &, rows],
    GeoRange -> {{Min[#Lat & /@ weeklyMeans] - 0.05,
                  Max[#Lat & /@ weeklyMeans] + 0.05},
                 {Min[#Lon & /@ weeklyMeans] - 0.05,
                  Max[#Lon & /@ weeklyMeans] + 0.05}},
    GeoProjection -> "Mercator",
    GeoBackground -> "StreetMapNoLabels",
    PlotLabel -> Column[{
      Style["Paderborn diesel - week of " <> wk, 16, Bold, GrayLevel[0.1]],
      ruleBanner}, Alignment -> Center],
    ImageSize -> 1000]];

framesDir = "docs/images/germany_frames";
If[!FileExistsQ[framesDir], CreateDirectory[framesDir]];

frames = Table[
  Module[{f = frameForWeek[wk]},
    Export[FileNameJoin[{framesDir,
      "paderborn_" <> wk <> ".png"}], f, ImageResolution -> 96];
    f],
  {wk, weekKeys}];

Print["[anim] assembling GIF (", Length[frames], " frames) ..."];
Export["docs/images/paderborn_animation.gif", frames,
  "AnimationRepetitions" -> Infinity,
  "DisplayDurations" -> 0.6];

Print["[anim] done."];
