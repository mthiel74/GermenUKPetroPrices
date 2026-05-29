(* ::Package:: *)

(* PetrolFigures.wl — every notebook figure as a single named function.

   The notebook (german_uk_petrol_prices.nb) stays tidy: it imports the
   library once and then, for each figure, shows one call with its
   arguments. The full code that builds each figure lives HERE, so the
   notebook is complete (all code is in the uploaded library) without
   being buried in source.

   Each function loads the data it needs (from the shared PetrolData`
   loaders or a committed CSV), performs the analysis, and RETURNS the
   graphic (the same expression the standalone wolfram/*.wls scripts
   Export to PNG). Side effects -- Print logging and CSV/PNG writing --
   are intentionally omitted here; the standalone scripts keep those.

   Data dependencies (a reader needs these to actually run a function):
     * DE/UK high-frequency feeds  -> PetrolData`Settings paths
       (the Paderborn / Aberdeen local collections)
     * data/uk_fuelfinder_snapshot.csv, data/brent_daily.csv,
       data/tankerkoenig_snapshot.csv, data/raw/gustavz/stations.csv
     * EIA Brent (downloaded live by HotellingRule)
*)

BeginPackage["PetrolFigures`", {"PetrolData`"}];

SnapshotUK::usage         = "SnapshotUK[] -> UK live diesel snapshot map (notebook §2).";
StationNetworkDE::usage   = "StationNetworkDE[] -> German station-network map (§2).";
HeadlineInversion::usage  = "HeadlineInversion[] -> two-panel intraday-cycle inversion (§3).";
HazardPrePost::usage      = "HazardPrePost[] -> 4-panel price-change hazard, pre/post × up/down (§4).";
TimeSeriesDEUK::usage     = "TimeSeriesDEUK[] -> raw DE vs UK diesel time series, 3 panels (§5).";
AmplitudePrePost::usage   = "AmplitudePrePost[] -> daily-amplitude distribution shift (§6).";
DiDPlot::usage            = "DiDPlot[] -> DE-vs-UK daily-mean diesel for the difference-in-differences (§7).";
DayOfWeekEffects::usage   = "DayOfWeekEffects[] -> station-demeaned day-of-week effects, DE & UK (§8).";
SpatialAnalysis::usage    = "SpatialAnalysis[] -> <|\"moran\"->.., \"variogram\"->..|> for the two §9 figures.";
CrossBorder::usage        = "CrossBorder[] -> border-distance map + density bar chart (§10).";
BrentLag::usage           = "BrentLag[kLag] -> asymmetric Brent->diesel impulse responses (§11); kLag defaults to 21.";
HotellingRule::usage      = "HotellingRule[] -> long-run real Brent vs the Hotelling path (§12).";

Begin["`Private`"];

(* ================================================================ *)
(* §2  UK snapshot                                                  *)
(* ================================================================ *)
SnapshotUK[] := Module[
  {snap, prices, priceMin, priceMax, priceSpan, colorFn, stationMarkers,
   ukMap, legend},
  snap = Normal @ Import["data/uk_fuelfinder_snapshot.csv", "Dataset", "HeaderLines" -> 1];
  snap = Select[snap, NumberQ[#["price_b7_eur"]] && NumberQ[#lat] && NumberQ[#lng] &];
  prices = #["price_b7_eur"] & /@ snap;
  priceMin = Quantile[prices, 0.02]; priceMax = Quantile[prices, 0.98];
  priceSpan = Max[priceMax - priceMin, 0.01];
  colorFn = Function[p, ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]]];
  stationMarkers = Map[
    {colorFn[#["price_b7_eur"]], PointSize[0.012],
     Point @ GeoPosition[{#["lat"], #["lng"]}]} &, snap];
  ukMap = GeoGraphics[
    {FaceForm[Lighter[Gray, 0.92]], EdgeForm[GrayLevel[0.6]],
     CountryData["UnitedKingdom", "Polygon"], stationMarkers},
    GeoRange -> Entity["Country", "UnitedKingdom"], GeoProjection -> "Mercator",
    GeoBackground -> White,
    PlotLabel -> Style[StringTemplate[
        "UK diesel (B7) snapshot · `1` stations · median €`2`/L · `3`"][
        Length[snap], ToString @ NumberForm[Median[prices], {4, 3}],
        DateString[Now, "ISODate"]], 16, Bold],
    ImageSize -> 1100];
  legend = BarLegend[{ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
    LegendLabel -> Style["EUR/L", 12, Bold], LabelStyle -> 11, LegendMarkerSize -> 350];
  Legended[ukMap, legend]
];

(* ================================================================ *)
(* §2  German station-network map                                  *)
(* ================================================================ *)
StationNetworkDE[] := Module[{stations},
  stations = Normal @ Import["data/raw/gustavz/stations.csv", "Dataset", "HeaderLines" -> 1];
  stations = Select[stations,
    NumberQ[#latitude] && NumberQ[#longitude] && 47 < #latitude < 56 && 5 < #longitude < 16 &];
  GeoGraphics[
    {FaceForm[Lighter[Gray, 0.94]], EdgeForm[GrayLevel[0.4]],
     CountryData["Germany", "Polygon"], Black, PointSize[0.002], Opacity[0.6],
     Point @ GeoPosition @ ({#latitude, #longitude} & /@ stations)},
    GeoRange -> Entity["Country", "Germany"], GeoProjection -> "Mercator",
    GeoBackground -> White,
    PlotLabel -> Style[StringTemplate[
        "German fuel-station network: `1` stations (Tankerkonig MTS-K master list)"][
        Length[stations]], 18, Bold, GrayLevel[0.1]],
    ImageSize -> 1100]
];

(* ================================================================ *)
(* §3  Headline intraday-cycle inversion                           *)
(* ================================================================ *)
HeadlineInversion[] := Module[
  {deAll, deDiesel, withLocal, dailyMeansDsl, residuals, aggMed, workdayProfile,
   preLine, postLine, teal, ember, panel, prePeakHr, postPeakHr, panA, panB},
  deAll = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  deDiesel = Select[deAll, #Fuel == "Diesel" &];
  withLocal = Function[r, Append[r, {
      "LocalHour" -> PetrolData`LocalHour[r], "LocalDate" -> PetrolData`LocalDate[r],
      "LocalWeekday" -> PetrolData`LocalWeekday[r], "AfterRule" -> PetrolData`IsAfterRule[r]}]] /@ deDiesel;
  dailyMeansDsl = Mean[#Price & /@ #] & /@ GroupBy[withLocal, {#StationID, #LocalDate} &];
  residuals = Function[r, Append[r, "Residual" ->
      r["Price"] - dailyMeansDsl[{r["StationID"], r["LocalDate"]}]]] /@ withLocal;
  residuals = Select[residuals, 1 <= #LocalWeekday <= 5 &];
  aggMed = Median[#Residual & /@ #] & /@ GroupBy[residuals, {#AfterRule, #LocalHour} &];
  workdayProfile[afterRule_] := Table[Lookup[aggMed, Key[{afterRule, hr}], 0.], {hr, 0, 23}];
  preLine = workdayProfile[False]; postLine = workdayProfile[True];
  teal = RGBColor[0.18, 0.45, 0.55]; ember = RGBColor[0.90, 0.45, 0.10];
  panel[trace_, color_, title_, peakLabel_, peakHour_] := Module[{maxV},
    maxV = Max[trace];
    ListLinePlot[Transpose[{Range[0, 23], trace}],
      PlotStyle -> Directive[color, Thickness[0.008]],
      Filling -> Bottom, FillingStyle -> Directive[color, Opacity[0.15]],
      Frame -> True, Axes -> False,
      FrameLabel -> {"hour of day (Europe/Berlin)", "median (price - daily mean) [EUR/L]"},
      FrameStyle -> Directive[GrayLevel[0.3], Thickness[0.001]],
      PlotLabel -> Style[title, 16, Bold, GrayLevel[0.15], FontFamily -> "Helvetica"],
      PlotRange -> {{-0.5, 24}, {-0.04, 0.12}},
      GridLines -> {Range[0, 24, 6], {0}}, GridLinesStyle -> Directive[GrayLevel[0.85], Dashed],
      Epilog -> {Directive[color, PointSize[0.018]], Point[{peakHour, maxV}],
        Inset[Style[peakLabel, 12, Bold, color],
          {If[peakHour <= 10, peakHour, peakHour + 4], maxV + 0.015}, {0, 0}],
        Directive[GrayLevel[0.45], Thickness[0.002], Dashed], Line[{{12, -0.04}, {12, 0.135}}],
        Inset[Style["12:00", 10, GrayLevel[0.45]], {12.4, -0.035}, {-1, 0}]},
      ImageSize -> 1000, AspectRatio -> 1/2.4]];
  prePeakHr = First[Ordering[preLine, -1]] - 1;
  postPeakHr = First[Ordering[postLine, -1]] - 1;
  panA = panel[preLine, teal, "BEFORE 12-Uhr-Regel  --  morning-led cycle",
    "morning peak +" <> ToString @ Round[Max[preLine] * 100] <> " c/L at " <>
      IntegerString[prePeakHr, 10, 2] <> ":00", prePeakHr];
  panB = panel[postLine, ember, "AFTER 12-Uhr-Regel  --  noon-led cycle",
    "12:00 spike +" <> ToString @ Round[Max[postLine] * 100] <> " c/L", postPeakHr];
  Column[{Style["Paderborn diesel intraday cycle: before vs after 1 April 2026", 18, Bold, GrayLevel[0.1]],
    Spacer[5], panA, Spacer[5], panB}, Alignment -> Center, Spacings -> 0.3]
];

(* ================================================================ *)
(* §4  Price-change hazard, pre/post × up/down                     *)
(* ================================================================ *)
HazardPrePost[] := Module[
  {deAll, wide, byStation, changeEvents, withMin, counts, deWithLocal, stationDays,
   hazard, hr, mkPlot, up, down, crimson, panels},
  deAll = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  wide = KeyValueMap[
    Function[{key, rows}, With[{r1 = First @ rows},
      <|"StationID" -> r1["StationID"], "TimestampUTC" -> r1["TimestampUTC"],
        "TimeZone" -> r1["TimeZone"],
        "E5" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "E5" &], NumberQ, Missing[]],
        "E10" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "E10" &], NumberQ, Missing[]],
        "Diesel" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "Diesel" &], NumberQ, Missing[]]|>]],
    GroupBy[deAll, {#StationID, #TimestampUTC} &]];
  byStation = SortBy[#, AbsoluteTime[#TimestampUTC] &] & /@ GroupBy[wide, #StationID &];
  changeEvents = Catenate @ KeyValueMap[
    Function[{sid, srows}, Module[{pairs = Partition[srows, 2, 1]},
      Map[Function[pair, With[{a = pair[[1]], b = pair[[2]]},
          Module[{dD = b["Diesel"] - a["Diesel"], dE = b["E5"] - a["E5"]},
            If[NumberQ[dD] && Abs[dD] > 0.001,
              <|"StationID" -> sid, "TimestampUTC" -> b["TimestampUTC"], "TimeZone" -> b["TimeZone"],
                "Direction" -> If[dD > 0, "up", "down"], "Delta" -> dD|>,
              If[NumberQ[dE] && Abs[dE] > 0.001,
                <|"StationID" -> sid, "TimestampUTC" -> b["TimestampUTC"], "TimeZone" -> b["TimeZone"],
                  "Direction" -> If[dE > 0, "up", "down"], "Delta" -> dE|>, Nothing]]]]], pairs]]],
    byStation];
  changeEvents = DeleteCases[changeEvents, Nothing];
  withMin = (Append[#, {
    "MinuteOfDay" -> Module[{lc = First @ TimeZoneConvert[#TimestampUTC, #TimeZone]}, lc[[4]] * 60 + lc[[5]]],
    "AfterRule" -> PetrolData`IsAfterRule[#]}] &) /@ changeEvents;
  counts = Length /@ GroupBy[withMin, {#Direction, #AfterRule, Quotient[#MinuteOfDay, 5]} &];
  deWithLocal = (Append[#, {"AfterRule" -> PetrolData`IsAfterRule[#],
    "LocalDate" -> PetrolData`LocalDate[#]}] &) /@ deAll;
  stationDays = Length @ Union[{#StationID, #LocalDate} & /@ #] & /@ GroupBy[deWithLocal, #AfterRule &];
  hazard[direction_, afterRule_] := Table[
    Lookup[counts, Key[{direction, afterRule, b}], 0] /
      N[Lookup[stationDays, afterRule, 1] * (5/(60*24))], {b, 0, 287}];
  hr = Range[0, 287] * 5 / 60.;
  crimson = RGBColor[0.78, 0.10, 0.18];
  mkPlot[direction_, afterRule_, color_, dash_, label_] :=
    ListLinePlot[Transpose[{hr, hazard[direction, afterRule]}],
      PlotStyle -> {color, Thickness[0.006], dash},
      PlotLabel -> Style[label, 15, Bold, GrayLevel[0.15]], Frame -> True,
      FrameLabel -> {Style["local hour (Europe/Berlin)", 14, Bold],
                     Style["events / station / hour", 13, Bold]},
      FrameTicksStyle -> Directive[FontSize -> 12], PlotRange -> {{0, 24}, All},
      GridLines -> {Range[0, 24, 2], Automatic},
      GridLinesStyle -> Directive[GrayLevel[0.85], Dotted],
      Epilog -> {Directive[crimson, Dashed, Thickness[0.003]], Line[{{12, 0}, {12, 1000}}],
        Text[Style["12:00", 12, Italic, Bold, crimson], {12.5, Scaled[0.92]}, {-1, 0}]},
      ImageSize -> 700, AspectRatio -> 1/1.6, PlotTheme -> "Detailed"];
  up = RGBColor[0.85, 0.27, 0.27]; down = RGBColor[0.20, 0.60, 0.86];
  panels = {mkPlot["up", False, up, None, "Price UP events, pre 12-Uhr-Regel"],
    mkPlot["up", True, up, None, "Price UP events, post 12-Uhr-Regel"],
    mkPlot["down", False, down, None, "Price DOWN events, pre 12-Uhr-Regel"],
    mkPlot["down", True, down, None, "Price DOWN events, post 12-Uhr-Regel"]};
  Grid[Partition[panels, 2], Spacings -> {1, 1}, Frame -> All, FrameStyle -> GrayLevel[0.85]]
];

(* ================================================================ *)
(* §5  Raw DE vs UK time series                                    *)
(* ================================================================ *)
TimeSeriesDEUK[] := Module[
  {de, uk, deCounts, deRepID, deRep, deName, ukCounts, ukRepID, ukRep, ukName,
   dePts, ukPts, ruleStart, brentPeakDate, deFull, zoomStart, zoomEnd, inWin,
   deZoom, ukWin, ukZoom},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
  deCounts = Counts[#StationID & /@ Select[de, #Fuel == "Diesel" &]];
  deRepID = First @ Keys @ ReverseSort @ deCounts;
  deRep = Select[de, #StationID == deRepID && #Fuel == "Diesel" &];
  deName = First[deRep]["Brand"] <> " " <> First[deRep]["City"];
  ukCounts = Counts[#StationID & /@ Select[uk, #Fuel == "Diesel" &]];
  ukRepID = First @ Keys @ ReverseSort @ ukCounts;
  ukRep = Select[uk, #StationID == ukRepID && #Fuel == "Diesel" &];
  ukName = First[ukRep]["Brand"] <> " " <> First[ukRep]["Postcode"];
  dePts = SortBy[{#TimestampUTC, #Price} & /@ deRep, AbsoluteTime[#[[1]]] &];
  ukPts = SortBy[{#TimestampUTC, #Price} & /@ ukRep, AbsoluteTime[#[[1]]] &];
  ruleStart = DateObject[{2026, 4, 1}, "Day", TimeZone -> "Europe/Berlin"];
  brentPeakDate = DateObject[{2026, 4, 15}, "Day"];
  deFull = DateListPlot[dePts, Joined -> True,
    PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], Thickness[0.0025]],
    Frame -> True, AspectRatio -> 1/3.5,
    FrameLabel -> {Style["date", 14, Bold], Style["diesel [EUR/L]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[deName <> " - 9-month diesel time series  -  Sep 2025 to May 2026", 16, Bold, GrayLevel[0.1]],
    GridLines -> {{ruleStart, brentPeakDate}, Automatic},
    GridLinesStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Dashed, Thickness[0.0025]],
    Epilog -> {Text[Style["12-Uhr-Regel\n1 Apr 2026", 11, Italic, Bold, RGBColor[0.78, 0.07, 0.13]], Scaled[{0.72, 0.93}], {0, 0}],
      Text[Style["Brent peak\n$114/bbl", 10, Italic, Bold, RGBColor[0.78, 0.07, 0.13]], Scaled[{0.83, 0.93}], {0, 0}]},
    ImageSize -> 1300, PlotTheme -> "Detailed"];
  zoomStart = DateObject[{2026, 3, 25}, "Day"]; zoomEnd = DateObject[{2026, 4, 8}, "Day"];
  inWin = Select[dePts, AbsoluteTime[#[[1]]] >= AbsoluteTime[zoomStart] && AbsoluteTime[#[[1]]] <= AbsoluteTime[zoomEnd] &];
  deZoom = DateListPlot[inWin, Joined -> True, PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], Thickness[0.004]],
    Frame -> True, AspectRatio -> 1/3,
    FrameLabel -> {Style["local date", 14, Bold], Style["diesel [EUR/L]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[deName <> " - two weeks around the 12-Uhr-Regel (zoom)", 16, Bold, GrayLevel[0.1]],
    GridLines -> {{ruleStart}, Automatic},
    GridLinesStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Dashed, Thickness[0.003]],
    ImageSize -> 1300, PlotTheme -> "Detailed"];
  ukWin = Select[ukPts, AbsoluteTime[#[[1]]] >= AbsoluteTime[zoomStart] && AbsoluteTime[#[[1]]] <= AbsoluteTime[zoomEnd] &];
  ukZoom = DateListPlot[ukWin, Joined -> True, PlotStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.004]],
    Frame -> True, AspectRatio -> 1/3,
    FrameLabel -> {Style["local date", 14, Bold], Style["diesel [EUR/L]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[ukName <> " - same two weeks (UK control market, no rule)", 16, Bold, GrayLevel[0.1]],
    ImageSize -> 1300, PlotTheme -> "Detailed"];
  Column[{deFull, deZoom, ukZoom}, Spacings -> 0.3]
];

(* ================================================================ *)
(* §6  Daily-amplitude distribution                                *)
(* ================================================================ *)
AmplitudePrePost[] := Module[
  {de, deDiesel, withLocal, ampByStnDay, prePost, preA, postA, pVal, nPre, nPost,
   deltaPairs, cliffsDelta, shifts, ci95, preColor, postColor, violinPlot, stripPlot},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  deDiesel = Select[de, #Fuel == "Diesel" &];
  withLocal = Function[r, Append[r, {"LocalDate" -> PetrolData`LocalDate[r], "AfterRule" -> PetrolData`IsAfterRule[r]}]] /@ deDiesel;
  ampByStnDay = KeyValueMap[
    Function[{k, rows}, <|"StationID" -> k[[1]], "LocalDate" -> k[[2]],
      "AfterRule" -> First[rows]["AfterRule"],
      "Amplitude" -> Max[#Price & /@ rows] - Min[#Price & /@ rows]|>],
    GroupBy[withLocal, {#StationID, #LocalDate} &]];
  prePost = GroupBy[ampByStnDay, #AfterRule &];
  preA = #Amplitude & /@ prePost[False]; postA = #Amplitude & /@ prePost[True];
  pVal = MannWhitneyTest[{preA, postA}];
  nPre = Length[preA]; nPost = Length[postA];
  deltaPairs = Total @ Flatten @ Table[Sign[a - b], {a, preA}, {b, postA}];
  cliffsDelta = deltaPairs / N[nPre * nPost];
  SeedRandom[42];
  shifts = Table[Median @ RandomChoice[preA, nPre] - Median @ RandomChoice[postA, nPost], {500}];
  ci95 = Quantile[shifts, {0.025, 0.5, 0.975}];
  preColor = RGBColor[0.18, 0.50, 0.62]; postColor = RGBColor[0.91, 0.45, 0.10];
  violinPlot = SmoothHistogram[{preA, postA},
    PlotStyle -> {Directive[preColor, Thickness[0.005]], Directive[postColor, Thickness[0.005]]},
    Filling -> Axis, FillingStyle -> {Directive[preColor, Opacity[0.25]], Directive[postColor, Opacity[0.25]]},
    PlotLegends -> Placed[LineLegend[{preColor, postColor},
      {Style["pre 12-Uhr-Regel", 13], Style["post 12-Uhr-Regel", 13]}], {0.80, 0.85}],
    Frame -> True, FrameLabel -> {Style["daily amplitude (max - min) [EUR/L]", 14, Bold], Style["density", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style["DE diesel: daily price amplitude, before vs after 12-Uhr-Regel", 16, Bold, GrayLevel[0.15]],
    PlotRange -> {{0, 0.4}, All}, ImageSize -> 1100, AspectRatio -> 1/2.4, PlotTheme -> "Detailed"];
  stripPlot = ListPlot[
    {Transpose[{RandomReal[{0.9, 1.1}, nPre], preA}], Transpose[{RandomReal[{1.9, 2.1}, nPost], postA}]},
    PlotStyle -> {Directive[Opacity[0.30], PointSize[0.005], preColor], Directive[Opacity[0.30], PointSize[0.005], postColor]},
    Frame -> True,
    FrameTicks -> {{Automatic, None}, {{{1, Style["pre 12-Uhr-Regel", 13, Bold]}, {2, Style["post 12-Uhr-Regel", 13, Bold]}}, None}},
    FrameLabel -> {None, Style["daily amplitude [EUR/L]", 13, Bold]}, FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[StringTemplate["median shift `1` EUR/L (95% CI `2`-`3`)  -  Cliff's d = `4`  -  p < `5`"][
        Round[ci95[[2]], 0.001], Round[ci95[[1]], 0.001], Round[ci95[[3]], 0.001],
        Round[cliffsDelta, 0.01], Round[pVal, 0.0001]], 14, Bold, GrayLevel[0.2]],
    PlotRange -> {{0.5, 2.5}, {0, 0.5}}, ImageSize -> 1100, AspectRatio -> 1/2.4, PlotTheme -> "Detailed"];
  Column[{violinPlot, stripPlot}, Spacings -> 0.2]
];

(* ================================================================ *)
(* §7  Difference-in-differences (the DE vs UK daily-mean plot)     *)
(* ================================================================ *)
DiDPlot[] := Module[
  {de, uk, all, withLocal, dailyMean, byDayCountry, deLine, ukLine, toDateNum,
   dePts, ukPts, ruleStart, iranPeak},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
  all = Join[Select[de, #Fuel == "Diesel" &], Select[uk, #Fuel == "Diesel" &]];
  withLocal = Function[r, Append[r, {"LocalDate" -> PetrolData`LocalDate[r],
    "AfterRule" -> PetrolData`IsAfterRule[r], "IsDE" -> Boole[r["Country"] == "DE"]}]] /@ all;
  dailyMean = KeyValueMap[
    Function[{k, rows}, <|"Country" -> k[[1]], "StationID" -> k[[2]], "LocalDate" -> k[[3]],
      "Date" -> DateString[DateObject[k[[3]]], "ISODate"], "AfterRule" -> First[rows]["AfterRule"],
      "IsDE" -> First[rows]["IsDE"], "Price" -> Mean[#Price & /@ rows]|>],
    GroupBy[withLocal, {#Country, #StationID, #LocalDate} &]];
  byDayCountry = KeyValueMap[
    Function[{k, rows}, <|"Country" -> k[[1]], "Date" -> k[[2]], "MeanPrice" -> Mean[#Price & /@ rows]|>],
    GroupBy[dailyMean, {#Country, #Date} &]];
  deLine = SortBy[Select[byDayCountry, #Country == "DE" &], #Date &];
  ukLine = SortBy[Select[byDayCountry, #Country == "UK" &], #Date &];
  toDateNum[s_String] := DateObject[s];
  dePts = Transpose[{toDateNum /@ (#Date & /@ deLine), #MeanPrice & /@ deLine}];
  ukPts = Transpose[{toDateNum /@ (#Date & /@ ukLine), #MeanPrice & /@ ukLine}];
  ruleStart = DateObject[{2026, 4, 1}]; iranPeak = DateObject[{2026, 4, 15}];
  DateListPlot[{dePts, ukPts},
    PlotStyle -> {Directive[RGBColor[0., 0.34, 0.62], Thickness[0.006]], Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.006]]},
    PlotLegends -> Placed[LineLegend[
      {Directive[RGBColor[0., 0.34, 0.62], Thickness[0.006]], Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.006]]},
      {Style["DE (Paderborn, 29 stations)", 13], Style["UK (Aberdeen, 34 stations)", 13]}], {0.18, 0.86}],
    Frame -> True, FrameLabel -> {Style["date", 14, Bold], Style["country-mean diesel price [EUR/L]", 14, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style["Daily-mean diesel: DE (Paderborn) vs UK (Aberdeen)", 18, Bold, GrayLevel[0.1]],
    GridLines -> {{ruleStart, iranPeak}, Automatic}, GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]],
    Epilog -> {Text[Style["12-Uhr-Regel\n1 Apr 2026", 11, Italic, Bold, GrayLevel[0.3]], Scaled[{0.78, 0.94}], {0, 0}],
      Text[Style["Brent peak\nIran war", 10, Italic, RGBColor[0.78, 0.07, 0.13]], Scaled[{0.88, 0.92}], {0, 0}]},
    ImageSize -> 1200, AspectRatio -> 1/2.4, PlotTheme -> "Detailed"]
];

(* ================================================================ *)
(* §8  Day-of-week effects                                         *)
(* ================================================================ *)
DayOfWeekEffects[] := Module[
  {de, uk, all, withLocal, dailyMean, stationMeans, demeaned, estimate,
   deMeans, deSEs, ukMeans, ukSEs, mkPlot},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
  all = Join[Select[de, #Fuel == "Diesel" &], Select[uk, #Fuel == "Diesel" &]];
  withLocal = Function[r, Append[r, {"LocalDate" -> PetrolData`LocalDate[r], "Weekday" -> PetrolData`LocalWeekday[r]}]] /@ all;
  dailyMean = KeyValueMap[
    Function[{k, rows}, <|"Country" -> k[[1]], "StationID" -> k[[2]], "LocalDate" -> k[[3]],
      "Weekday" -> First[rows]["Weekday"], "Price" -> Mean[#Price & /@ rows]|>],
    GroupBy[withLocal, {#Country, #StationID, #LocalDate} &]];
  stationMeans = Mean[#Price & /@ #] & /@ GroupBy[dailyMean, {#Country, #StationID} &];
  demeaned = Function[r, Append[r, "PriceDemean" -> r["Price"] - stationMeans[{r["Country"], r["StationID"]}]]] /@ dailyMean;
  estimate[country_] := Module[{rows, weekdayMeans, weekdaySEs},
    rows = Select[demeaned, #Country == country &];
    weekdayMeans = Table[With[{wdRows = Select[rows, #Weekday == wd &]},
      If[Length[wdRows] > 0, Mean[#PriceDemean & /@ wdRows], Missing[]]], {wd, 1, 7}];
    weekdaySEs = Table[With[{wdRows = Select[rows, #Weekday == wd &]},
      If[Length[wdRows] > 0, StandardDeviation[#PriceDemean & /@ wdRows] / Sqrt[Length[wdRows]], Missing[]]], {wd, 1, 7}];
    {weekdayMeans, weekdaySEs}];
  {deMeans, deSEs} = estimate["DE"]; {ukMeans, ukSEs} = estimate["UK"];
  mkPlot[means_, ses_, color_, country_] := With[
    {centred = means - Mean[means], wdNames = {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}},
    ListPlot[Transpose[{Range[7], centred, Around @@@ Transpose[{centred, ses}]}] // (Transpose[{#[[All, 1]], #[[All, 3]]}] &),
      PlotStyle -> Directive[color, PointSize[0.022]], IntervalMarkers -> "Bars",
      IntervalMarkersStyle -> Directive[color, Thickness[0.004]], Frame -> True,
      FrameTicks -> {{Automatic, None}, {Transpose[{Range[7], Style[#, 13, Bold] & /@ wdNames}], None}},
      FrameLabel -> {Style["weekday", 14, Bold], Style["diesel price - country mean [EUR/L]", 13, Bold]},
      FrameTicksStyle -> Directive[FontSize -> 12],
      PlotLabel -> Style[country <> " day-of-week effect (station-demeaned)", 16, Bold, GrayLevel[0.15]],
      GridLines -> {None, {0}}, GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]],
      PlotRange -> {{0.5, 7.5}, {-0.05, 0.05}}, ImageSize -> 750, AspectRatio -> 0.7, PlotTheme -> "Detailed"]];
  Row[{mkPlot[deMeans, deSEs, RGBColor[0., 0.34, 0.62], "DE (Paderborn)"],
       mkPlot[ukMeans, ukSEs, RGBColor[0.78, 0.07, 0.13], "UK (Aberdeen)"]}, Spacer[10]]
];

(* ================================================================ *)
(* §9  Spatial autocorrelation: Moran map + variogram              *)
(* ================================================================ *)
SpatialAnalysis[] := Module[
  {de, uk, latest, cutoff, recentDE, recentUK, stationMean, deStn, ukStn,
   haversine, distMatrix, moranI, variogram, deI, ukI, deBins, ukBins, deGam, ukGam,
   sphericalModel, fitSpherical, deFit, ukFit, mkVarPlot, varPlot, priceMin, priceMax,
   colorFn, mapDE, distMat2, n2, wMat2, p, pbar, wp, moranScatter},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
  latest = SortBy[de, AbsoluteTime[#TimestampUTC] &][[-1]]["TimestampUTC"];
  cutoff = AbsoluteTime[latest] - 7 * 86400;
  recentDE = Select[de, #Fuel == "Diesel" && AbsoluteTime[#TimestampUTC] >= cutoff &];
  recentUK = Select[uk, #Fuel == "Diesel" && AbsoluteTime[#TimestampUTC] >= cutoff &];
  stationMean[rows_] := KeyValueMap[
    Function[{sid, rs}, <|"StationID" -> sid, "Lat" -> First[rs]["Lat"], "Lon" -> First[rs]["Lon"],
      "Brand" -> First[rs]["Brand"], "Price" -> Mean[#Price & /@ rs]|>], GroupBy[rows, #StationID &]];
  deStn = stationMean[recentDE]; ukStn = stationMean[recentUK];
  haversine[{lat1_, lon1_}, {lat2_, lon2_}] := Module[
    {dLat = (lat2 - lat1) Pi/180, dLon = (lon2 - lon1) Pi/180, l1 = lat1 Pi/180, l2 = lat2 Pi/180, a},
    a = Sin[dLat/2]^2 + Cos[l1] Cos[l2] Sin[dLon/2]^2; 6371 * 2 * ArcSin[Sqrt[a]]];
  distMatrix[stns_] := Outer[haversine[{#1[[1]], #1[[2]]}, {#2[[1]], #2[[2]]}] &, #, #, 1] & @ ({#Lat, #Lon} & /@ stns);
  moranI[stns_, capKm_:30] := Module[{distMat, wMat, n, pp, pbarL, num, den},
    distMat = distMatrix[stns]; n = Length[stns];
    wMat = Table[If[i == j || distMat[[i, j]] > capKm, 0., 1./distMat[[i, j]]], {i, n}, {j, n}];
    wMat = Table[With[{rs = Total[wMat[[i]]]}, If[rs > 0, wMat[[i]] / rs, ConstantArray[0., n]]], {i, n}];
    pp = #Price & /@ stns; pbarL = Mean[pp];
    num = Sum[wMat[[i, j]] (pp[[i]] - pbarL) (pp[[j]] - pbarL), {i, n}, {j, n}];
    den = Total[(pp - pbarL)^2]; num / den];
  variogram[stns_, hCenters_, hWidth_:1.0] := Module[{distMat, pp},
    distMat = distMatrix[stns]; pp = #Price & /@ stns;
    Table[With[{pairs = Position[distMat, x_ /; h - hWidth/2 <= x < h + hWidth/2, {2}]},
      If[Length[pairs] >= 4, Mean[(pp[[#[[1]]]] - pp[[#[[2]]]])^2 / 2 & /@ pairs], Missing[]]], {h, hCenters}]];
  deI = moranI[deStn]; ukI = moranI[ukStn];
  deBins = {1, 2, 5, 10, 20}; ukBins = {1, 2, 5, 10, 25, 50};
  deGam = variogram[deStn, deBins]; ukGam = variogram[ukStn, ukBins, 5.];
  sphericalModel[h_, c0_, c1_, a_] := Piecewise[{{c0 + c1 (1.5 (h/a) - 0.5 (h/a)^3), h < a}, {c0 + c1, h >= a}}, c0];
  fitSpherical[bins_, gam_] := Module[{data, fit},
    data = Select[Transpose[{bins, gam}], NumberQ[#[[2]]] &];
    If[Length[data] < 3, Return[$Failed]];
    Quiet @ Check[fit = NonlinearModelFit[data, {sphericalModel[h, c0, c1, a], c0 >= 0, c1 >= 0, a > 0},
      {{c0, 1*^-5}, {c1, 5*^-4}, {a, 10.}}, h]; {fit["BestFitParameters"], fit}, $Failed]];
  deFit = fitSpherical[deBins, deGam]; ukFit = fitSpherical[ukBins, ukGam];
  mkVarPlot[bins_, gam_, fit_, color_, name_] := Show[
    ListPlot[Transpose[{bins, gam}], PlotStyle -> Directive[color, PointSize[0.025]], Frame -> True, AspectRatio -> 0.7,
      FrameLabel -> {Style["lag h [km]", 14, Bold], Style["semivariance gamma(h) [(EUR/L)^2]", 13, Bold]},
      FrameTicksStyle -> Directive[FontSize -> 12], PlotLabel -> Style[name <> " variogram", 15, Bold, GrayLevel[0.15]],
      PlotTheme -> "Detailed", GridLines -> Automatic],
    If[fit =!= $Failed, Plot[fit[[2]]["BestFit"], {h, 0, Max[bins] * 1.2}, PlotStyle -> Directive[color, Thickness[0.005]]], Graphics[]],
    ImageSize -> 750];
  varPlot = Row[{mkVarPlot[deBins, deGam, deFit, RGBColor[0., 0.34, 0.62], "DE Paderborn"],
    mkVarPlot[ukBins, ukGam, ukFit, RGBColor[0.78, 0.07, 0.13], "UK Aberdeen"]}, Spacer[10]];
  priceMin = Min[#Price & /@ deStn]; priceMax = Max[#Price & /@ deStn];
  colorFn = ColorData[{"TemperatureMap", {priceMin, priceMax}}];
  mapDE = GeoGraphics[
    Map[{colorFn[#Price], PointSize[0.028], Tooltip[Point @ GeoPosition[{#Lat, #Lon}], #Brand <> " " <> ToString[Round[#Price, 0.001]]]} &, deStn],
    GeoRange -> {{Min[#Lat & /@ deStn] - 0.05, Max[#Lat & /@ deStn] + 0.05}, {Min[#Lon & /@ deStn] - 0.05, Max[#Lon & /@ deStn] + 0.05}},
    GeoProjection -> "Mercator", ImageSize -> 850, GeoBackground -> "StreetMapNoLabels",
    PlotLabel -> Style[StringTemplate["DE Paderborn diesel, last 7 days  -  Moran's I = `i`"][Round[N @ deI, 0.001]], 15, Bold, GrayLevel[0.15]]];
  distMat2 = distMatrix[deStn]; n2 = Length[deStn];
  wMat2 = Table[If[i == j || distMat2[[i, j]] > 30, 0., 1./distMat2[[i, j]]], {i, n2}, {j, n2}];
  wMat2 = Table[With[{rs = Total[wMat2[[i]]]}, If[rs > 0, wMat2[[i]]/rs, 0 wMat2[[i]]]], {i, n2}];
  p = #Price & /@ deStn; pbar = Mean[p]; wp = wMat2 . (p - pbar);
  moranScatter = ListPlot[Transpose[{p - pbar, wp}],
    PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], PointSize[0.022]], Frame -> True, AspectRatio -> 1,
    FrameLabel -> {Style["price - mean", 14, Bold], Style["neighbour-weighted price - mean", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12], PlotLabel -> Style["Moran scatterplot (Paderborn)", 15, Bold, GrayLevel[0.15]],
    ImageSize -> 700, PlotTheme -> "Detailed", GridLines -> {{0}, {0}}, GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]]];
  <|"moran" -> Row[{mapDE, moranScatter}, Spacer[10]], "variogram" -> varPlot|>
];

(* ================================================================ *)
(* §10  Cross-border station density                               *)
(* ================================================================ *)
CrossBorder[] := Module[
  {file, snap, neighbours, neighborGeoms, hav, extractCoords, borderPoints, nearestBorderKm,
   sample, sampleWithD, binEdges, binnedCount, densityPlot, maxD, colorFn, mapPlot},
  file = "data/tankerkoenig_snapshot.csv";
  If[!FileExistsQ[file],
    Return @ Style[
      "CrossBorder[] needs " <> file <> " (the country-wide station " <>
      "coordinate snapshot from wolfram/fetch_tankerkoenig.wls). " <>
      "Generate it first, then re-run.", Italic, Red]];
  snap = Normal @ Import[file, "Dataset", "HeaderLines" -> 1];
  snap = Select[snap, NumberQ[#lat] && NumberQ[#lng] &];
  neighbours = {"Netherlands", "Belgium", "Luxembourg", "France", "Switzerland", "Austria", "CzechRepublic", "Poland", "Denmark"};
  neighborGeoms = Quiet @ Map[Function[c, CountryData[c, "Polygon"]], neighbours];
  hav[{lat1_, lon1_}, {lat2_, lon2_}] := Module[{dLat = (lat2 - lat1) Pi/180, dLon = (lon2 - lon1) Pi/180, l1 = lat1 Pi/180, l2 = lat2 Pi/180},
    6371 * 2 * ArcSin[Sqrt[Sin[dLat/2]^2 + Cos[l1] Cos[l2] Sin[dLon/2]^2]]];
  extractCoords[poly_] := Partition[Cases[poly, _Real | _Integer, Infinity], 2];
  borderPoints = Catenate[extractCoords /@ neighborGeoms];
  borderPoints = Select[borderPoints, Length[#] == 2 && 46 < #[[1]] < 56 && 4 < #[[2]] < 17 &];
  If[Length[borderPoints] > 5000, borderPoints = RandomSample[borderPoints, 5000]];
  nearestBorderKm[{stnLat_, stnLon_}] := Min @ Map[hav[{stnLat, stnLon}, {#[[1]], #[[2]]}] &, borderPoints];
  sample = RandomSample[snap, UpTo[800]];
  sampleWithD = Map[Function[s, <|s, "BorderKm" -> nearestBorderKm[{s["lat"], s["lng"]}]|>], sample];
  binEdges = {0, 5, 10, 20, 50, 100, 200, 500, 1000};
  binnedCount = Table[Length @ Select[sampleWithD, binEdges[[i]] <= #BorderKm < binEdges[[i + 1]] &], {i, 1, Length[binEdges] - 1}];
  densityPlot = BarChart[binnedCount,
    ChartLabels -> Placed[Style[#, 12, Bold] & /@ (StringTemplate["``-``"] @@@ Partition[binEdges, 2, 1]), Below],
    Frame -> True, FrameLabel -> {Style["distance to nearest foreign border [km]", 14, Bold], Style["stations in 800-sample", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style["German station count vs distance to nearest foreign border", 16, Bold, GrayLevel[0.15]],
    ChartStyle -> RGBColor[0., 0.34, 0.62], ImageSize -> 1100, AspectRatio -> 1/2.4];
  maxD = Max[#BorderKm & /@ sampleWithD];
  colorFn = Function[d, ColorData["TemperatureMap"][Clip[d/maxD, {0, 1}]]];
  mapPlot = GeoGraphics[
    {FaceForm[Lighter[Gray, 0.92]], EdgeForm[GrayLevel[0.5]], CountryData["Germany", "Polygon"],
     Map[{colorFn[#BorderKm], PointSize[0.008], Point @ GeoPosition[{#lat, #lng}]} &, sampleWithD]},
    GeoRange -> Entity["Country", "Germany"], GeoProjection -> "Mercator", GeoBackground -> White,
    PlotLabel -> Style["Station coordinates colored by distance to nearest foreign border", 16, Bold, GrayLevel[0.15]], ImageSize -> 1100];
  Column[{mapPlot, densityPlot}, Spacings -> 0.3]
];

(* ================================================================ *)
(* §11  Rockets and feathers: asymmetric Brent pass-through         *)
(* ================================================================ *)
BrentLag[kLag_:21] := Module[
  {de, deDiesel, deWithLocal, dailyDiesel, brent, brentRows, toISO, brentByDate, merged,
   diff, pos, neg, prices, brentValues, dP, dB, n, usable, X, y, XtX, Xty, beta, yhat,
   resid, ssr, sigma2, covBeta, ses, betaPos, sePos, betaNeg, seNeg, cumPos, cumNeg,
   posCol, negCol, irfPlot, cumPlot},
  de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
  deDiesel = Select[de, #Fuel == "Diesel" &];
  deWithLocal = Function[r, Append[r, "LocalDate" -> PetrolData`LocalDate[r]]] /@ deDiesel;
  dailyDiesel = KeyValueMap[Function[{d, rows}, <|"Date" -> DateString[DateObject[d], "ISODate"], "Diesel" -> Mean[#Price & /@ rows]|>],
    GroupBy[deWithLocal, #LocalDate &]];
  dailyDiesel = SortBy[dailyDiesel, #Date &];
  brentRows = Normal @ Import["data/brent_daily.csv", "Dataset", "HeaderLines" -> 1];
  toISO[d_DateObject] := DateString[d, "ISODate"]; toISO[s_String] := s;
  brentByDate = Association[(toISO @ #Date) -> #BrentEURPerL & /@ brentRows];
  merged = SortBy[Map[Function[r, With[{b = Lookup[brentByDate, r["Date"], Missing[]]},
    If[NumberQ[b], <|r, "Brent" -> b|>, Nothing]]], dailyDiesel], #Date &];
  diff[xs_] := Differences[xs]; pos[x_] := Max[x, 0.]; neg[x_] := Min[x, 0.];
  prices = #Diesel & /@ merged; brentValues = #Brent & /@ merged;
  dP = diff[prices]; dB = diff[brentValues]; n = Length[dB];
  usable = Range[kLag + 1, n];
  X = Map[Function[t, Flatten[{1., Table[pos[dB[[t - k]]], {k, 0, kLag}], Table[neg[dB[[t - k]]], {k, 0, kLag}]}]], usable];
  y = dP[[usable]];
  XtX = Transpose[X] . X; Xty = Transpose[X] . y; beta = LinearSolve[XtX, Xty];
  yhat = X . beta; resid = y - yhat; ssr = resid . resid;
  sigma2 = ssr / (Length[y] - Length[beta]); covBeta = sigma2 * Inverse[XtX]; ses = Sqrt @ Diagonal[covBeta];
  betaPos = beta[[2 ;; kLag + 2]]; sePos = ses[[2 ;; kLag + 2]];
  betaNeg = beta[[kLag + 3 ;; 2 kLag + 3]]; seNeg = ses[[kLag + 3 ;; 2 kLag + 3]];
  cumPos = Accumulate[betaPos]; cumNeg = Accumulate[betaNeg];
  posCol = RGBColor[0.85, 0.27, 0.27]; negCol = RGBColor[0.20, 0.60, 0.86];
  irfPlot = ListPlot[{Transpose[{Range[0, kLag], betaPos}], Transpose[{Range[0, kLag], betaNeg}]},
    PlotStyle -> {Directive[posCol, PointSize[0.014]], Directive[negCol, PointSize[0.014]]}, Joined -> True, Frame -> True,
    FrameLabel -> {Style["lag h [days]", 14, Bold], Style["beta_h (EUR/L Delta-diesel per EUR/L Delta-brent)", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style["Asymmetric impulse responses: diesel reaction to Brent shocks", 16, Bold, GrayLevel[0.15]],
    GridLines -> {None, {0}}, GridLinesStyle -> Directive[GrayLevel[0.5], Dashed],
    PlotLegends -> Placed[LineLegend[{posCol, negCol}, {Style["positive Brent shocks (beta+)", 13], Style["negative Brent shocks (beta-)", 13]}], {0.80, 0.85}],
    ImageSize -> 1100, AspectRatio -> 1/2.4, PlotTheme -> "Detailed"];
  cumPlot = ListPlot[{Transpose[{Range[1, kLag + 1], cumPos}], Transpose[{Range[1, kLag + 1], -cumNeg}]},
    PlotStyle -> {Directive[posCol, Thickness[0.006]], Directive[negCol, Thickness[0.006]]}, Joined -> True, Frame -> True,
    FrameLabel -> {Style["horizon h [days]", 14, Bold], Style["cumulative pass-through", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style["Cumulative pass-through: rockets (up) vs feathers (down, sign-flipped)", 16, Bold, GrayLevel[0.15]],
    GridLines -> {None, {0, 0.5, 1.0}},
    PlotLegends -> Placed[LineLegend[{posCol, negCol}, {Style["Sum beta+ (rockets)", 13], Style["-Sum beta- (feathers, flipped)", 13]}], {0.80, 0.85}],
    ImageSize -> 1100, AspectRatio -> 1/2.4, PlotTheme -> "Detailed"];
  Column[{irfPlot, cumPlot}, Spacings -> 0.5]
];

(* ================================================================ *)
(* §12  Hotelling's rule (honest negative)                          *)
(* ================================================================ *)
HotellingRule[] := Module[
  {eiaURL, xls, sheet, rawData, annualBrent, cpiHardCoded, cpi, cpi2025, realBrent,
   realRate, y0, p0, hotellingPath, events, realBrentByYear, annotEpilog},
  eiaURL = "https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls";
  xls = Import[eiaURL, "XLS"]; sheet = xls[[2]];
  rawData = Select[sheet[[4 ;;]], MatchQ[#, {_DateObject, _?NumberQ}] &];
  annualBrent = SortBy[KeyValueMap[Function[{y, rs}, {y, Mean[#[[2]] & /@ rs]}], GroupBy[rawData, DateValue[#[[1]], "Year"] &]], First];
  cpiHardCoded = <|1987 -> 113.6, 1988 -> 118.3, 1989 -> 124.0, 1990 -> 130.7, 1991 -> 136.2, 1992 -> 140.3, 1993 -> 144.5,
    1994 -> 148.2, 1995 -> 152.4, 1996 -> 156.9, 1997 -> 160.5, 1998 -> 163.0, 1999 -> 166.6, 2000 -> 172.2, 2001 -> 177.1,
    2002 -> 179.9, 2003 -> 184.0, 2004 -> 188.9, 2005 -> 195.3, 2006 -> 201.6, 2007 -> 207.342, 2008 -> 215.303, 2009 -> 214.537,
    2010 -> 218.056, 2011 -> 224.939, 2012 -> 229.594, 2013 -> 232.957, 2014 -> 236.736, 2015 -> 237.017, 2016 -> 240.007,
    2017 -> 245.120, 2018 -> 251.107, 2019 -> 255.657, 2020 -> 258.811, 2021 -> 270.970, 2022 -> 292.655, 2023 -> 304.702,
    2024 -> 313.700, 2025 -> 320.500, 2026 -> 327.000|>;
  cpi[y_] := Lookup[cpiHardCoded, y, Last @ Values @ cpiHardCoded]; cpi2025 = cpi[2025];
  realBrent = Map[Function[pair, {pair[[1]], pair[[2]] * cpi2025 / cpi[pair[[1]]]}], annualBrent];
  realRate = 0.02; y0 = realBrent[[1, 1]]; p0 = realBrent[[1, 2]];
  hotellingPath = Map[Function[pair, {pair[[1]], p0 * Exp[realRate * (pair[[1]] - y0)]}], annualBrent];
  events = {{1990, "Iraq invades Kuwait"}, {2000, "tech bust + post-Asia"}, {2008, "GFC peak"}, {2014, "shale glut"},
    {2020, "COVID demand collapse"}, {2022, "Russia war"}, {2026, "Iran war"}};
  realBrentByYear = Association @@ ({#[[1]] -> #[[2]]} & /@ realBrent);
  annotEpilog = Map[Function[ev, Module[{y = ev[[1]], label = ev[[2]], v}, v = Lookup[realBrentByYear, y, 1.];
    {Black, PointSize[0.010], Point[{y, v}], Inset[Style[label, 11, Bold, GrayLevel[0.25]], {y, v * 1.14}, {-1, 0}]}]], events];
  ListLogPlot[{realBrent, hotellingPath}, Joined -> True,
    PlotStyle -> {Directive[RGBColor[0.85, 0.27, 0.27], Thickness[0.006]], Directive[GrayLevel[0.3], Thickness[0.005], Dashed]},
    Frame -> True, FrameLabel -> {Style["year", 14, Bold], Style["real Brent (2025 USD/bbl, log scale)", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLegends -> Placed[LineLegend[
      {Directive[RGBColor[0.85, 0.27, 0.27], Thickness[0.006]], Directive[GrayLevel[0.3], Thickness[0.005], Dashed]},
      {Style["observed Brent (real)", 13], Style["Hotelling path: 2 %/yr real growth", 13]}], {0.20, 0.86}],
    PlotLabel -> Style["Long-run Brent vs Hotelling's rule (1987-2026)", 18, Bold, GrayLevel[0.1]],
    GridLines -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.88], Thickness[0.0005]],
    Epilog -> annotEpilog, ImageSize -> 1300, AspectRatio -> 1/2.2]
];

End[];
EndPackage[];
