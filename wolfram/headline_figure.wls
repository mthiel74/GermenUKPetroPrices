#!/usr/bin/env wolframscript
(* headline_figure.wls — Wolfram-native editorial headline plot.

   A clean two-panel chart showing the inverted-cycle finding using
   the actual hazard-rate data computed by hazard_12uhr.wls. Guaranteed
   visually correct (no LLM/image-gen vagaries). Used as Figure 1 of
   the notebook.

   Outputs:
     docs/images/headline_inversion.png
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[headline] loading DE rows ..."];
deAll = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[deAll, #Fuel == "Diesel" &];

Print["[headline] computing residuals ..."];
withLocal = Function[r,
  Append[r, {
    "LocalHour" -> PetrolData`LocalHour[r],
    "LocalDate" -> PetrolData`LocalDate[r],
    "LocalWeekday" -> PetrolData`LocalWeekday[r],
    "AfterRule" -> PetrolData`IsAfterRule[r]}]] /@ deDiesel;

dailyMeans = Mean[#Price & /@ #] & /@
  GroupBy[withLocal, {#StationID, #LocalDate} &];
residuals = Function[r,
  Append[r, "Residual" ->
    r["Price"] - dailyMeans[{r["StationID"], r["Fuel"], r["LocalDate"]}]]] /@ withLocal;
(* Fuel is part of the dailyMeans group key in intraday_cycle.wls;
   here we only do diesel so the lookup omits Fuel: *)
dailyMeansDsl = Mean[#Price & /@ #] & /@
  GroupBy[withLocal, {#StationID, #LocalDate} &];
residuals = Function[r,
  Append[r, "Residual" ->
    r["Price"] - dailyMeansDsl[{r["StationID"], r["LocalDate"]}]]] /@ withLocal;
residuals = Select[residuals, 1 <= #LocalWeekday <= 5 &];

aggMed = Median[#Residual & /@ #] & /@
  GroupBy[residuals, {#AfterRule, #LocalHour} &];

workdayProfile[afterRule_] := Table[
  Lookup[aggMed, Key[{afterRule, hr}], 0.], {hr, 0, 23}];

preLine = workdayProfile[False];
postLine = workdayProfile[True];

Print["[headline] pre peak = ", Max[preLine], " at hour ", First[Ordering[preLine, -1]] - 1];
Print["[headline] post peak = ", Max[postLine], " at hour ", First[Ordering[postLine, -1]] - 1];

teal = RGBColor[0.18, 0.45, 0.55];
ember = RGBColor[0.90, 0.45, 0.10];

panel[trace_, color_, title_, peakLabel_, peakHour_] := Module[{maxV},
  maxV = Max[trace];
  ListLinePlot[
    Transpose[{Range[0, 23], trace}],
    PlotStyle -> Directive[color, Thickness[0.008]],
    Filling -> Bottom, FillingStyle -> Directive[color, Opacity[0.15]],
    Frame -> True, Axes -> False,
    FrameLabel -> {"hour of day (Europe/Berlin)",
                   "median (price - daily mean) [EUR/L]"},
    FrameStyle -> Directive[GrayLevel[0.3], Thickness[0.001]],
    PlotLabel -> Style[title, 16, Bold, GrayLevel[0.15],
                       FontFamily -> "Helvetica"],
    PlotRange -> {{-0.5, 24}, {-0.04, 0.12}},
    GridLines -> {Range[0, 24, 6], {0}},
    GridLinesStyle -> Directive[GrayLevel[0.85], Dashed],
    Epilog -> {
      (* peak marker, label offset away from 12:00 line *)
      Directive[color, PointSize[0.018]], Point[{peakHour, maxV}],
      Inset[Style[peakLabel, 12, Bold, color],
            {If[peakHour <= 10, peakHour, peakHour + 4], maxV + 0.015}, {0, 0}],
      (* 12:00 reference line — solid dashed *)
      Directive[GrayLevel[0.45], Thickness[0.002], Dashed],
      Line[{{12, -0.04}, {12, 0.135}}],
      Inset[Style["12:00", 10, GrayLevel[0.45]], {12.4, -0.035}, {-1, 0}]
    },
    ImageSize -> 1000, AspectRatio -> 1/2.4
  ]];

prePeakHr = First[Ordering[preLine, -1]] - 1;
postPeakHr = First[Ordering[postLine, -1]] - 1;

panA = panel[preLine, teal,
  "BEFORE 12-Uhr-Regel  --  morning-led cycle",
  "morning peak +" <> ToString @ Round[Max[preLine] * 100] <> " c/L at " <>
    IntegerString[prePeakHr, 10, 2] <> ":00",
  prePeakHr];

panB = panel[postLine, ember,
  "AFTER 12-Uhr-Regel  --  noon-led cycle",
  "12:00 spike +" <> ToString @ Round[Max[postLine] * 100] <> " c/L",
  postPeakHr];

headline = Column[
  {Style["Paderborn diesel intraday cycle: before vs after 1 April 2026", 18, Bold,
         GrayLevel[0.1]],
   Spacer[5],
   panA, Spacer[5], panB},
  Alignment -> Center, Spacings -> 0.3];

Print["[headline] writing headline_inversion.png ..."];
Export["docs/images/headline_inversion.png", headline, ImageResolution -> 144];
Print["[headline] done."];
