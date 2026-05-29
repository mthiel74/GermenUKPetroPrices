#!/usr/bin/env wolframscript
(* intraday_cycle.wls — Analysis §1 (+ pre/post comparison)
   The intraday cycle in Paderborn DE prices, before and after the
   12-Uhr-Regel (effective 2026-04-01).

   For each (station × calendar-day × fuel) we subtract the daily mean
   to remove the long-run Brent trend; the residual is then aggregated
   over the chosen window by local hour-of-day on working days.

   Outputs:
     docs/images/intraday_cycle_prepost_1d.png   — workday hour slice
     docs/images/intraday_cycle_pre.png          — weekday × hour heatmap, pre
     docs/images/intraday_cycle_post.png         — weekday × hour heatmap, post
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[intraday] loading DE rows ..."];
deAll = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
Print["[intraday]   ", Length[deAll], " rows"];

(* Attach derived local-time columns once *)
attachLocal[r_] := Append[r, {
  "LocalHour"    -> PetrolData`LocalHour[r],
  "LocalDate"    -> PetrolData`LocalDate[r],
  "LocalWeekday" -> PetrolData`LocalWeekday[r],
  "AfterRule"    -> PetrolData`IsAfterRule[r]}];

withLocal = attachLocal /@ deAll;

(* Daily means per (station, fuel, day) *)
dailyMeans = Mean[#Price & /@ #] & /@
  GroupBy[withLocal, {#StationID, #Fuel, #LocalDate} &];

residuals = Function[r,
  Append[r, "Residual" ->
   r["Price"] - dailyMeans[{r["StationID"], r["Fuel"], r["LocalDate"]}]]
] /@ withLocal;

(* Aggregate by (Fuel, AfterRule, LocalWeekday, LocalHour) → median *)
aggMed = Median[#Residual & /@ #] & /@
  GroupBy[residuals, {#Fuel, #AfterRule, #LocalWeekday, #LocalHour} &];

(* Workday slice: average over Mon–Fri *)
workdayProfile[fuel_, afterRule_] := Table[
  Mean @ DeleteMissing @
    Table[Lookup[aggMed, Key[{fuel, afterRule, wd, hr}], Missing[]], {wd, 1, 5}],
  {hr, 0, 23}];

fuels  = {"E5", "E10", "Diesel"};
colors = {RGBColor[0.85, 0.27, 0.27],
          RGBColor[0.20, 0.60, 0.86],
          RGBColor[0.30, 0.69, 0.32]};

mk[fuel_, color_] := {
  ListLinePlot[
    {Transpose[{Range[0, 23], workdayProfile[fuel, False]}],
     Transpose[{Range[0, 23], workdayProfile[fuel, True]}]},
    PlotStyle -> {{color, Thickness[0.008], Dashing[None]},
                  {color, Thickness[0.008], Dashing[{0.025, 0.015}]}},
    PlotLegends -> Placed[
      LineLegend[
        {Directive[color, Thickness[0.008]],
         Directive[color, Thickness[0.008], Dashing[{0.025, 0.015}]]},
        {Style["pre 12-Uhr-Regel", 13],
         Style["post 12-Uhr-Regel", 13]}],
      {0.78, 0.85}],
    Frame -> True,
    FrameLabel -> {Style["local hour (Europe/Berlin)", 14, Bold],
                   Style["median (price - station-day mean) [EUR/L]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[
      fuel <> " intraday cycle, Paderborn 10 km, workdays only",
      16, Bold, GrayLevel[0.15]],
    GridLines -> {Range[0, 23, 1], Automatic},
    GridLinesStyle -> Directive[GrayLevel[0.85], Dotted],
    Epilog -> {GrayLevel[0.4], Dashed, Thickness[0.003],
               Line[{{12, -0.05}, {12, 0.13}}],
               Text[Style["12:00", 12, Italic, Bold, GrayLevel[0.4]],
                    {12.3, 0.115}, {-1, 0}]},
    ImageSize -> 1200, AspectRatio -> 1/2.5,
    PlotTheme -> "Detailed"
  ]};

plots = Flatten @ MapThread[mk, {fuels, colors}];
combined = Column[plots, Spacings -> {0, 0.5}];

Print["[intraday] writing prepost 1-D plot ..."];
Export["docs/images/intraday_cycle_prepost_1d.png", combined, ImageResolution -> 144];

(* ---- Heatmaps ---- *)

heatmapMatrix[fuel_, afterRule_] := Table[
  Lookup[aggMed, Key[{fuel, afterRule, wd, hr}], Missing[]],
  {wd, 1, 7}, {hr, 0, 23}];

divergent = ColorData[{"RedBlueTones", "Reverse"}];

makeHeatmap[fuel_, afterRule_, label_] := ArrayPlot[
  heatmapMatrix[fuel, afterRule] /. _Missing -> 0.,
  DataReversed -> True,
  ColorFunction -> (divergent[(#+0.10)/0.20] &),
  ColorFunctionScaling -> False,
  PlotLegends -> BarLegend[{(divergent[(#+0.10)/0.20] &), {-0.10, 0.10}},
                            LegendLabel -> "EUR/L"],
  FrameTicks -> {
    {Transpose @ {Range[7],
       Reverse @ {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}}, None},
    {Transpose @ {Range[24], Range[0, 23]}, None}
  },
  FrameLabel -> {"local hour", ""},
  PlotLabel -> fuel <> " - " <> label,
  ImageSize -> 1000, AspectRatio -> 1/3.2,
  Epilog -> {Black, Thickness[0.003],
             Line[{{12.5, -0.5}, {12.5, 7.5}}]}
];

Print["[intraday] writing E5 heatmaps ..."];
Export["docs/images/intraday_cycle_pre.png",
  makeHeatmap["E5", False, "pre 12-Uhr-Regel (Sep 2025 - Mar 2026)"],
  ImageResolution -> 144];
Export["docs/images/intraday_cycle_post.png",
  makeHeatmap["E5", True, "post 12-Uhr-Regel (Apr 2026 - May 2026)"],
  ImageResolution -> 144];

Print["[intraday] done."];
