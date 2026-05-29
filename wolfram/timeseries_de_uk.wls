#!/usr/bin/env wolframscript
(* timeseries_de_uk.wls — high-frequency comparison
   Show the *raw* time series of a representative DE station alongside
   a representative UK station. Hypothesis: DE has daily oscillation
   (sawtooth), UK has essentially step-flat behaviour between rare
   updates. After the 12-Uhr-Regel (2026-04-01) the DE oscillation
   shifts but does not disappear.

   Three panels in one figure:
     (a) DE one-station raw diesel, full 9-month window, with rule
         vertical line.
     (b) DE same station, zoomed to two weeks straddling 2026-04-01.
     (c) UK one-station raw diesel, same two-week window.

   Outputs:
     docs/images/timeseries_de_uk.png
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[ts] loading data ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];

(* Pick a representative DE station: Aral Paderborn (most observations) *)
deCounts = Counts[#StationID & /@ Select[de, #Fuel == "Diesel" &]];
deRepID = First @ Keys @ ReverseSort @ deCounts;
deRep = Select[de, #StationID == deRepID && #Fuel == "Diesel" &];
deName = First[deRep]["Brand"] <> " " <> First[deRep]["City"];
Print["[ts] DE rep: ", deName, " (", Length[deRep], " readings)"];

ukCounts = Counts[#StationID & /@ Select[uk, #Fuel == "Diesel" &]];
ukRepID = First @ Keys @ ReverseSort @ ukCounts;
ukRep = Select[uk, #StationID == ukRepID && #Fuel == "Diesel" &];
ukName = First[ukRep]["Brand"] <> " " <> First[ukRep]["Postcode"];
Print["[ts] UK rep: ", ukName, " (", Length[ukRep], " readings)"];

dePts = SortBy[{#TimestampUTC, #Price} & /@ deRep, AbsoluteTime[#[[1]]] &];
ukPts = SortBy[{#TimestampUTC, #Price} & /@ ukRep, AbsoluteTime[#[[1]]] &];

ruleStart = DateObject[{2026, 4, 1}, "Day", TimeZone -> "Europe/Berlin"];

(* (a) Full DE series — with major events annotated *)
brentPeakDate = DateObject[{2026, 4, 15}, "Day"];
russiaWarDate = DateObject[{2022, 3, 1}, "Day"];  (* outside range *)
covidDate     = DateObject[{2020, 4, 20}, "Day"]; (* outside range *)

deFull = DateListPlot[dePts,
  Joined -> True,
  PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], Thickness[0.0025]],
  Frame -> True, AspectRatio -> 1/3.5,
  FrameLabel -> {Style["date", 14, Bold],
                 Style["diesel [EUR/L]", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    deName <> " - 9-month diesel time series  -  Sep 2025 to May 2026",
    16, Bold, GrayLevel[0.1]],
  GridLines -> {{ruleStart, brentPeakDate}, Automatic},
  GridLinesStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Dashed,
                              Thickness[0.0025]],
  Epilog -> {
    Text[Style["12-Uhr-Regel\n1 Apr 2026", 11, Italic, Bold,
               RGBColor[0.78, 0.07, 0.13]],
         Scaled[{0.72, 0.93}], {0, 0}],
    Text[Style["Brent peak\n$114/bbl", 10, Italic, Bold,
               RGBColor[0.78, 0.07, 0.13]],
         Scaled[{0.83, 0.93}], {0, 0}]},
  ImageSize -> 1300, PlotTheme -> "Detailed"];

(* (b) DE zoom: 2 weeks straddling rule *)
zoomStart = DateObject[{2026, 3, 25}, "Day"];
zoomEnd   = DateObject[{2026, 4, 8}, "Day"];
inWin = Select[dePts,
  AbsoluteTime[#[[1]]] >= AbsoluteTime[zoomStart] &&
  AbsoluteTime[#[[1]]] <= AbsoluteTime[zoomEnd] &];

deZoom = DateListPlot[inWin,
  Joined -> True, PlotStyle -> Directive[RGBColor[0., 0.34, 0.62], Thickness[0.004]],
  Frame -> True, AspectRatio -> 1/3,
  FrameLabel -> {Style["local date", 14, Bold],
                 Style["diesel [EUR/L]", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    deName <> " - two weeks around the 12-Uhr-Regel (zoom)",
    16, Bold, GrayLevel[0.1]],
  GridLines -> {{ruleStart}, Automatic},
  GridLinesStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Dashed,
                              Thickness[0.003]],
  ImageSize -> 1300, PlotTheme -> "Detailed"];

(* (c) UK same window *)
ukWin = Select[ukPts,
  AbsoluteTime[#[[1]]] >= AbsoluteTime[zoomStart] &&
  AbsoluteTime[#[[1]]] <= AbsoluteTime[zoomEnd] &];

ukZoom = DateListPlot[ukWin,
  Joined -> True, PlotStyle -> Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.004]],
  Frame -> True, AspectRatio -> 1/3,
  FrameLabel -> {Style["local date", 14, Bold],
                 Style["diesel [EUR/L]", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    ukName <> " - same two weeks (UK control market, no rule)",
    16, Bold, GrayLevel[0.1]],
  ImageSize -> 1300, PlotTheme -> "Detailed"];

Export["docs/images/timeseries_de_uk.png",
  Column[{deFull, deZoom, ukZoom}, Spacings -> 0.3],
  ImageResolution -> 144];

Print["[ts] done."];
