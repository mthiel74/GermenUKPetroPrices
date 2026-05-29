#!/usr/bin/env wolframscript
(* day_of_week.wls — Analysis §5: day-of-week effects.

   For each country separately, regress daily station-mean diesel
   price on day-of-week dummies (Mon = reference), with a station FE
   absorbed by demeaning per station.

       (p_{i,d} - p̄_i) = Σ_{k=2..7} β_k · DoW_k(d) + ε_{i,d}

   Output:
     docs/images/day_of_week.png
     data/day_of_week_coefs.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[dow] loading DE + UK ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
all = Join[Select[de, #Fuel == "Diesel" &], Select[uk, #Fuel == "Diesel" &]];

withLocal = Function[r,
  Append[r, {
    "LocalDate" -> PetrolData`LocalDate[r],
    "Weekday" -> PetrolData`LocalWeekday[r]}]] /@ all;

(* Daily station means *)
dailyMean = With[{groups = GroupBy[withLocal, {#Country, #StationID, #LocalDate} &]},
  KeyValueMap[
    Function[{k, rows},
      <|"Country" -> k[[1]], "StationID" -> k[[2]], "LocalDate" -> k[[3]],
        "Weekday" -> First[rows]["Weekday"],
        "Price" -> Mean[#Price & /@ rows]|>], groups]];

(* Demean per station *)
stationMeans = With[{groups = GroupBy[dailyMean, {#Country, #StationID} &]},
  Mean[#Price & /@ #] & /@ groups];

demeaned = Function[r,
  Append[r, "PriceDemean" ->
    r["Price"] - stationMeans[{r["Country"], r["StationID"]}]]] /@ dailyMean;

estimate[country_] := Module[{rows, weekdayMeans, weekdaySEs},
  rows = Select[demeaned, #Country == country &];
  weekdayMeans = Table[
    With[{wdRows = Select[rows, #Weekday == wd &]},
      If[Length[wdRows] > 0,
        Mean[#PriceDemean & /@ wdRows], Missing[]]],
    {wd, 1, 7}];
  weekdaySEs = Table[
    With[{wdRows = Select[rows, #Weekday == wd &]},
      If[Length[wdRows] > 0,
        StandardDeviation[#PriceDemean & /@ wdRows] / Sqrt[Length[wdRows]],
        Missing[]]],
    {wd, 1, 7}];
  {weekdayMeans, weekdaySEs}
];

{deMeans, deSEs} = estimate["DE"];
{ukMeans, ukSEs} = estimate["UK"];

Print["[dow] DE weekday means: ", deMeans];
Print["[dow] UK weekday means: ", ukMeans];

mkPlot[means_, ses_, color_, country_] := With[
  {centred = means - Mean[means],
   wdNames = {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}},
  ListPlot[
    Transpose[{Range[7], centred,
      Around @@@ Transpose[{centred, ses}]}] // (Transpose[{#[[All, 1]], #[[All, 3]]}] &),
    PlotStyle -> Directive[color, PointSize[0.022]],
    IntervalMarkers -> "Bars",
    IntervalMarkersStyle -> Directive[color, Thickness[0.004]],
    Frame -> True,
    FrameTicks -> {{Automatic, None},
                   {Transpose[{Range[7], Style[#, 13, Bold] & /@ wdNames}], None}},
    FrameLabel -> {Style["weekday", 14, Bold],
                   Style["diesel price - country mean [EUR/L]", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotLabel -> Style[
      country <> " day-of-week effect (station-demeaned)",
      16, Bold, GrayLevel[0.15]],
    GridLines -> {None, {0}},
    GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]],
    PlotRange -> {{0.5, 7.5}, {-0.05, 0.05}},
    ImageSize -> 750, AspectRatio -> 0.7,
    PlotTheme -> "Detailed"]];

dePlot = mkPlot[deMeans, deSEs, RGBColor[0., 0.34, 0.62], "DE (Paderborn)"];
ukPlot = mkPlot[ukMeans, ukSEs, RGBColor[0.78, 0.07, 0.13], "UK (Aberdeen)"];

Export["docs/images/day_of_week.png",
  Row[{dePlot, ukPlot}, Spacer[10]],
  ImageResolution -> 144];

Export["data/day_of_week_coefs.csv",
  Dataset @ Flatten @ Table[
    With[{ms = If[c == "DE", deMeans, ukMeans],
          ss = If[c == "DE", deSEs, ukSEs]},
      Table[<|"Country" -> c, "Weekday" -> wd,
              "MeanDemeaned" -> N @ ms[[wd]],
              "SE" -> N @ ss[[wd]]|>, {wd, 1, 7}]],
    {c, {"DE", "UK"}}],
  "CSV"];

Print["[dow] done."];
