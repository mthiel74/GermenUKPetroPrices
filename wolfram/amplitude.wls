#!/usr/bin/env wolframscript
(* amplitude.wls — Analysis §3: daily price-amplitude shift.

   For each (station, day) compute A = max - min over all readings on
   that day (diesel only). Compare distribution DE pre vs post
   2026-04-01 with Mann-Whitney U + Cliff's δ + bootstrap CI.

   Outputs:
     docs/images/amplitude_prepost.png
     data/amplitude_stats.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[amp] loading DE rows ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];
Print["[amp] DE diesel rows: ", Length[deDiesel]];

withLocal = Function[r,
  Append[r, {
    "LocalDate"  -> PetrolData`LocalDate[r],
    "AfterRule"  -> PetrolData`IsAfterRule[r]}]] /@ deDiesel;

(* Per-(station, day) amplitude *)
ampByStnDay = With[{groups = GroupBy[withLocal, {#StationID, #LocalDate} &]},
  KeyValueMap[
    Function[{k, rows},
      <|"StationID" -> k[[1]], "LocalDate" -> k[[2]],
        "AfterRule" -> First[rows]["AfterRule"],
        "Amplitude" -> Max[#Price & /@ rows] - Min[#Price & /@ rows]
      |>],
    groups]];

Print["[amp] station-day pairs: ", Length[ampByStnDay]];

prePost = GroupBy[ampByStnDay, #AfterRule &];
preA = #Amplitude & /@ prePost[False];
postA = #Amplitude & /@ prePost[True];

Print["[amp] n pre = ", Length[preA], "; n post = ", Length[postA]];
Print["[amp] median pre  = ", Median[preA] // N];
Print["[amp] median post = ", Median[postA] // N];

(* Mann-Whitney U via MannWhitneyTest (returns p-value) *)
pVal = MannWhitneyTest[{preA, postA}];
Print["[amp] Mann-Whitney p = ", pVal];

(* Cliff's δ = (#pre>post - #pre<post) / (n_pre * n_post) *)
nPre = Length[preA]; nPost = Length[postA];
deltaPairs = Total @ Flatten @ Table[Sign[a - b], {a, preA}, {b, postA}];
cliffsDelta = deltaPairs / N[nPre * nPost];
Print["[amp] Cliff's delta (pre vs post) = ", cliffsDelta];

(* Bootstrap CI on median shift *)
SeedRandom[42];
nBoot = 500;
shifts = Table[
  Median @ RandomChoice[preA, nPre] - Median @ RandomChoice[postA, nPost],
  {nBoot}];
ci95 = Quantile[shifts, {0.025, 0.5, 0.975}];
Print["[amp] bootstrap 95% CI on median shift: ", ci95];

(* Plots *)
preColor = RGBColor[0.18, 0.50, 0.62];
postColor = RGBColor[0.91, 0.45, 0.10];

violinPlot = SmoothHistogram[{preA, postA},
  PlotStyle -> {Directive[preColor, Thickness[0.005]],
                Directive[postColor, Thickness[0.005]]},
  Filling -> Axis,
  FillingStyle -> {Directive[preColor, Opacity[0.25]],
                   Directive[postColor, Opacity[0.25]]},
  PlotLegends -> Placed[
    LineLegend[{preColor, postColor},
      {Style["pre 12-Uhr-Regel", 13], Style["post 12-Uhr-Regel", 13]}],
    {0.80, 0.85}],
  Frame -> True,
  FrameLabel -> {Style["daily amplitude (max - min) [EUR/L]", 14, Bold],
                 Style["density", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    "DE diesel: daily price amplitude, before vs after 12-Uhr-Regel",
    16, Bold, GrayLevel[0.15]],
  PlotRange -> {{0, 0.4}, All},
  ImageSize -> 1100, AspectRatio -> 1/2.4,
  PlotTheme -> "Detailed"];

stripPlot = ListPlot[
  {Transpose[{RandomReal[{0.9, 1.1}, nPre], preA}],
   Transpose[{RandomReal[{1.9, 2.1}, nPost], postA}]},
  PlotStyle -> {
    Directive[Opacity[0.30], PointSize[0.005], preColor],
    Directive[Opacity[0.30], PointSize[0.005], postColor]},
  Frame -> True,
  FrameTicks -> {{Automatic, None},
                 {{{1, Style["pre 12-Uhr-Regel", 13, Bold]},
                   {2, Style["post 12-Uhr-Regel", 13, Bold]}}, None}},
  FrameLabel -> {None,
                 Style["daily amplitude [EUR/L]", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    StringTemplate["median shift `1` EUR/L (95% CI `2`-`3`)  -  Cliff's d = `4`  -  p < `5`"][
      Round[ci95[[2]], 0.001], Round[ci95[[1]], 0.001], Round[ci95[[3]], 0.001],
      Round[cliffsDelta, 0.01], Round[pVal, 0.0001]],
    14, Bold, GrayLevel[0.2]],
  PlotRange -> {{0.5, 2.5}, {0, 0.5}},
  ImageSize -> 1100, AspectRatio -> 1/2.4,
  PlotTheme -> "Detailed"];

Export["docs/images/amplitude_prepost.png",
  Column[{violinPlot, stripPlot}, Spacings -> 0.2],
  ImageResolution -> 144];

Export["data/amplitude_stats.csv",
  Dataset[{<|
    "n_pre" -> nPre, "n_post" -> nPost,
    "median_pre" -> N @ Median[preA], "median_post" -> N @ Median[postA],
    "median_shift" -> N @ ci95[[2]],
    "ci95_low" -> N @ ci95[[1]], "ci95_high" -> N @ ci95[[3]],
    "cliffs_delta" -> N @ cliffsDelta,
    "mann_whitney_p" -> N @ pVal
  |>}], "CSV"];

Print["[amp] done."];
