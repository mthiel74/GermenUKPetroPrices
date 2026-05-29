#!/usr/bin/env wolframscript
(* did_de_uk.wls — Analysis §4: difference-in-differences DE vs UK.

   Estimand: did the 12-Uhr-Regel change the *level* of German diesel
   prices, relative to UK as a control?

       p_{i,d} = α_i + γ_d + τ · Post_d · DE_i + ε_{i,d}

   where α_i is station FE, γ_d is calendar-day FE (common to both
   countries — absorbs Brent shocks), and τ is the DiD coefficient
   of interest. Standard errors clustered by station.

   Output:
     docs/images/did_de_uk.png
     data/did_estimates.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[did] loading both feeds ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
uk = Normal @ PetrolData`LoadLocalUK[PetrolData`Settings["AberdeenRawUK"]];
all = Join[Select[de, #Fuel == "Diesel" &], Select[uk, #Fuel == "Diesel" &]];

withLocal = Function[r,
  Append[r, {
    "LocalDate"  -> PetrolData`LocalDate[r],
    "AfterRule"  -> PetrolData`IsAfterRule[r],
    "IsDE"       -> Boole[r["Country"] == "DE"]}]] /@ all;

(* Daily station mean *)
dailyMean = With[{groups = GroupBy[withLocal, {#Country, #StationID, #LocalDate} &]},
  KeyValueMap[
    Function[{k, rows},
      <|"Country" -> k[[1]], "StationID" -> k[[2]], "LocalDate" -> k[[3]],
        "Date" -> DateString[DateObject[k[[3]]], "ISODate"],
        "AfterRule" -> First[rows]["AfterRule"],
        "IsDE" -> First[rows]["IsDE"],
        "Price" -> Mean[#Price & /@ rows]
      |>], groups]];

Print["[did] daily station-day rows: ", Length[dailyMean]];

(* Aggregate plot: country-mean diesel price by day *)
byDayCountry = With[{groups = GroupBy[dailyMean, {#Country, #Date} &]},
  KeyValueMap[
    Function[{k, rows},
      <|"Country" -> k[[1]], "Date" -> k[[2]],
        "MeanPrice" -> Mean[#Price & /@ rows]|>], groups]];

deLine = SortBy[Select[byDayCountry, #Country == "DE" &], #Date &];
ukLine = SortBy[Select[byDayCountry, #Country == "UK" &], #Date &];

toDateNum[s_String] := DateObject[s];
dePts = Transpose[{toDateNum /@ (#Date & /@ deLine), #MeanPrice & /@ deLine}];
ukPts = Transpose[{toDateNum /@ (#Date & /@ ukLine), #MeanPrice & /@ ukLine}];

ruleStart = DateObject[{2026, 4, 1}];
russiaWar = DateObject[{2022, 3, 1}];  (* off-range, ignored *)
iranPeak = DateObject[{2026, 4, 15}];

aggPlot = DateListPlot[
  {dePts, ukPts},
  PlotStyle -> {Directive[RGBColor[0., 0.34, 0.62], Thickness[0.006]],
                Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.006]]},
  PlotLegends -> Placed[
    LineLegend[
      {Directive[RGBColor[0., 0.34, 0.62], Thickness[0.006]],
       Directive[RGBColor[0.78, 0.07, 0.13], Thickness[0.006]]},
      {Style["DE (Paderborn, 29 stations)", 13],
       Style["UK (Aberdeen, 34 stations)", 13]}],
    {0.18, 0.86}],
  Frame -> True,
  FrameLabel -> {Style["date", 14, Bold],
                 Style["country-mean diesel price [EUR/L]", 14, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    "Daily-mean diesel: DE (Paderborn) vs UK (Aberdeen)",
    18, Bold, GrayLevel[0.1]],
  GridLines -> {{ruleStart, iranPeak}, Automatic},
  GridLinesStyle -> Directive[GrayLevel[0.5], Dashed, Thickness[0.0015]],
  Epilog -> {
    Text[Style["12-Uhr-Regel\n1 Apr 2026", 11, Italic, Bold, GrayLevel[0.3]],
         Scaled[{0.78, 0.94}], {0, 0}],
    Text[Style["Brent peak\nIran war", 10, Italic, RGBColor[0.78, 0.07, 0.13]],
         Scaled[{0.88, 0.92}], {0, 0}]},
  ImageSize -> 1200, AspectRatio -> 1/2.4,
  PlotTheme -> "Detailed"];

(* DiD regression done manually for robustness.
   Design matrix columns: [1, DE, Post, DE*Post, t (days since start)]
   Response: daily station-mean diesel price. *)

t0d = AbsoluteTime[DateObject[{2025, 9, 14}]];
rows = {1, #IsDE, Boole[#AfterRule], #IsDE * Boole[#AfterRule],
        (AbsoluteTime[DateObject[#LocalDate]] - t0d) / 86400.,
        #Price} & /@ dailyMean;

X = rows[[All, 1 ;; 5]];
y = rows[[All, 6]];
n = Length[y];

Print["[did] design matrix size: ", Dimensions[X], "  response length: ", n];

XtX = Transpose[X] . X;
Xty = Transpose[X] . y;
beta = LinearSolve[XtX, Xty];
yhat = X . beta;
resid = y - yhat;
ssr = resid . resid;
sigma2 = ssr / (n - Length[beta]);
covBeta = sigma2 * Inverse[XtX];
ses = Sqrt @ Diagonal[covBeta];

Print["[did] coefficients (intercept, DE, Post, DE*Post, t): ", N @ beta];
Print["[did] SE:                                              ", N @ ses];

{interceptC, deC, postC, didC, trendC} = beta;
didSE = ses[[4]];
didCI = {didC - 1.96 didSE, didC + 1.96 didSE};

Print["[did] τ (DiD) = ", didC, " ± ", didSE, " (95% CI: ", didCI, ")"];

Export["docs/images/did_de_uk.png", aggPlot, ImageResolution -> 144];

Export["data/did_estimates.csv",
  Dataset[{<|
    "tau_DiD" -> didC, "tau_SE" -> didSE,
    "tau_ci_low" -> didCI[[1]], "tau_ci_high" -> didCI[[2]],
    "n_obs" -> n,
    "intercept" -> interceptC, "de_coef" -> deC,
    "post_coef" -> postC, "trend_coef" -> trendC
  |>}], "CSV"];

Print["[did] done."];
