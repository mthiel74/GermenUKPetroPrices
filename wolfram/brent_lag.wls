#!/usr/bin/env wolframscript
(* brent_lag.wls — Analysis §8: rockets and feathers.

   Test the canonical asymmetric distributed-lag relationship between
   retail diesel and Brent crude:

       Δp_t = α + Σ_{k=0..K} β⁺_k · max(Δb_{t-k}, 0)
                + Σ_{k=0..K} β⁻_k · min(Δb_{t-k}, 0) + ε_t

   where p_t = national-mean DE diesel (Paderborn cluster as proxy),
   b_t = Brent in EUR/L, both daily. Asymmetry test:
       Σ β⁺ > |Σ β⁻| ?

   Output:
     docs/images/brent_lag.png
     data/brent_lag_coefficients.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

KLag = 21;   (* days of lag *)

Print["[brentlag] loading data ..."];
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];
deWithLocal = Function[r, Append[r, "LocalDate" -> PetrolData`LocalDate[r]]] /@ deDiesel;

dailyDiesel = With[{groups = GroupBy[deWithLocal, #LocalDate &]},
  KeyValueMap[
    Function[{d, rows},
      <|"Date" -> DateString[DateObject[d], "ISODate"],
        "Diesel" -> Mean[#Price & /@ rows]|>],
    groups]];
dailyDiesel = SortBy[dailyDiesel, #Date &];
Print["[brentlag] diesel daily rows: ", Length[dailyDiesel]];

brent = Import["data/brent_daily.csv", "Dataset", "HeaderLines" -> 1];
brentRows = Normal[brent];
Print["[brentlag] brent rows: ", Length[brentRows]];

(* Normalise Date keys to ISO strings on both sides *)
toISO[d_DateObject] := DateString[d, "ISODate"];
toISO[s_String] := s;

brentByDate = Association[(toISO @ #Date) -> #BrentEURPerL & /@ brentRows];
merged = SortBy[
  Map[
    Function[r,
      With[{b = Lookup[brentByDate, r["Date"], Missing[]]},
        If[NumberQ[b],
          <|r, "Brent" -> b|>, Nothing]]],
    dailyDiesel],
  #Date &];

Print["[brentlag] merged rows: ", Length[merged]];

(* First-differences and asymmetric splits *)
diff[xs_] := Differences[xs];
pos[x_] := Max[x, 0.];
neg[x_] := Min[x, 0.];

prices = #Diesel & /@ merged;
brentValues = #Brent & /@ merged;

dP = diff[prices];
dB = diff[brentValues];

(* Drop the first index that aligns with t=1 (Δb_t in dB[[t-1]]) *)
n = Length[dB];

(* Build design matrix: at time t, regressors are
   [1, dB+_{t-0}, dB+_{t-1}, ..., dB+_{t-K}, dB-_{t-0}, ..., dB-_{t-K}].
   We need t >= K. Length of usable rows: n - K. *)
usable = Range[KLag + 1, n];

X = Map[
  Function[t,
    Flatten[{
      1.,
      Table[pos[dB[[t - k]]], {k, 0, KLag}],
      Table[neg[dB[[t - k]]], {k, 0, KLag}]
    }]],
  usable];

y = dP[[usable]];

Print["[brentlag] design: ", Dimensions[X], ", n = ", Length[y]];

XtX = Transpose[X] . X;
Xty = Transpose[X] . y;
beta = LinearSolve[XtX, Xty];
yhat = X . beta;
resid = y - yhat;
ssr = resid . resid;
sigma2 = ssr / (Length[y] - Length[beta]);
covBeta = sigma2 * Inverse[XtX];
ses = Sqrt @ Diagonal[covBeta];

(* β[[1]] = intercept; β[[2..K+2]] = β⁺, β[[K+3..2K+3]] = β⁻ *)
intercept = beta[[1]];
betaPos = beta[[2 ;; KLag + 2]];
sePos = ses[[2 ;; KLag + 2]];
betaNeg = beta[[KLag + 3 ;; 2 KLag + 3]];
seNeg = ses[[KLag + 3 ;; 2 KLag + 3]];

cumPos = Accumulate[betaPos];
cumNeg = Accumulate[betaNeg];

(* Cumulative pass-through: at horizon h, share of Brent shock passed through *)
Print["[brentlag] cum positive pass-through at h=1,5,21: ",
  N @ {cumPos[[2]], cumPos[[6]], cumPos[[-1]]}];
Print["[brentlag] cum negative pass-through at h=1,5,21: ",
  N @ {cumNeg[[2]], cumNeg[[6]], cumNeg[[-1]]}];
Print["[brentlag] asymmetry (cum⁺ - cum⁻) at h=21: ",
  N @ (cumPos[[-1]] + cumNeg[[-1]])];

(* Plot impulse responses *)
posPts = Transpose[{Range[0, KLag], betaPos, sePos}];
negPts = Transpose[{Range[0, KLag], betaNeg, seNeg}];

posCol = RGBColor[0.85, 0.27, 0.27];
negCol = RGBColor[0.20, 0.60, 0.86];

irfPlot = ListPlot[
  {Transpose[{Range[0, KLag], betaPos}],
   Transpose[{Range[0, KLag], betaNeg}]},
  PlotStyle -> {
    Directive[posCol, PointSize[0.014]],
    Directive[negCol, PointSize[0.014]]},
  Joined -> True,
  Frame -> True,
  FrameLabel -> {Style["lag h [days]", 14, Bold],
                 Style["beta_h (EUR/L Delta-diesel per EUR/L Delta-brent)", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    "Asymmetric impulse responses: diesel reaction to Brent shocks",
    16, Bold, GrayLevel[0.15]],
  GridLines -> {None, {0}}, GridLinesStyle -> Directive[GrayLevel[0.5], Dashed],
  PlotLegends -> Placed[
    LineLegend[{posCol, negCol},
      {Style["positive Brent shocks (beta+)", 13],
       Style["negative Brent shocks (beta-)", 13]}],
    {0.80, 0.85}],
  ImageSize -> 1100, AspectRatio -> 1/2.4,
  PlotTheme -> "Detailed"];

cumPlot = ListPlot[
  {Transpose[{Range[1, KLag + 1], cumPos}],
   Transpose[{Range[1, KLag + 1], -cumNeg}]},
  PlotStyle -> {
    Directive[posCol, Thickness[0.006]],
    Directive[negCol, Thickness[0.006]]},
  Joined -> True,
  Frame -> True,
  FrameLabel -> {Style["horizon h [days]", 14, Bold],
                 Style["cumulative pass-through", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLabel -> Style[
    "Cumulative pass-through: rockets (up) vs feathers (down, sign-flipped)",
    16, Bold, GrayLevel[0.15]],
  GridLines -> {None, {0, 0.5, 1.0}},
  PlotLegends -> Placed[
    LineLegend[{posCol, negCol},
      {Style["Sum beta+ (rockets)", 13],
       Style["-Sum beta- (feathers, flipped)", 13]}],
    {0.80, 0.85}],
  ImageSize -> 1100, AspectRatio -> 1/2.4,
  PlotTheme -> "Detailed"];

Export["docs/images/brent_lag.png",
  Column[{irfPlot, cumPlot}, Spacings -> 0.5],
  ImageResolution -> 144];

Export["data/brent_lag_coefficients.csv",
  Dataset @ Flatten[
    Table[
      {<|"h" -> h, "kind" -> "positive",
         "beta" -> N @ betaPos[[h + 1]], "SE" -> N @ sePos[[h + 1]],
         "cumulative" -> N @ cumPos[[h + 1]]|>,
       <|"h" -> h, "kind" -> "negative",
         "beta" -> N @ betaNeg[[h + 1]], "SE" -> N @ seNeg[[h + 1]],
         "cumulative" -> N @ cumNeg[[h + 1]]|>},
      {h, 0, KLag}]],
  "CSV"];

Print["[brentlag] done."];
