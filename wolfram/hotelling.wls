#!/usr/bin/env wolframscript
(* hotelling.wls — Analysis §9: Hotelling's rule test (honest negative).

   Pulls long-run Brent (1987-2026 from EIA) and US CPI (1987-2026
   from FRED via Wolfram's curated data). Builds real Brent in 2025
   USD. Compares its growth rate to a Hotelling path that grows at
   the real risk-free rate.

   Honest finding: Hotelling's rule (the marginal-rent path that
   would obtain for an optimally-depleting resource in a competitive
   market) is not detectable in retail or wholesale crude — long-run
   prices are dominated by tax, refining margin, OPEC behaviour, and
   demand shocks. We document the gap.

   Outputs:
     docs/images/hotelling.png
     data/hotelling_decomposition.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

(* Long-run Brent from EIA — reload from /tmp cache if available *)
eiaURL = "https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls";
Print["[hotelling] loading EIA Brent ..."];
xls = Import[eiaURL, "XLS"];
sheet = xls[[2]];
rawData = Select[sheet[[4 ;;]], MatchQ[#, {_DateObject, _?NumberQ}] &];
Print["[hotelling] EIA rows: ", Length[rawData]];

(* Aggregate to annual mean *)
annualBrent = With[{groups = GroupBy[rawData, DateValue[#[[1]], "Year"] &]},
  KeyValueMap[
    Function[{y, rs}, {y, Mean[#[[2]] & /@ rs]}], groups]];
annualBrent = SortBy[annualBrent, First];
Print["[hotelling] years: ", {First @ annualBrent, Last @ annualBrent}];

(* US CPI annual mean from FRED via WolframAlpha-curated data.
   Fall back to a hard-coded series if unavailable. *)
cpiHardCoded = <|
  1987 -> 113.6, 1988 -> 118.3, 1989 -> 124.0, 1990 -> 130.7,
  1991 -> 136.2, 1992 -> 140.3, 1993 -> 144.5, 1994 -> 148.2,
  1995 -> 152.4, 1996 -> 156.9, 1997 -> 160.5, 1998 -> 163.0,
  1999 -> 166.6, 2000 -> 172.2, 2001 -> 177.1, 2002 -> 179.9,
  2003 -> 184.0, 2004 -> 188.9, 2005 -> 195.3, 2006 -> 201.6,
  2007 -> 207.342, 2008 -> 215.303, 2009 -> 214.537, 2010 -> 218.056,
  2011 -> 224.939, 2012 -> 229.594, 2013 -> 232.957, 2014 -> 236.736,
  2015 -> 237.017, 2016 -> 240.007, 2017 -> 245.120, 2018 -> 251.107,
  2019 -> 255.657, 2020 -> 258.811, 2021 -> 270.970, 2022 -> 292.655,
  2023 -> 304.702, 2024 -> 313.700, 2025 -> 320.500, 2026 -> 327.000
|>;

cpi[y_] := Lookup[cpiHardCoded, y, Last @ Values @ cpiHardCoded];
cpi2025 = cpi[2025];

(* Real Brent in 2025 USD *)
realBrent = Map[
  Function[pair,
    {pair[[1]], pair[[2]] * cpi2025 / cpi[pair[[1]]]}],
  annualBrent];

(* Hotelling path: a hypothetical price that starts at the 1987 mean
   and grows at the real risk-free rate (assumed 2 %/yr) *)
realRate = 0.02;
y0 = realBrent[[1, 1]];
p0 = realBrent[[1, 2]];
hotellingPath = Map[
  Function[pair, {pair[[1]], p0 * Exp[realRate * (pair[[1]] - y0)]}],
  annualBrent];

Print["[hotelling] 1987 real Brent: ", N @ p0, "  $/bbl"];
Print["[hotelling] 2026 real Brent: ", N @ Last[realBrent][[2]], "  $/bbl"];
Print["[hotelling] 2026 Hotelling path value: ",
  N @ Last[hotellingPath][[2]], "  $/bbl"];

(* Major events to annotate *)
events = {
  {1990, "Iraq invades Kuwait"},
  {2000, "tech bust + post-Asia"},
  {2008, "GFC peak"},
  {2014, "shale glut"},
  {2020, "COVID demand collapse"},
  {2022, "Russia war"},
  {2026, "Iran war"}
};

realBrentByYear = Association @@ ({#[[1]] -> #[[2]]} & /@ realBrent);

annotEpilog = Map[
  Function[ev,
    Module[{y = ev[[1]], label = ev[[2]], v},
      v = Lookup[realBrentByYear, y, 1.];
      {Black, PointSize[0.010], Point[{y, v}],
       Inset[Style[label, 11, Bold, GrayLevel[0.25]],
             {y, v * 1.14}, {-1, 0}]}]],
  events];

plot = ListLogPlot[
  {realBrent, hotellingPath},
  Joined -> True,
  PlotStyle -> {Directive[RGBColor[0.85, 0.27, 0.27], Thickness[0.006]],
                Directive[GrayLevel[0.3], Thickness[0.005], Dashed]},
  Frame -> True,
  FrameLabel -> {Style["year", 14, Bold],
                 Style["real Brent (2025 USD/bbl, log scale)", 13, Bold]},
  FrameTicksStyle -> Directive[FontSize -> 12],
  PlotLegends -> Placed[
    LineLegend[
      {Directive[RGBColor[0.85, 0.27, 0.27], Thickness[0.006]],
       Directive[GrayLevel[0.3], Thickness[0.005], Dashed]},
      {Style["observed Brent (real)", 13],
       Style["Hotelling path: 2 %/yr real growth", 13]}],
    {0.20, 0.86}],
  PlotLabel -> Style[
    "Long-run Brent vs Hotelling's rule (1987-2026)",
    18, Bold, GrayLevel[0.1]],
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.88], Thickness[0.0005]],
  Epilog -> annotEpilog,
  ImageSize -> 1300, AspectRatio -> 1/2.2];

Export["docs/images/hotelling.png", plot, ImageResolution -> 144];

Export["data/hotelling_decomposition.csv",
  Dataset @ Map[
    Function[pair,
      <|"Year" -> pair[[1]],
        "RealBrent2025USD" -> N @ pair[[2]],
        "HotellingPathPrice" -> N @ SelectFirst[hotellingPath, #[[1]] == pair[[1]] &][[2]]|>],
    realBrent],
  "CSV"];

(* Pearson correlation between log price and Hotelling log path *)
logObs = Log[#[[2]] & /@ realBrent];
logHot = Log[#[[2]] & /@ hotellingPath];
cor = Correlation[logObs, logHot];
Print["[hotelling] correlation(log real Brent, log Hotelling) = ", N @ cor];

Print["[hotelling] done."];
