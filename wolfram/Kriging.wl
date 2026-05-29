(* ::Package:: *)

(* Kriging.wl — general-purpose ordinary kriging.

   ----------------------------------------------------------------
   WHAT KRIGING IS (read this before using the code)
   ----------------------------------------------------------------
   Kriging is the geostatistician's interpolator: given a scalar
   quantity Z measured at a scattered set of sample locations x_i,
   it predicts Z at an unobserved location x_0 as a weighted average
   of the samples,   Z*(x_0) = Sum_i lambda_i Z(x_i),  choosing the
   weights lambda_i to be the Best Linear Unbiased Predictor (BLUP).

   "Best"        = minimum mean-squared prediction error.
   "Unbiased"    = the weights sum to 1, so a constant field is
                   reproduced exactly (ordinary kriging).
   The trick that makes it more than inverse-distance weighting is
   that the weights are derived from the *spatial covariance
   structure of the data itself*, summarised by the VARIOGRAM

        gamma(h) = (1/2) E[ (Z(x) - Z(x+h))^2 ]

   i.e. how fast pairs of points decorrelate as their separation h
   grows. We fit a parametric model to the empirical variogram:

        Exponential:  gamma(h) = c0 + c1 (1 - exp(-h/a))
        Spherical:    gamma(h) = c0 + c1 (3h/2a - (h/a)^3/2),  h<a
        Gaussian:     gamma(h) = c0 + c1 (1 - exp(-(h/a)^2))

   with nugget c0 (micro-scale noise / measurement error), sill
   c0+c1 (the variance plateau), and range a (the distance beyond
   which points are effectively uncorrelated). Ordinary kriging at
   x_0 then solves the (n+1)x(n+1) Lagrangian system

        [ Gamma  1 ] [ lambda ]   [ gamma_0 ]
        [ 1^T    0 ] [   mu   ] = [    1    ]

   where Gamma_ij = gamma(|x_i - x_j|), gamma_0,i = gamma(|x_i-x_0|),
   and mu is the Lagrange multiplier enforcing Sum lambda_i = 1. The
   minimised error (the KRIGING VARIANCE) is sigma^2(x_0) =
   lambda.gamma_0 + mu — a genuine, location-dependent uncertainty.

   ----------------------------------------------------------------
   THIS PACKAGE IS DELIBERATELY GENERAL
   ----------------------------------------------------------------
   Nothing here is petrol-specific. A "sample" is any association
   carrying coordinate keys and a value key, OR a plain {coords,
   value} pair where `coords` is a numeric vector of any dimension.
   The distance function is pluggable (HaversineKm for lon/lat data,
   EuclideanDistance for projected/abstract data, or anything you
   pass). So the same code kriges petrol prices on the globe, ore
   grades on a mine grid, or a 3-D temperature field.

      pts   = {<|"x"->.., "y"->.., "z"->..|>, ...};
      vg    = FitVariogram[pts, "CoordinateKeys"->{"x","y"},
                                "ValueKey"->"z",
                                DistanceFunction->EuclideanDistance,
                                "Model"->"Spherical"];
      f     = KrigeFunction[pts, vg, "CoordinateKeys"->{"x","y"}];
      f[{3.1, 4.2}]                       (* kriged estimate        *)
      f[{3.1, 4.2}, "Variance"]           (* kriging variance there *)

   For lon/lat petrol data the historical 2-argument call
   `krige[lat, lon]` and the keys "lat"/"lng"/"price" still work
   unchanged, so existing scripts need no edits.
*)

BeginPackage["Kriging`"];

HaversineKm::usage =
  "HaversineKm[{lat1,lon1},{lat2,lon2}] returns the great-circle \
distance in km.";

FitVariogram::usage =
  "FitVariogram[samples, opts] fits a theoretical variogram by \
least-squares on the binned empirical variogram. Options: \
\"Model\" (\"Exponential\"|\"Spherical\"|\"Gaussian\"), DistanceFunction, \
\"CoordinateKeys\", \"ValueKey\", \"Bins\", \"SampleSize\", \"MaxRange\". \
Returns an Association with c0, c1, a, model, distance function and \
the empirical cloud. \"MaxRange\" bounds the fitted range so the \
optimiser cannot run away when the empirical variogram never \
plateaus.";

KrigeFunction::usage =
  "KrigeFunction[samples, variogram, opts] returns an ordinary-kriging \
predictor f. Call f[{c1,c2,...}] or, for 2-D, f[c1,c2] to get the \
kriged value; f[coords, \"Variance\"] returns the kriging variance and \
f[coords, \"Both\"] returns {value, variance}. Option \"nNearest\" sets \
the local neighbourhood size.";

KrigedGrid::usage =
  "KrigedGrid[f, {{latMin,latMax},{lonMin,lonMax}}, {dLat,dLon}, opts] \
evaluates a kriging predictor on a regular grid and returns \
<|\"Lat\"->..,\"Lon\"->..,\"Field\"->matrix|>. Cells failing the \
optional \"Mask\" test become Missing[].";

KrigedFieldPlot::usage =
  "KrigedFieldPlot[grid, colorFn, opts] renders a kriged grid as a \
SMOOTH interpolated surface (bicubic by default) rather than flat \
blocks, in plain (lon,lat) coordinates. For ABSTRACT / non-geographic \
fields. Option \"Region\" clips the surface to a Region/Entity; \
\"InterpolationOrder\" controls smoothness; \"Contours\" switches to \
filled-contour banding.";

KrigedGeoGraphics::usage =
  "KrigedGeoGraphics[grid, colorFn, opts] renders a kriged grid on a \
real GeoGraphics basemap. Each grid cell becomes a quad in a \
GraphicsComplex with per-vertex colours, so the colour field is \
SMOOTHLY interpolated between grid nodes instead of drawn as flat \
blocks; the coastline/relief comes from GeoBackground and the surface \
is clipped to the cells that pass the grid's mask. This is the \
geographic counterpart of KrigedFieldPlot. Options: GeoBackground, \
GeoRange, GeoProjection, \"Points\" (overlay station markers), \
\"Opacity\", ImageSize.";

Begin["`Private`"];

(* ================================================================ *)
(*  Distance                                                         *)
(* ================================================================ *)

HaversineKm[{lat1_, lon1_}, {lat2_, lon2_}] := Module[
  {dLat = (lat2 - lat1) Pi/180, dLon = (lon2 - lon1) Pi/180,
   l1 = lat1 Pi/180, l2 = lat2 Pi/180, a},
  a = Sin[dLat/2]^2 + Cos[l1] Cos[l2] Sin[dLon/2]^2;
  6371. * 2 * ArcSin[Sqrt[Min[1., a]]]
];

(* ================================================================ *)
(*  Theoretical variogram models  (private: System`VariogramModel    *)
(*  already exists, so we keep our own selector internal)            *)
(* ================================================================ *)

modelFn["Exponential"] = Function[{h, c0, c1, a},
  c0 + c1 (1 - Exp[-h/a])];
modelFn["Gaussian"] = Function[{h, c0, c1, a},
  c0 + c1 (1 - Exp[-(h/a)^2])];
(* Piecewise (not If) so NonlinearModelFit can differentiate it cleanly. *)
modelFn["Spherical"] = Function[{h, c0, c1, a},
  Piecewise[{{c0 + c1 (1.5 (h/a) - 0.5 (h/a)^3), h < a}, {c0 + c1, h >= a}}, c0 + c1]];

(* ================================================================ *)
(*  Sample coordinate / value extraction (general)                   *)
(* ================================================================ *)

coordOf[s_Association, keys_List] := Lookup[s, keys];
coordOf[{c_List, _}, _] := c;
valueOf[s_Association, key_] := s[key];
valueOf[{_, v_}, _] := v;

(* ================================================================ *)
(*  Empirical (binned) variogram                                     *)
(* ================================================================ *)

pairCloud[coords_, vals_, distFn_, sampleN_] := Module[{n, idx},
  n = Length[coords];
  idx = If[n > sampleN, RandomSample[Range[n], sampleN], Range[n]];
  Flatten[
    Table[
      With[{a = idx[[i]], b = idx[[j]]},
        {distFn[coords[[a]], coords[[b]]],
         (vals[[a]] - vals[[b]])^2 / 2.}],
      {i, Length[idx]}, {j, i + 1, Length[idx]}],
    1]
];

binnedVariogram[cloud_, binEdges_] := Table[
  With[{inBin = Select[cloud,
      binEdges[[i]] <= #[[1]] < binEdges[[i + 1]] &]},
    {(binEdges[[i]] + binEdges[[i + 1]]) / 2.,
     If[Length[inBin] >= 4, Mean[#[[2]] & /@ inBin], Missing[]]}],
  {i, 1, Length[binEdges] - 1}];

Options[FitVariogram] = {
  "Model" -> "Exponential",
  DistanceFunction -> HaversineKm,
  "CoordinateKeys" -> {"lat", "lng"},
  "ValueKey" -> "price",
  "Bins" -> Automatic,
  "SampleSize" -> 1500,
  "MaxRange" -> Automatic};

FitVariogram[samples_List, opts:OptionsPattern[]] := Module[
  {model, distFn, ckeys, vkey, bins, sampleN, maxRange,
   coords, vals, cloud, edges, ev, mfun, fit, c0, c1, a, maxH, hSpan},
  model    = OptionValue["Model"];
  distFn   = OptionValue[DistanceFunction];
  ckeys    = OptionValue["CoordinateKeys"];
  vkey     = OptionValue["ValueKey"];
  sampleN  = OptionValue["SampleSize"];
  maxRange = OptionValue["MaxRange"];

  coords = coordOf[#, ckeys] & /@ samples;
  vals   = valueOf[#, vkey] & /@ samples;
  cloud  = pairCloud[coords, vals, distFn, sampleN];
  maxH   = Max[cloud[[All, 1]]];

  edges = OptionValue["Bins"];
  If[edges === Automatic,
    edges = maxH * {0., .02, .05, .1, .15, .25, .4, .6, .8, 1.}];
  ev = Select[binnedVariogram[cloud, edges], NumberQ[#[[2]]] &];

  (* Bound the range so NonlinearModelFit cannot send a -> Infinity
     when the empirical variogram keeps rising past the last bin. *)
  hSpan = If[Length[ev] >= 2, Max[ev[[All, 1]]], maxH];
  If[maxRange === Automatic, maxRange = hSpan];

  If[Length[ev] < 3,
    Return[<|"c0" -> 0., "c1" -> Variance[vals], "a" -> maxRange/3.,
             "model" -> model, "distance" -> distFn,
             "emp" -> ev, "fit" -> None|>]];

  mfun = modelFn[model];
  fit = Quiet @ NonlinearModelFit[ev,
    {mfun[h, c0, c1, a], c0 >= 0, c1 >= 0,
     maxRange/50. <= a <= maxRange},
    {{c0, 1*^-4}, {c1, Variance[vals]}, {a, maxRange/3.}}, h];

  <|"c0" -> Clip[c0 /. fit["BestFitParameters"], {0., Infinity}],
    "c1" -> Clip[c1 /. fit["BestFitParameters"], {0., Infinity}],
    "a"  -> Clip[a  /. fit["BestFitParameters"], {maxRange/50., maxRange}],
    "model" -> model, "distance" -> distFn,
    "emp" -> ev, "fit" -> fit|>
];

variogramAt[v_, h_] := modelFn[v["model"]][h, v["c0"], v["c1"], v["a"]];

(* ================================================================ *)
(*  Ordinary kriging at a single query point  (value + variance)     *)
(* ================================================================ *)

krigeAt[coords_, vals_, distFn_, vario_, nNearest_, query_] := Module[
  {distQ, near, idx, ck, n, K, kvec, sol, w, mu},
  distQ = distFn[query, #] & /@ coords;
  near  = TakeSmallest[Transpose[{distQ, Range[Length[coords]]}], nNearest];
  idx   = near[[All, 2]];
  ck    = coords[[idx]];
  n     = Length[idx];
  K = Table[variogramAt[vario, distFn[ck[[i]], ck[[j]]]], {i, n}, {j, n}];
  K = ArrayPad[K, {{0, 1}, {0, 1}}, 1.];
  K[[-1, -1]] = 0.;
  kvec = Append[Table[variogramAt[vario, near[[i, 1]]], {i, n}], 1.];
  sol = Quiet @ LinearSolve[K, kvec];
  If[Head[sol] === LinearSolve || !VectorQ[sol, NumberQ],
    Return[<|"value" -> Missing[], "variance" -> Missing[]|>]];
  w  = sol[[1 ;; n]];
  mu = sol[[-1]];
  <|"value" -> w . vals[[idx]],
    "variance" -> Clip[w . kvec[[1 ;; n]] + mu, {0., Infinity}]|>
];

Options[KrigeFunction] = {
  "nNearest" -> 24,
  DistanceFunction -> Automatic,
  "CoordinateKeys" -> {"lat", "lng"},
  "ValueKey" -> "price"};

KrigeFunction[samples_List, vario_, opts:OptionsPattern[]] := Module[
  {nNearest, distFn, ckeys, vkey, coords, vals, predict},
  nNearest = OptionValue["nNearest"];
  distFn   = OptionValue[DistanceFunction] /.
               Automatic -> Lookup[vario, "distance", HaversineKm];
  ckeys    = OptionValue["CoordinateKeys"];
  vkey     = OptionValue["ValueKey"];
  coords   = coordOf[#, ckeys] & /@ samples;
  vals     = valueOf[#, vkey] & /@ samples;
  predict[q_List, which_:"Value"] := Module[{r},
    r = krigeAt[coords, vals, distFn, vario, Min[nNearest, Length[coords]], q];
    Switch[which,
      "Value",    r["value"],
      "Variance", r["variance"],
      "Both",     {r["value"], r["variance"]},
      _,          r["value"]]];
  (* Accept f[{c1,c2,..}], f[{..},"Variance"], and legacy f[lat,lon]. *)
  With[{p = predict},
    Function[
      Switch[{##},
        {_List},          p[{##}[[1]]],
        {_List, _String}, p[{##}[[1]], {##}[[2]]],
        {_?NumberQ, _?NumberQ}, p[{##}],
        _,                p[{First[{##}]}]]]]
];

(* ================================================================ *)
(*  Grid evaluation + SMOOTH rendering                               *)
(* ================================================================ *)

Options[KrigedGrid] = {"Mask" -> None};

KrigedGrid[f_, {{latMin_, latMax_}, {lonMin_, lonMax_}},
           {dLat_, dLon_}, opts:OptionsPattern[]] := Module[
  {mask, lats, lons, field},
  mask = OptionValue["Mask"];
  lats = Range[latMin, latMax, dLat];
  lons = Range[lonMin, lonMax, dLon];
  field = Table[
    If[mask === None || TrueQ[mask[la, lo]],
      With[{v = f[{la, lo}]}, If[NumberQ[v], v, Missing[]]],
      Missing[]],
    {la, lats}, {lo, lons}];
  <|"Lat" -> lats, "Lon" -> lons, "Field" -> field|>
];

Options[KrigedFieldPlot] = {
  "Region" -> None, "Boundary" -> None,
  "InterpolationOrder" -> 3, "Contours" -> None,
  "PlotRange" -> Automatic, ImageSize -> 620};

KrigedFieldPlot[grid_Association, colorFn_, opts:OptionsPattern[]] := Module[
  {lats, lons, field, pts, region, regFn, boundary, io, contours,
   pr, isz, meanLat, aspect, base, epi},
  lats   = grid["Lat"];  lons = grid["Lon"];  field = grid["Field"];
  region = OptionValue["Region"];
  io     = OptionValue["InterpolationOrder"];
  contours = OptionValue["Contours"];
  pr     = OptionValue["PlotRange"];
  isz    = OptionValue[ImageSize];
  boundary = OptionValue["Boundary"];

  (* {lon, lat, value} cloud, lon as x so the picture reads as a map *)
  pts = Flatten[Table[
      With[{v = field[[i, j]]},
        If[NumberQ[v], {lons[[j]], lats[[i]], v}, Nothing]],
      {i, Length[lats]}, {j, Length[lons]}], 1];

  (* Clip smooth fill to a geographic region if requested. Country
     polygons are in {lat,lon}; the plot's x is lon, y is lat, so we
     query the membership test in {lat,lon} = {y,x}. *)
  regFn = If[region === None, (True &),
    With[{rm = RegionMember[
        region /. e_Entity :> e["Polygon"]]},
      Function[{x, y}, rm[{y, x}]]]];

  (* Latitude-corrected aspect ratio so the map isn't stretched *)
  meanLat = Mean[lats];
  aspect  = (Max[lats] - Min[lats]) / (Max[lons] - Min[lons]) /
              Cos[meanLat Degree];

  base = If[contours === None,
    ListDensityPlot[pts,
      InterpolationOrder -> io, ColorFunction -> colorFn,
      ColorFunctionScaling -> False, RegionFunction -> regFn,
      PlotRange -> pr, AspectRatio -> aspect, Frame -> False,
      ImageSize -> isz, ImagePadding -> 2],
    ListContourPlot[pts,
      InterpolationOrder -> io, ColorFunction -> colorFn,
      ColorFunctionScaling -> False, RegionFunction -> regFn,
      Contours -> contours, ContourStyle -> Directive[GrayLevel[1], Opacity[.35]],
      PlotRange -> pr, AspectRatio -> aspect, Frame -> False,
      ImageSize -> isz, ImagePadding -> 2]];

  epi = If[boundary === None, {}, boundary];
  If[epi === {}, base, Show[base, Epilog -> epi]]
];

(* ---- geographic smooth renderer: GraphicsComplex + VertexColors ---- *)

Options[KrigedGeoGraphics] = {
  GeoBackground -> "VectorMinimal", GeoRange -> Automatic,
  GeoProjection -> "Mercator", "Points" -> None, "Opacity" -> 1.,
  ImageSize -> 700};

KrigedGeoGraphics[grid_Association, colorFn_, opts:OptionsPattern[]] := Module[
  {lats, lons, field, nLat, nLon, idx, k, verts, vColors, quads,
   complex, geoRange, pts, ptMarks, op},
  lats = grid["Lat"];  lons = grid["Lon"];  field = grid["Field"];
  nLat = Length[lats]; nLon = Length[lons];
  op   = OptionValue["Opacity"];

  (* Number the grid nodes; build vertex list (GeoPosition {lat,lon})
     and per-vertex colours. Invalid nodes get index 0 / no colour. *)
  k = 0; idx = ConstantArray[0, {nLat, nLon}];
  verts = {}; vColors = {};
  Do[
    If[NumberQ[field[[i, j]]],
      k++; idx[[i, j]] = k;
      AppendTo[verts, {lats[[i]], lons[[j]]}];
      AppendTo[vColors, colorFn[field[[i, j]]]]],
    {i, nLat}, {j, nLon}];

  (* Emit a quad for every cell whose four corners are all valid;
     VertexColors makes GeoGraphics blend the colours across it. *)
  quads = Reap[
    Do[
      With[{a = idx[[i, j]], b = idx[[i + 1, j]],
            c = idx[[i + 1, j + 1]], d = idx[[i, j + 1]]},
        If[a > 0 && b > 0 && c > 0 && d > 0, Sow[{a, b, c, d}]]],
      {i, nLat - 1}, {j, nLon - 1}]][[2]];
  quads = If[quads === {}, {}, First[quads]];

  complex = GraphicsComplex[GeoPosition[verts],
    {EdgeForm[None], Opacity[op], Polygon[quads]},
    VertexColors -> vColors];

  geoRange = OptionValue[GeoRange] /. Automatic ->
    {MinMax[lats], MinMax[lons]};

  pts = OptionValue["Points"];
  ptMarks = If[pts === None, {},
    {Black, PointSize[0.006],
     Point[GeoPosition[{#["lat"], #["lng"]}]] & /@ pts}];

  GeoGraphics[{complex, ptMarks},
    Sequence @@ FilterRules[{opts},
      Except[{"Points", "Opacity", GeoBackground, GeoRange,
              GeoProjection, ImageSize}]],
    GeoRange -> geoRange,
    GeoProjection -> OptionValue[GeoProjection],
    GeoBackground -> OptionValue[GeoBackground],
    ImageSize -> OptionValue[ImageSize]]
];

End[];
EndPackage[];
