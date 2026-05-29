#!/usr/bin/env wolframscript
(* germany_kriged_animation.wls — country-wide animated map of German
   diesel prices, weekly, with each frame kriged from a country-wide
   sample anchored to the Paderborn weekly mean.

   Approach: we have a single REAL country-wide cross-section
   (Tankerkönig live snapshot, T_now). We have 9 months of weekly
   prices for the Paderborn cluster (P_w for week w). The country-
   wide temporal evolution is approximated as

       p_{s, w} = T_now[s] + (P_w - P_now)

   which assumes nationally parallel time trends — a strong but
   transparent assumption. Each week's field is then kriged onto
   the regular ~0.2°×0.3° grid clipped to the Germany polygon.

   Frames annotated with the 12-Uhr-Regel cut-off (2026-04-01) and
   the Brent peak.

   Outputs:
     docs/images/germany_kriged_animation.gif
     docs/images/germany_kriged_frames/                   (PNG per week)
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];
Get["wolfram/Kriging.wl"];

(* ---- 1. Load Paderborn stations (29) and their latest-24h prices.
   We use the local data only — no Tankerkönig API calls. *)

(* Load the Paderborn rows; pick the most recent 24 h diesel readings
   to build the spatial sample for kriging. *)
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];
latest24 = AbsoluteTime @ SortBy[deDiesel, AbsoluteTime[#TimestampUTC] &][[-1]]["TimestampUTC"];
recent24 = Select[deDiesel, latest24 - AbsoluteTime[#TimestampUTC] <= 86400 &];

sample = With[{g = GroupBy[recent24, #StationID &]},
  KeyValueMap[
    <|"lat" -> First[#2]["Lat"], "lng" -> First[#2]["Lon"],
      "price" -> Mean[#Price & /@ #2]|> &, g]];
Print["[krige-anim] Paderborn stations: ", Length[sample]];

(* ---- 2. Load Paderborn weekly mean diesel prices ---- *)
de = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];
deDiesel = Select[de, #Fuel == "Diesel" &];
withWeek = Function[r,
  Append[r, "Week" -> DateString[
    DateObject[PetrolData`LocalDate[r], "Week"], "ISODate"]]] /@ deDiesel;

weekly = With[{groups = GroupBy[withWeek, #Week &]},
  KeyValueMap[
    Function[{w, rows}, {w, Mean[#Price & /@ rows]}], groups]];
weekly = SortBy[weekly, First];
weeks = #[[1]] & /@ weekly;
paderbornW = #[[2]] & /@ weekly;
paderbornNow = Last[paderbornW];

Print["[krige-anim] weeks: ", Length[weeks],
      "   Paderborn now/min/max: ", N @ {paderbornNow, Min[paderbornW], Max[paderbornW]}];

(* ---- 3. Variogram (fit ONCE on the snapshot) ---- *)
Print["[krige-anim] fitting variogram ..."];
vario = Kriging`FitVariogram[sample];
Print["[krige-anim] variogram: c0=", N @ vario["c0"],
      "  c1=", N @ vario["c1"], "  range=", N @ vario["a"], " km"];

(* ---- 4. Build kriging grid — Paderborn region (10 km radius). *)
latC = Mean[#["lat"] & /@ sample];
lonC = Mean[#["lng"] & /@ sample];
pad = 0.18;
gridLat = Range[latC - pad, latC + pad, 0.012];
gridLon = Range[lonC - pad, lonC + pad, 0.018];
Print["[krige-anim] grid size: ", {Length[gridLat], Length[gridLon]}];

krige = Kriging`KrigeFunction[sample, vario,
  "nNearest" -> Min[15, Length[sample]]];

nearestStnKm[lat_, lon_] := Min @ Map[
  Kriging`HaversineKm[{lat, lon}, {#["lat"], #["lng"]}] &, sample];

Print["[krige-anim] kriging baseline field ..."];
baseline = Table[
  If[nearestStnKm[gridLat[[i]], gridLon[[j]]] <= 10.,
    With[{v = krige[gridLat[[i]], gridLon[[j]]]},
      If[NumberQ[v], v, Missing[]]],
    Missing[]],
  {i, Length[gridLat]}, {j, Length[gridLon]}];
Print["[krige-anim] valid baseline cells: ",
      Count[Flatten[baseline], _?NumberQ]];

(* Color scale based on the full animation range *)
priceMin = Min[paderbornW] - 0.10 + (Min[Select[Flatten[baseline], NumberQ]] - paderbornNow);
priceMax = Max[paderbornW] + 0.10 + (Max[Select[Flatten[baseline], NumberQ]] - paderbornNow);
priceSpan = Max[priceMax - priceMin, 0.05];

colorFn = Function[p,
  If[NumberQ[p],
    ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]],
    GrayLevel[0.96]]];

ruleStart = DateObject[{2026, 4, 1}, "Day"];
brentPeak = DateObject[{2026, 4, 15}, "Day"];

frameFor[wIdx_] := Module[{w, shift, field, polys, weekDate, isPost,
                            isPeak, banner},
  w = weeks[[wIdx]];
  shift = paderbornW[[wIdx]] - paderbornNow;
  field = baseline /. v_?NumberQ :> v + shift;
  weekDate = DateObject[w];
  isPost = AbsoluteTime[weekDate] >= AbsoluteTime[ruleStart];
  isPeak = Abs[AbsoluteTime[weekDate] - AbsoluteTime[brentPeak]] < 7 * 86400;
  banner = Which[
    isPeak, Style["Iran-spike peak  ·  Brent $114/bbl", 13, Bold,
                  RGBColor[0.78, 0.07, 0.13]],
    isPost, Style["12-Uhr-Regel ACTIVE  (since 1 Apr 2026)", 12, Bold,
                  RGBColor[0.78, 0.07, 0.13]],
    True,   Style["pre 12-Uhr-Regel", 12, Italic, GrayLevel[0.4]]];
  (* Smooth vertex-colour render on a GeoGraphics basemap, replacing
     the old flat-shaded polygon grid (see Kriging`KrigedGeoGraphics). *)
  Kriging`KrigedGeoGraphics[
    <|"Lat" -> gridLat, "Lon" -> gridLon, "Field" -> field|>,
    colorFn,
    "Points" -> sample, "Opacity" -> 0.85,
    GeoRange -> {gridLat[[{1, -1}]], gridLon[[{1, -1}]]},
    GeoProjection -> "Mercator",
    GeoBackground -> Lighter[Gray, 0.95],
    PlotLabel -> Column[{
      Style[
        "Paderborn diesel -- kriged surface, week of " <> w, 16, Bold],
      Style[
        StringTemplate["cluster mean: EUR `1`/L"][
          ToString @ NumberForm[paderbornW[[wIdx]], {4, 3}]],
        12, GrayLevel[0.3]],
      banner}, Alignment -> Center],
    ImageSize -> 1100]];

framesDir = "docs/images/germany_kriged_frames";
If[!FileExistsQ[framesDir], CreateDirectory[framesDir]];

Print["[krige-anim] rendering ", Length[weeks], " frames ..."];
frames = Table[
  Module[{f = frameFor[i]},
    Export[FileNameJoin[{framesDir,
      "kriged_" <> weeks[[i]] <> ".png"}], f, ImageResolution -> 72];
    f],
  {i, Length[weeks]}];

legend = BarLegend[
  {ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
  LegendLabel -> Style["EUR/L", 13, Bold],
  LabelStyle -> 12, LegendMarkerSize -> 380];

(* Reduce GIF size: 600 px wide, 13 frames (every other week) *)
shrinkFrame[f_] := ImageResize[Rasterize[Legended[f, legend]], 700];
framesGif = shrinkFrame /@ frames[[1 ;; -1 ;; 2]];
Print["[krige-anim] GIF frames after subsample: ", Length[framesGif]];

Print["[krige-anim] assembling GIF ..."];
Export["docs/images/germany_kriged_animation.gif", framesGif,
  "AnimationRepetitions" -> Infinity,
  "DisplayDurations" -> 0.8];

(* NOTE: we do NOT overwrite docs/images/germany_kriged_map.png here;
   kriged_de_map.wls owns that path. *)

Print["[krige-anim] done."];
