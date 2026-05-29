#!/usr/bin/env wolframscript
(* germany_2019_animation.wls — country-wide kriged daily diesel
   animation for November 2019, using real prices from ~14 500
   stations across all of Germany. Every frame is a genuine kriged
   estimate from same-day data.

   Inputs:
     data/tk_archive_2019_11_daily.csv  (built by ingest_gustavz_archive.wls)

   Outputs:
     docs/images/germany_2019_animation.gif        — 30-frame loop
     docs/images/germany_2019_frames/*.png         — daily PNGs

   Data licence: CC BY-NC-SA 4.0 (Tankerkönig / MTS-K, gustavz mirror).
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/Kriging.wl"];

Print["[krige2019] loading aggregated daily table ..."];
daily = Normal @ Import["data/tk_archive_2019_11_daily.csv",
                        "Dataset", "HeaderLines" -> 1];
Print["[krige2019]   rows: ", Length[daily]];

(* Re-import auto-converts "2019-11-01" -> DateObject; normalise to strings. *)
toDateStr[d_DateObject] := DateString[d, "ISODate"];
toDateStr[s_String]     := s;
daily = Map[Append[#, "Date" -> toDateStr[#Date]] &, daily];

allDates = Sort @ Union[#Date & /@ daily];
Print["[krige2019] dates: ", Length[allDates], "  first/last: ",
      {First[allDates], Last[allDates]}];

(* Color scale fixed across all frames using monthly quantiles *)
allDiesels = #DieselMean & /@ daily;
priceMin = Quantile[allDiesels, 0.02];
priceMax = Quantile[allDiesels, 0.98];
priceSpan = Max[priceMax - priceMin, 0.01];
Print["[krige2019] color range (2/98 %): ", {priceMin, priceMax}];

colorFn = Function[p,
  If[NumberQ[p],
    ColorData["RedBlueTones"][Clip[1. - (p - priceMin)/priceSpan, {0., 1.}]],
    GrayLevel[0.96]]];

(* Build grid clipped to Germany *)
deBox = {{47.3, 55.0}, {5.9, 15.0}};
gridLat = Range @@ Append[deBox[[1]], 0.30];
gridLon = Range @@ Append[deBox[[2]], 0.45];
Print["[krige2019] grid size: ", {Length[gridLat], Length[gridLon]}];

(* Pre-compute, for each date, the kriging input + nearest-station mask *)
buildFrame[d_String, frameIdx_, totalFrames_] := Module[
  {dayRows, dayStns, vario, krige, grid, ttl},
  dayRows = Select[daily, #Date == d &];
  dayStns = Map[
    <|"lat" -> #Lat, "lng" -> #Lon, "price" -> #DieselMean|> &,
    dayRows];
  (* For speed, subsample to 1000 if more *)
  If[Length[dayStns] > 1200,
    SeedRandom[Hash[d]];
    dayStns = RandomSample[dayStns, 1200]];
  vario = Kriging`FitVariogram[dayStns];
  krige = Kriging`KrigeFunction[dayStns, vario, "nNearest" -> 18];

  nearestKm[la_, lo_] := Min @ (Kriging`HaversineKm[
     {la, lo}, {#["lat"], #["lng"]}] & /@ dayStns);
  grid = Kriging`KrigedGrid[krige, deBox, {0.18, 0.27},
    "Mask" -> (nearestKm[#1, #2] <= 50. &)];

  ttl = Column[{
    Style[StringTemplate["German diesel - kriged daily map - `1`"][d],
          18, Bold, GrayLevel[0.1]],
    Style[StringTemplate["frame `1`/`2`  *  `3` stations on this day"][
            frameIdx, totalFrames, Length[dayStns]],
          12, GrayLevel[0.35]],
    Style["Data: Tankerkonig / MTS-K (CC BY-NC-SA 4.0)",
          9, Italic, GrayLevel[0.5]]
  }, Alignment -> Center];

  (* SMOOTH render: vertex-coloured quads on a GeoGraphics basemap *)
  Kriging`KrigedGeoGraphics[grid, colorFn,
    GeoRange -> Entity["Country", "Germany"],
    GeoProjection -> "Mercator",
    GeoBackground -> Lighter[Gray, 0.97],
    "Opacity" -> 0.85,
    PlotLabel -> ttl,
    ImageSize -> 900]
];

framesDir = "docs/images/germany_2019_frames";
If[!FileExistsQ[framesDir], CreateDirectory[framesDir]];

Print["[krige2019] rendering ", Length[allDates], " daily frames ..."];
frames = Table[
  Module[{f = buildFrame[allDates[[i]], i, Length[allDates]]},
    Export[FileNameJoin[{framesDir,
      "frame_" <> StringReplace[allDates[[i]], "-" -> ""] <> ".png"}],
      f, ImageResolution -> 72];
    Print["[krige2019]   ", allDates[[i]]];
    f],
  {i, Length[allDates]}];

legend = BarLegend[
  {ColorData["RedBlueTones"][1 - #] &, {priceMin, priceMax}},
  LegendLabel -> Style["EUR/L", 13, Bold],
  LabelStyle -> 12, LegendMarkerSize -> 380];

(* Bake legend into each frame so the GIF stays self-contained *)
framesGif = ImageResize[Rasterize[Legended[#, legend]], 700] & /@ frames;

Print["[krige2019] assembling GIF ..."];
Export["docs/images/germany_2019_animation.gif", framesGif,
  "AnimationRepetitions" -> Infinity,
  "DisplayDurations" -> 0.45];

Print["[krige2019] wrote ",
      FileByteCount["docs/images/germany_2019_animation.gif"]/1024^2 // Round,
      " MB GIF with ", Length[framesGif], " frames."];
