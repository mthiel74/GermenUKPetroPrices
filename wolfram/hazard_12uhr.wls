#!/usr/bin/env wolframscript
(* hazard_12uhr.wls — Analysis §2 (the headline 12-Uhr-Regel plot)
   For each station, identify price-change events as transitions
   p_t != p_{t-1} for any of E5 / E10 / Diesel. Classify each event
   as "up" or "down" by the sign of the change in diesel (or e5 if
   diesel missing). Bin events by local minute-of-day and split
   pre / post 2026-04-01.

   Outputs:
     docs/images/hazard_rate_prepost.png    — 4-panel (up/down × pre/post)
     docs/images/hazard_rate_polar.png      — polar comparison

   The 12-Uhr-Regel hypothesis: post-rule, the "up" hazard rate should
   concentrate sharply at 12:00, while the "down" hazard remains
   spread across the day.
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

Print["[hazard] loading DE rows ..."];
deAll = Normal @ PetrolData`LoadLocalDE[PetrolData`Settings["PaderbornRawDE"]];

(* For one station, build the time series of (timestamp, e5, e10, diesel)
   from the long-format rows: group by timestamp. *)

wide = (
  KeyValueMap[
    Function[{key, rows},
      With[{r1 = First @ rows},
        <|
          "StationID" -> r1["StationID"],
          "TimestampUTC" -> r1["TimestampUTC"],
          "TimeZone" -> r1["TimeZone"],
          "E5" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "E5" &], NumberQ, Missing[]],
          "E10" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "E10" &], NumberQ, Missing[]],
          "Diesel" -> SelectFirst[#Price & /@ Select[rows, #Fuel == "Diesel" &], NumberQ, Missing[]]
        |>
      ]
    ],
    GroupBy[deAll, {#StationID, #TimestampUTC} &]
  ]
);

Print["[hazard]   ", Length[wide], " (station, timestamp) wide rows"];

(* Group by station, sort by time, diff *)
byStation = SortBy[#, AbsoluteTime[#TimestampUTC] &] & /@
            GroupBy[wide, #StationID &];

Print["[hazard] computing change events ..."];

changeEvents = Catenate @ KeyValueMap[
  Function[{sid, srows},
    Module[{pairs = Partition[srows, 2, 1], events},
      events = Map[
        Function[pair,
          With[{a = pair[[1]], b = pair[[2]]},
            Module[{dD = b["Diesel"] - a["Diesel"],
                    dE = b["E5"] - a["E5"]},
              If[NumberQ[dD] && Abs[dD] > 0.001,
                <|"StationID" -> sid, "TimestampUTC" -> b["TimestampUTC"],
                  "TimeZone" -> b["TimeZone"],
                  "Direction" -> If[dD > 0, "up", "down"],
                  "Delta" -> dD|>,
                If[NumberQ[dE] && Abs[dE] > 0.001,
                  <|"StationID" -> sid, "TimestampUTC" -> b["TimestampUTC"],
                    "TimeZone" -> b["TimeZone"],
                    "Direction" -> If[dE > 0, "up", "down"],
                    "Delta" -> dE|>,
                  Nothing]]]
          ]
        ],
        pairs
      ];
      events
    ]
  ],
  byStation
];

changeEvents = DeleteCases[changeEvents, Nothing];
Print["[hazard]   ", Length[changeEvents], " price-change events"];

(* Attach LocalMinuteOfDay (0..1439) and AfterRule *)
withMin = (Append[#, {
  "MinuteOfDay" -> Module[{lc = First @ TimeZoneConvert[#TimestampUTC, #TimeZone]},
    lc[[4]] * 60 + lc[[5]]],
  "AfterRule" -> PetrolData`IsAfterRule[#]
}] &) /@ changeEvents;

Print["[hazard] binning events by 5-minute bucket × direction × rule ..."];

(* 5-minute buckets: 0..287 covering 24 hours *)
counts = Length /@ GroupBy[withMin,
  {#Direction, #AfterRule, Quotient[#MinuteOfDay, 5]} &];

Print["[hazard] count keys (sample): ", Take[Keys[counts], UpTo[5]]];
Print["[hazard] max bucket count: ", Max @ Values @ counts];

(* Empirical hazard: count of events per bucket per (station × day) *)
(* Station-days observed: count of distinct (station, LocalDate) pairs
   in each rule period — needed as denominator. *)
deWithLocal = (Append[#, {
   "AfterRule" -> PetrolData`IsAfterRule[#],
   "LocalDate" -> PetrolData`LocalDate[#]}] & ) /@ deAll;

stationDays = Length @ Union[{#StationID, #LocalDate} & /@ #] & /@
  GroupBy[deWithLocal, #AfterRule &];

Print["[hazard] station-days pre/post: ", stationDays];

hazard[direction_, afterRule_] := Table[
  Lookup[counts, Key[{direction, afterRule, b}], 0] /
    N[Lookup[stationDays, afterRule, 1] * (5/(60*24))],
  {b, 0, 287}];

(* hazard normalised so units are: events per station per hour at that
   time-of-day. *)

hr = Range[0, 287] * 5 / 60.;   (* fractional hours of day *)

mkPlot[direction_, afterRule_, color_, dash_, label_] :=
  ListLinePlot[Transpose[{hr, hazard[direction, afterRule]}],
    PlotStyle -> {color, Thickness[0.006], dash},
    PlotLabel -> Style[label, 15, Bold, GrayLevel[0.15]],
    Frame -> True,
    FrameLabel -> {Style["local hour (Europe/Berlin)", 14, Bold],
                   Style["events / station / hour", 13, Bold]},
    FrameTicksStyle -> Directive[FontSize -> 12],
    PlotRange -> {{0, 24}, All},
    GridLines -> {Range[0, 24, 2], Automatic},
    GridLinesStyle -> Directive[GrayLevel[0.85], Dotted],
    Epilog -> {Directive[Crimson, Dashed, Thickness[0.003]],
               Line[{{12, 0}, {12, 1000}}],
               Text[Style["12:00", 12, Italic, Bold, Crimson],
                    {12.5, Scaled[0.92]}, {-1, 0}]},
    ImageSize -> 700, AspectRatio -> 1/1.6,
    PlotTheme -> "Detailed"];

up   = RGBColor[0.85, 0.27, 0.27];
down = RGBColor[0.20, 0.60, 0.86];
Crimson = RGBColor[0.78, 0.10, 0.18];

panels = {
  mkPlot["up",   False, up,   None, "Price UP events, pre 12-Uhr-Regel"],
  mkPlot["up",   True,  up,   None, "Price UP events, post 12-Uhr-Regel"],
  mkPlot["down", False, down, None, "Price DOWN events, pre 12-Uhr-Regel"],
  mkPlot["down", True,  down, None, "Price DOWN events, post 12-Uhr-Regel"]};

grid = Grid[Partition[panels, 2], Spacings -> {1, 1}, Frame -> All, FrameStyle -> GrayLevel[0.85]];

Print["[hazard] writing 4-panel plot ..."];
Export["docs/images/hazard_rate_prepost.png", grid, ImageResolution -> 144];

(* Polar overlay: up-events, pre vs post, on a 24-h clock *)

(* Clock-face orientation: h=0 at bottom, h=6 right, h=12 top, h=18 left *)
toPolar[h_] := {Sin[Pi h/12], -Cos[Pi h/12]};
unitCircle = Circle[{0, 0}, 1];

polarPath[direction_, afterRule_, color_, dash_] := With[
  {vals = hazard[direction, afterRule]},
  {color, Thickness[0.004], dash,
   Line @ MapIndexed[
     Function[{v, idx}, (1 + v / 200.) * toPolar[hr[[idx[[1]]]]]],
     vals
   ]}];

polarHourTicks = Table[
  {GrayLevel[0.6], Line[{toPolar[h], 1.05 toPolar[h]}],
   Text[Style[ToString[h], 9], 1.13 toPolar[h]]},
  {h, 0, 23, 1}];

polarFig = Graphics[{
  GrayLevel[0.85], unitCircle,
  polarHourTicks,
  GrayLevel[0.4], Dashed, Line[{{0, -1.3}, {0, 1.3}}],   (* 12 o'clock axis *)
  polarPath["up", False, up, None],
  polarPath["up", True,  up, Dashing[{0.02, 0.015}]]
}, ImageSize -> 700, PlotRange -> 1.4 {{-1, 1}, {-1, 1}},
   PlotLabel -> Style[
     "Price-UP hazard on a 24-h clock\nsolid = pre 12-Uhr-Regel, dashed = post",
     14]];

Print["[hazard] writing polar plot ..."];
Export["docs/images/hazard_rate_polar.png", polarFig, ImageResolution -> 144];

Print["[hazard] done."];
