(* ::Package:: *)

(* PetrolData.wl
   Shared data loaders for the German + UK petrol price study.

   Conventions
   -----------
   * Prices are stored *internally* in € per litre, regardless of the
     source format. UK pence are converted via /100; the GBP→EUR
     conversion (for cross-market plots) uses a fixed nominal rate.
   * Timestamps are DateObjects (UTC). LocalTimestamp gives the local
     civil time so we can recover hour-of-day with the right offset.
   * The long table schema:
       Country (DE | UK), StationID, Brand, Lat, Lon, City, Postcode,
       TimestampUTC, LocalTimestamp, Fuel (E5|E10|Diesel), Price
*)

BeginPackage["PetrolData`"];

LoadLocalDE::usage =
  "LoadLocalDE[dir] reads all prices.csv files under dir recursively \
and returns a long-format Dataset (one row per station-timestamp-fuel).";

LoadLocalUK::usage =
  "LoadLocalUK[dir] reads UK prices.csv files and returns a long-format \
Dataset with prices converted from pence to EUR/L.";

LoadAll::usage =
  "LoadAll[] loads both DE and UK from the default project paths.";

LocalHour::usage =
  "LocalHour[row] returns the integer hour-of-day (0–23) in the \
station's civil time zone (Europe/Berlin or Europe/London).";

LocalDate::usage =
  "LocalDate[row] returns the calendar date (DateObject, granularity Day) \
in the station's civil time zone.";

LocalWeekday::usage =
  "LocalWeekday[row] returns 1..7 (Monday=1) for the station-local date.";

IsAfterRule::usage =
  "IsAfterRule[row] returns True if the row's timestamp is on or after \
2026-04-01 00:00 Europe/Berlin (the 12-Uhr-Regel start).";

StationMaster::usage =
  "StationMaster[ds] returns one row per station with metadata.";

Settings::usage =
  "Association of project-wide settings.";

Begin["`Private`"];

ProjectRoot[] := FileNameDrop[$InputFileName, -2];

Settings = <|
  "GBPEUR"        -> 1.15,
  "PenceToEUR"    -> 1.15/100,
  "PaderbornRawDE" -> FileNameJoin[{ProjectRoot[], "data/raw/local/fuel-data"}],
  "AberdeenRawUK"  -> FileNameJoin[{ProjectRoot[], "data/raw/local/uk-fuel-data"}],
  "RuleStart"      -> DateObject[{2026, 4, 1}, "Day", TimeZone -> "Europe/Berlin"]
|>;

(* -------------------------------------------------------------- *)
(* Defensive parsers (handle either pre-typed or raw strings)      *)

toDate[s_String] := Quiet @ DateObject[StringTrim @ s, TimeZone -> "UTC"];
toDate[d_DateObject] := d;
toDate[_] := Missing["NotAvailable"];

toFloat[s_String] := Module[{x = Quiet @ ToExpression @ StringTrim @ s},
  If[NumberQ[x], x, Missing["NotAvailable"]]];
toFloat[x_?NumberQ] := x;
toFloat[_] := Missing["NotAvailable"];

(* Import returns Dataset where each row is an Association. Numeric
   columns get auto-converted; we still wrap in toFloat for safety. *)

readCSVRows[path_String] := Module[{ds},
  ds = Import[path, "Dataset", "HeaderLines" -> 1];
  If[Head[ds] === Dataset, Normal[ds], {}]
];

findPriceCSVs[dir_String] := FileNames["prices.csv", dir, Infinity];

(* -------------------------------------------------------------- *)
(* Build long-format rows from one wide row + a fuel→column map.   *)

deFuelMap = {{"E5", "price_e5"}, {"E10", "price_e10"}, {"Diesel", "price_diesel"}};
ukFuelMap = {{"E10", "price_e10"}, {"E5", "price_e5"}, {"Diesel", "price_b7"}};

expandDE[row_Association] := Module[{ts, sid, br, pl, la, lo},
  ts  = toDate @ row["timestamp_utc"];
  sid = row["station_id"];
  br  = row["brand"];
  pl  = row["place"];
  la  = toFloat @ row["lat"];
  lo  = toFloat @ row["lon"];
  Function[pair,
    With[{f = pair[[1]], p = toFloat @ row[pair[[2]]]},
      <|
        "Country" -> "DE", "StationID" -> sid, "Brand" -> br,
        "Lat" -> la, "Lon" -> lo, "City" -> pl, "Postcode" -> Missing[],
        "TimestampUTC" -> ts, "TimeZone" -> "Europe/Berlin",
        "Fuel" -> f, "Price" -> p
      |>
    ]
  ] /@ deFuelMap
];

expandUK[row_Association, k_] := Module[{ts, sid, br, pc, la, lo},
  ts  = toDate @ row["timestamp_utc"];
  sid = row["site_id"];
  br  = row["brand"];
  pc  = row["postcode"];
  la  = toFloat @ row["lat"];
  lo  = toFloat @ row["lon"];
  Function[pair,
    With[{f = pair[[1]], p = toFloat @ row[pair[[2]]]},
      <|
        "Country" -> "UK", "StationID" -> sid, "Brand" -> br,
        "Lat" -> la, "Lon" -> lo, "City" -> Missing[], "Postcode" -> pc,
        "TimestampUTC" -> ts, "TimeZone" -> "Europe/London",
        "Fuel" -> f,
        "Price" -> If[NumberQ[p], p * k, Missing["NotAvailable"]]
      |>
    ]
  ] /@ ukFuelMap
];

(* -------------------------------------------------------------- *)

LoadLocalDE[dir_String] := Module[{files, rows, long},
  files = findPriceCSVs[dir];
  If[files === {}, Return[Dataset[{}]]];
  rows = Catenate @ Map[readCSVRows, files];
  long = Catenate @ Map[expandDE, rows];
  long = Select[long,
    NumberQ[#Price] && Head[#TimestampUTC] === DateObject &];
  Dataset @ long
];

LoadLocalUK[dir_String] := Module[{files, rows, long, k},
  files = findPriceCSVs[dir];
  If[files === {}, Return[Dataset[{}]]];
  k = Settings["PenceToEUR"];
  rows = Catenate @ Map[readCSVRows, files];
  long = Catenate @ Map[expandUK[#, k] &, rows];
  long = Select[long,
    NumberQ[#Price] && Head[#TimestampUTC] === DateObject &];
  Dataset @ long
];

LoadAll[] := Join[
  LoadLocalDE @ Settings["PaderbornRawDE"],
  LoadLocalUK @ Settings["AberdeenRawUK"]
];

StationMaster[ds_Dataset] := Module[{rows},
  rows = Normal[ds];
  Dataset @ DeleteDuplicatesBy[
    KeyTake[#, {"Country", "StationID", "Brand", "Lat", "Lon", "City", "Postcode"}] & /@ rows,
    {#Country, #StationID} &
  ]
];

(* -------------------------------------------------------------- *)
(* Local-time helpers — re-anchor the absolute date to local TZ.   *)

localObject[row_Association] :=
  TimeZoneConvert[row["TimestampUTC"], row["TimeZone"]];

(* First[DateObject] gives the local civil components for that TZ;
   DateList[...] would re-convert to $TimeZone, which we don't want. *)
localComponents[row_Association] := First @ localObject[row];

LocalHour[row_Association] := localComponents[row][[4]];
LocalDate[row_Association] := Take[localComponents[row], 3];
LocalWeekday[row_Association] := DayName[LocalDate[row]] /. {
  Monday -> 1, Tuesday -> 2, Wednesday -> 3, Thursday -> 4,
  Friday -> 5, Saturday -> 6, Sunday -> 7};

IsAfterRule[row_Association] :=
  AbsoluteTime @ row["TimestampUTC"] >=
    AbsoluteTime @ DateObject[{2026, 4, 1}, "Day", TimeZone -> "Europe/Berlin"];

End[];
EndPackage[];
