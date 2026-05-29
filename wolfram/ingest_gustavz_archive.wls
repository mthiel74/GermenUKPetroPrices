#!/usr/bin/env wolframscript
(* ingest_gustavz_archive.wls — parse the November 2019 daily price
   events into a tidy long-format daily-per-station table joined to
   station coordinates.

   Each input file looks like
       date,station_uuid,diesel,e5,e10,dieselchange,e5change,e10change
   with one row per price-change event. We aggregate to one row per
   (date, station) holding mean(diesel), mean(e5), mean(e10) and event
   counts over the day. Stations with zero events on a given day are
   omitted; downstream kriging will interpolate them spatially. We do
   NOT forward-fill across days (~95% of stations report >=1 event
   per day in the 2019 data so the gap is small).

   Output:
     data/tk_archive_2019_11_daily.csv
       columns: Date,StationID,Lat,Lon,Brand,Postcode,
                DieselMean,E5Mean,E10Mean,EventCount
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

(* 1. Load station master *)
Print["[ingest] loading stations master ..."];
stations = Import["data/raw/gustavz/stations.csv", "Dataset", "HeaderLines" -> 1];
stationsByID = Association[
  (#uuid -> <|"Lat" -> #latitude, "Lon" -> #longitude,
              "Brand" -> #brand, "Postcode" -> #post_code|>) & /@ Normal[stations]];
Print["[ingest]   ", Length[stationsByID], " stations"];

(* 2. Process each daily file *)
processDay[filePath_String] := Module[{rows, byStation, dayStr},
  Print["[ingest] ", FileBaseName[filePath]];
  rows = Import[filePath, "CSV", "HeaderLines" -> 1, "Numeric" -> True];
  dayStr = StringTake[FileBaseName[filePath], 10];   (* "2019-11-01" *)
  byStation = GroupBy[rows, #[[2]] &];
  Map[
    Function[uuid,
      With[{events = byStation[uuid],
            meta = Lookup[stationsByID, uuid, <||>]},
        <|
          "Date"       -> dayStr,
          "StationID"  -> uuid,
          "Lat"        -> Lookup[meta, "Lat", Missing[]],
          "Lon"        -> Lookup[meta, "Lon", Missing[]],
          "Brand"      -> Lookup[meta, "Brand", ""],
          "Postcode"   -> Lookup[meta, "Postcode", ""],
          "DieselMean" -> N @ Mean @ Select[events[[All, 3]], NumberQ[#] && # > 0.5 &],
          "E5Mean"     -> N @ Mean @ Select[events[[All, 4]], NumberQ[#] && # > 0.5 &],
          "E10Mean"    -> N @ Mean @ Select[events[[All, 5]], NumberQ[#] && # > 0.5 &],
          "EventCount" -> Length[events]
        |>]],
    Keys[byStation]
  ]];

files = Sort @ FileNames["*.csv", "data/raw/gustavz/prices/2019/11"];
Print["[ingest] ", Length[files], " daily files to process"];

allRows = Catenate @ Map[processDay, files];

(* Drop rows whose station coords are missing or whose diesel mean is
   not numeric *)
clean = Select[allRows,
  NumberQ[#Lat] && NumberQ[#Lon] && NumberQ[#DieselMean] &];
Print["[ingest] cleaned: ", Length[clean], " (station, day) rows"];

Export["data/tk_archive_2019_11_daily.csv", Dataset[clean], "CSV"];
Print["[ingest] wrote data/tk_archive_2019_11_daily.csv (",
      FileByteCount["data/tk_archive_2019_11_daily.csv"]/1024^2 // Round, " MB)"];

(* Print a quick sanity summary *)
diesels = #DieselMean & /@ clean;
Print["[ingest] diesel daily-mean quantiles (2/50/98 %): ",
      Quantile[diesels, {0.02, 0.5, 0.98}]];
Print["[ingest] distinct stations:", Length @ Union[#StationID & /@ clean]];
Print["[ingest] distinct dates:", Length @ Union[#Date & /@ clean]];
