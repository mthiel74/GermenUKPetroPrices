#!/usr/bin/env wolframscript
(* fetch_gustavz_2019.wls — download the November 2019 Tankerkönig
   archive plus the stations master list from the public gustavz
   GitHub mirror.

   Source:
     https://github.com/gustavz/tankerkoenig_dataset
   Original data:
     Tankerkönig / Markttransparenzstelle für Kraftstoffe (Bundeskartellamt).
   License:
     CC BY-NC-SA 4.0 (attribution + non-commercial + ShareAlike).

   Why November 2019?  The gustavz mirror contains exactly one complete
   country-wide month (1-30 Nov 2019, ~590 MB) plus one partial day
   (1 Dec 2019). It's the only public country-wide window we can use
   without registering for the official bulk archive at
   onboarding.tankerkoenig.de.

   This script makes ~31 HTTPS GET requests to raw.githubusercontent.com.
   It does NOT call the Tankerkönig live API.

   Output:
     data/raw/gustavz/stations.csv
     data/raw/gustavz/prices/2019/11/2019-11-DD-prices.csv  (DD = 01..30)
     data/raw/gustavz/prices/2019/12/2019-12-01-prices.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

baseURL = "https://raw.githubusercontent.com/gustavz/tankerkoenig_dataset/master/data";
outRoot = "data/raw/gustavz";

If[!FileExistsQ[outRoot], CreateDirectory[outRoot, CreateIntermediateDirectories -> True]];

downloadIfMissing[url_String, dest_String] := Module[{},
  If[FileExistsQ[dest],
    Print["[gustavz] skip (exists): ", FileNameTake[dest]];
    Return[Null]];
  If[!FileExistsQ[DirectoryName[dest]],
    CreateDirectory[DirectoryName[dest], CreateIntermediateDirectories -> True]];
  Print["[gustavz] fetching ", FileNameTake[dest], " ..."];
  result = Quiet @ Check[URLDownload[url, dest], $Failed];
  If[result === $Failed,
    Print["[gustavz]   FAIL ", url],
    Print["[gustavz]   OK ",  FileByteCount[dest], " bytes"]
  ]];

(* 1. stations master *)
downloadIfMissing[
  baseURL <> "/stations/stations.csv",
  FileNameJoin[{outRoot, "stations.csv"}]];

(* 2. Full Nov 2019 *)
Do[
  With[{dd = IntegerString[d, 10, 2]},
    downloadIfMissing[
      baseURL <> "/prices/2019/11/2019-11-" <> dd <> "-prices.csv",
      FileNameJoin[{outRoot, "prices", "2019", "11", "2019-11-" <> dd <> "-prices.csv"}]
    ]],
  {d, 1, 30}];

(* 3. The single Dec 2019 file (mirror only has one day) *)
downloadIfMissing[
  baseURL <> "/prices/2019/12/2019-12-01-prices.csv",
  FileNameJoin[{outRoot, "prices", "2019", "12", "2019-12-01-prices.csv"}]];

Print["[gustavz] done."];
Print["[gustavz] disk used: ",
  Round[DirectorySize[outRoot] / 1024^2], " MB"];

(* Local DirectorySize helper *)
DirectorySize[d_String] := Total @ FileByteCount /@ FileNames["*", d, Infinity];
