#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ingest_local.wls
   Rsync the petrol-price feeds from the mac-mini Lexar drive into
   ./data/raw/local/, then load them and write a tidy long-format
   Parquet+CSV pair under ./data/.

   Usage:
     wolframscript -file wolfram/ingest_local.wls           (* default: refresh *)
     wolframscript -file wolfram/ingest_local.wls --skip-rsync   (* load only *)
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];   (* repo root *)

skipRsync = MemberQ[$ScriptCommandLine, "--skip-rsync"];

ProjectRoot = Directory[];
RawDE = FileNameJoin[{ProjectRoot, "data/raw/local/fuel-data"}];
RawUK = FileNameJoin[{ProjectRoot, "data/raw/local/uk-fuel-data"}];

If[!FileExistsQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] & /@ {RawDE, RawUK};

If[!skipRsync,
  Print["[ingest] rsync DE fuel-data ..."];
  Run["rsync -a --delete thiel@100.102.204.52:/Volumes/Lexar/fuel-data/ "
        <> RawDE <> "/ > " <> FileNameJoin[{ProjectRoot, "data/raw/local/rsync_de.log"}] <> " 2>&1"];
  Print["[ingest] rsync UK uk-fuel-data ..."];
  Run["rsync -a --delete thiel@100.102.204.52:/Volumes/Lexar/uk-fuel-data/ "
        <> RawUK <> "/ > " <> FileNameJoin[{ProjectRoot, "data/raw/local/rsync_uk.log"}] <> " 2>&1"];
  Print["[ingest] rsync done."];
];

Print["[ingest] loading PetrolData package ..."];
Get @ FileNameJoin[{ProjectRoot, "wolfram/PetrolData.wl"}];

Print["[ingest] reading DE long table ..."];
deLong = PetrolData`LoadLocalDE[RawDE];
Print["[ingest]   ", Length[deLong], " rows"];

Print["[ingest] reading UK long table ..."];
ukLong = PetrolData`LoadLocalUK[RawUK];
Print["[ingest]   ", Length[ukLong], " rows"];

allLong = Join[deLong, ukLong];

Print["[ingest] writing data/local_long.csv ..."];
Export[FileNameJoin[{ProjectRoot, "data/local_long.csv"}], allLong, "CSV"];

Print["[ingest] writing data/stations.csv ..."];
Export[FileNameJoin[{ProjectRoot, "data/stations.csv"}],
       Dataset[PetrolData`StationMaster[allLong]], "CSV"];

Print["[ingest] done. n_rows=", Length[allLong]];
