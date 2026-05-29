#!/usr/bin/env wolframscript
(* Smoke tests for PetrolData loaders. *)

SetDirectory @ FileNameDrop[$InputFileName, -2];
Get["wolfram/PetrolData.wl"];

fail = 0;
check[name_, cond_] := If[TrueQ[cond],
  Print["[OK]   ", name],
  Print["[FAIL] ", name]; fail++];

deSmall = PetrolData`LoadLocalDE[
  "data/raw/local/fuel-data/Paderborn_33102_r10km/aral_aral-tankstelle_paderborn_01a79c0b"];
ukSmall = PetrolData`LoadLocalUK[
  "data/raw/local/uk-fuel-data/Aberdeen_r50km/asda_garth-dee-road-bridge-of-dee-aberdeen-aberdeen_ab10-7qa_gfnkzxjs"];

check["DE loader returns Dataset", Head[deSmall] === Dataset];
check["UK loader returns Dataset", Head[ukSmall] === Dataset];
check["DE has expected schema",
  ContainsAll[Keys @ First @ Normal @ deSmall,
   {"Country","StationID","Brand","Lat","Lon","TimestampUTC","TimeZone","Fuel","Price"}]];
check["UK prices in reasonable EUR/L range",
  With[{p = Normal @ ukSmall[Select[#Fuel == "E10" &], "Price"]},
   And[1.0 < Min[p], Max[p] < 3.0]]];
check["DE prices in reasonable EUR/L range",
  With[{p = Normal @ deSmall[Select[#Fuel == "E5" &], "Price"]},
   And[1.0 < Min[p], Max[p] < 3.0]]];
check["DE LocalHour DST sanity (CEST Sep → UTC+2)",
  With[{r = First @ Normal @ deSmall},
   PetrolData`LocalHour[r] == Mod[First[r["TimestampUTC"]][[4]] + 2, 24]]];
check["IsAfterRule classifies pre-2026-04-01 as False",
  ! PetrolData`IsAfterRule[First @ Normal @ deSmall]];
check["IsAfterRule classifies post-2026-04-01 as True",
  PetrolData`IsAfterRule[Last  @ Normal @ deSmall]];
check["Three fuel types present in DE rows",
  Length @ Union @ Normal @ deSmall[All, "Fuel"] == 3];

If[fail == 0,
  Print["\nAll tests passed."],
  Print["\n", fail, " test(s) failed."]; Exit[1]
];
