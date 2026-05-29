#!/usr/bin/env wolframscript
(* fetch_brent.wls — daily Brent spot price (EIA RBRTE) + USD/EUR
   (FinancialData), written to data/brent_daily.csv.

   Columns: Date, BrentUSDPerBbl, USDperEUR, BrentEURPerL
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

barrelsPerLitre = 1./158.987;

(* --- 1. Brent spot via EIA RBRTE Excel --- *)
eiaURL = "https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls";
Print["[brent] downloading EIA RBRTE ..."];
xls = Import[eiaURL, "XLS"];
sheet = xls[[2]];   (* sheet 2 has the data *)
(* Skip the first 3 header rows *)
rawData = Select[sheet[[4 ;;]], MatchQ[#, {_DateObject, _?NumberQ}] &];
Print["[brent] EIA rows: ", Length[rawData]];

brentRows = Map[
  Function[r,
    <|"Date" -> DateString[r[[1]], "ISODate"],
      "BrentUSDPerBbl" -> r[[2]]|>],
  rawData];

(* --- 2. USD/EUR from FinancialData --- *)
Print["[brent] FinancialData USD/EUR ..."];
fxTD = Quiet @ FinancialData["USD/EUR", "Close", {2024, 1, 1}, "Day"];
fxRows = If[Head[fxTD] === TemporalData && Length[fxTD["Path"]] > 0,
  Map[
    Function[pt,
      With[{v = Quiet @ QuantityMagnitude[pt[[2]]]},
        If[NumberQ[v] && v > 0,
          <|"Date" -> DateString[AbsoluteTime[pt[[1]]], "ISODate"],
            "USDperEUR" -> v|>,
          Nothing]]],
    fxTD["Path"]],
  {}];
fxRows = Cases[fxRows, _Association];
Print["[brent] FX rows: ", Length[fxRows]];

(* --- 3. Merge on Date and compute EUR/L --- *)
fxByDate = Association[#Date -> #USDperEUR & /@ fxRows];
Print["[brent] fxByDate sample keys: ", Take[Keys[fxByDate], UpTo[3]],
      " ... ", Take[Keys[fxByDate], -3]];
Print["[brent] brentRows sample dates: ",
      Take[brentRows[[All, Key["Date"]]], UpTo[3]], " ... ",
      Take[brentRows[[All, Key["Date"]]], -3]];

(* Diagnose merge step *)
testR = brentRows[[-2]];
Print["[brent] test row: ", testR, "  Head: ", Head[testR]];
Print["[brent]   r['Date']: ", testR["Date"]];
Print["[brent]   lookup: ", Lookup[fxByDate, testR["Date"], "MISS"]];

combined = Map[
  Function[r,
    With[{usdEur = Lookup[fxByDate, r["Date"], Missing[]]},
      If[NumberQ[usdEur],
        Append[r, {"USDperEUR" -> usdEur,
          "BrentEURPerL" -> r["BrentUSDPerBbl"] * usdEur * barrelsPerLitre}],
        Nothing]]],
  brentRows];
Print["[brent] post-map length: ", Length[combined]];
Print["[brent] first non-Nothing: ", SelectFirst[combined, Head[#] === Association &, "NONE"]];
combined = Select[combined, Head[#] === Association &];

(* Truncate to overlapping window 2024-01-01 onwards *)
combined = Select[combined, OrderedQ[{"2024-01-01", #Date}] &];

Print["[brent] combined rows: ", Length[combined]];
Export["data/brent_daily.csv", Dataset[combined], "CSV"];
Print["[brent] last 3: ", Take[combined, -3]];
