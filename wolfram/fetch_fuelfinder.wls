#!/usr/bin/env wolframscript
(* fetch_fuelfinder.wls — UK retailer fuel-price snapshot.

   The statutory Fuel Finder gateway (gov.uk) requires an access
   token; the original 2023 CMA voluntary scheme exposed retailer
   JSON feeds directly, several of which still serve as of May 2026:

     Asda, Morrisons, Esso, Jet, Applegreen, Sainsbury's.

   This script polls each, normalises the schema (postcode, lat/lon,
   E10/B7 in pence), converts to EUR/L, and writes a single CSV.

   Output:
     data/uk_fuelfinder_snapshot.csv
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

GBPEUR = 1.15;
penceToEUR = GBPEUR / 100.;

retailers = <|
  "Asda"        -> "https://storelocator.asda.com/fuel_prices_data.json",
  "Morrisons"   -> "https://www.morrisons.com/fuel-prices/fuel.json",
  "Esso"        -> "https://fuelprices.esso.co.uk/latestdata.json",
  "Jet"         -> "https://jetlocal.co.uk/fuel_prices_data.json",
  "Applegreen"  -> "https://applegreenstores.com/fuel-prices/data.json",
  "Sainsburys"  -> "https://api.sainsburys.co.uk/v1/exports/latest/fuel_prices_data.json"
|>;

fetchRetailer[name_, url_] := Module[{raw, json, stations},
  Print["[ff] ", name, " ..."];
  raw = Quiet @ Check[URLExecute[url, "RawJSON"], $Failed];
  If[raw === $Failed || !AssociationQ[raw],
    Print["[ff]   FAIL ", name]; Return[{}]];
  stations = Lookup[raw, "stations", {}];
  Print["[ff]   ", name, " stations: ", Length[stations]];
  Map[
    Function[s,
      <|
        "site_id"   -> Lookup[s, "site_id", ""],
        "brand"     -> Lookup[s, "brand", name],
        "address"   -> Lookup[s, "address", ""],
        "postcode"  -> Lookup[s, "postcode", ""],
        "lat"       -> Lookup[Lookup[s, "location", <||>], "latitude", Missing[]],
        "lng"       -> Lookup[Lookup[s, "location", <||>], "longitude", Missing[]],
        "price_e10_pence" -> Lookup[Lookup[s, "prices", <||>], "E10", Missing[]],
        "price_b7_pence"  -> Lookup[Lookup[s, "prices", <||>], "B7", Missing[]],
        "price_e10_eur"   -> With[{p = Lookup[Lookup[s, "prices", <||>], "E10", Missing[]]},
                                  If[NumberQ[p], p * penceToEUR, Missing[]]],
        "price_b7_eur"    -> With[{p = Lookup[Lookup[s, "prices", <||>], "B7", Missing[]]},
                                  If[NumberQ[p], p * penceToEUR, Missing[]]],
        "last_updated"    -> Lookup[raw, "last_updated", ""],
        "retailer_source" -> name
      |>],
    stations
  ]
];

allStations = Catenate[
  KeyValueMap[fetchRetailer, retailers]];
allStations = Select[allStations,
  NumberQ[#["price_b7_eur"]] && NumberQ[#["lat"]] && NumberQ[#["lng"]] &];

Print["[ff] total UK stations with valid B7 price: ", Length[allStations]];
Print["[ff] B7 price range (EUR/L): ",
      MinMax[#["price_b7_eur"] & /@ allStations]];
Print["[ff] by retailer: ",
      Normal @ Counts[#["retailer_source"] & /@ allStations]];

If[Length[allStations] > 0,
  Export["data/uk_fuelfinder_snapshot.csv", Dataset[allStations], "CSV"];
  Print["[ff] wrote data/uk_fuelfinder_snapshot.csv"],
  Print["[ff] no data — skipping export"]];
