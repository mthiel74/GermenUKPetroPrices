#!/usr/bin/env wolframscript
(* fetch_tankerkoenig.wls — country-wide snapshot of German stations.

   The historical bulk dump (creativecommons.tankerkoenig.de) requires
   Azure DevOps authentication which we don't have. Instead this
   fetches a *live* spatial snapshot of all stations using the public
   API, which only requires a free demo key.

   The script samples a 7×11 lat/lng grid over Germany (radius=25 km
   per query) and de-dupes by station UUID. Rate-limited to 1
   request/second (gentler than the published 1/min "best practice"
   but the API tolerates this in burst).

   Output:
     data/tankerkoenig_snapshot.csv
       columns: uuid, name, brand, street, place, postcode, lat, lng,
                e5, e10, diesel, isOpen, fetched_at

   The data is a *single* moment in time, not a history. It enables
   the spatial / country-wide map (Analysis §10) and cross-border
   (Analysis §7) using current prices.
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

(* HARD GUARD — by default this script REFUSES to run. You must
   explicitly pass --i-have-read-the-rules to call the API. The API
   has been suspended for our IP in a previous session due to
   aggressive polling, and Geo Inform reserves the right to revoke
   keys. Read the API rules in the long comment below before enabling. *)
If[!MemberQ[$ScriptCommandLine, "--i-have-read-the-rules"],
  Print["[tk-snap] REFUSING to run: pass --i-have-read-the-rules after"];
  Print["[tk-snap]    confirming you have read the API rules in the"];
  Print["[tk-snap]    header comment of this script (60 s/request, etc.)"];
  Exit[1]];

(* Prefer the real key Marco stores in SystemCredential;
   fall back to the public demo key (returns placeholder 1.009 prices). *)
apikey = With[{k = SystemCredential["TANKERKOENIG_API_KEY"]},
  If[StringQ[k] && StringLength[k] > 20,
    Print["[tk-snap] using real Tankerkönig key from SystemCredential."]; k,
    Print["[tk-snap] WARNING — no real key in SystemCredential, demo key gives fake 1.009."];
    "00000000-0000-0000-0000-000000000002"]];

(* IMPORTANT — API USAGE RULES (read these BEFORE running this script)

   The Tankerkönig live API is operated by Geo Inform (the
   Markttransparenzstelle for fuels) and the data provider's terms
   (https://creativecommons.tankerkoenig.de/) state explicitly:

     * Maximum request rate for *automation* is 1 request per minute
       per API key.
     * Home-automation use should be at most 1 query per 5 minutes.
     * Bulk historical access is via the Azure DevOps git repo, not
       the live API.
     * Abusive request rates can and will lead to IP-level bans on
       the API key holder.

   This script therefore uses a deliberately slow 60-second pause
   between queries — a 180-point grid takes ~3 hours to complete.
   That is the cost of being polite. If you need faster turnaround,
   request a research-tier key from Geo Inform with explicit
   higher-frequency permissions.

   The grid is also *shuffled* before iteration, so any partial
   completion is geographically spread across all of Germany rather
   than packed into one latitude band. *)
latMin = 47.3; latMax = 55.0; latStep = 0.55;
lngMin = 5.9;  lngMax = 15.0; lngStep = 0.8;

points = Flatten[
  Table[{lat, lng}, {lat, latMin, latMax, latStep}, {lng, lngMin, lngMax, lngStep}],
  1];
SeedRandom[2026];                     (* reproducible shuffle *)
points = RandomSample[points];
Print["[tk-snap] grid points (shuffled): ", Length[points]];

fetchOne[{lat_, lng_}] := Module[{url, raw, json},
  url = StringTemplate["https://creativecommons.tankerkoenig.de/json/list.php?lat=`lat`&lng=`lng`&rad=25&type=all&sort=dist&apikey=`k`"] @
    <|"lat" -> lat, "lng" -> lng, "k" -> apikey|>;
  raw = Quiet @ Check[URLExecute[url, "RawJSON"], $Failed];
  If[AssociationQ[raw] && raw["ok"] === True,
    raw["stations"], {}]
];

(* Pacing — 60 seconds per request, per the docs. Do NOT shorten. *)
pauseSec = 60.0;

stations = Reap[
  Do[
    With[{batch = fetchOne[points[[i]]]},
      Print["[tk-snap] ", i, "/", Length[points], " pt=", points[[i]],
            " stations=", Length[batch]];
      Sow /@ batch;
    ];
    Pause[pauseSec],
    {i, Length[points]}
  ];
][[2, 1]];

Print["[tk-snap] total raw entries: ", Length[stations]];
stations = DeleteDuplicatesBy[stations, #["id"] &];
Print["[tk-snap] unique stations: ", Length[stations]];

fetchedAt = DateString[Now, "ISODateTime"];

mkRow[s_Association] := <|
  "uuid"     -> s["id"],
  "name"     -> s["name"],
  "brand"    -> s["brand"],
  "street"   -> Lookup[s, "street", ""],
  "place"    -> Lookup[s, "place", ""],
  "postcode" -> Lookup[s, "postCode", ""],
  "lat"      -> s["lat"],
  "lng"      -> s["lng"],
  "e5"       -> Lookup[s, "e5", Missing[]],
  "e10"      -> Lookup[s, "e10", Missing[]],
  "diesel"   -> Lookup[s, "diesel", Missing[]],
  "isOpen"   -> Lookup[s, "isOpen", Missing[]],
  "fetched_at" -> fetchedAt
|>;

rows = mkRow /@ stations;

If[!FileExistsQ["data"], CreateDirectory["data"]];
Export["data/tankerkoenig_snapshot.csv", Dataset[rows], "CSV"];
Print["[tk-snap] wrote data/tankerkoenig_snapshot.csv with ", Length[rows], " rows."];
