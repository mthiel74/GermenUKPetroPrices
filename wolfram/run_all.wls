#!/usr/bin/env wolframscript
(* run_all.wls — runs the full empirical pipeline end-to-end.

   Order:
     1. ingest from /Volumes/Lexar (rsync + tidy long-format)
     2. fetch country-wide Tankerkönig snapshot (live API)
     3. fetch Brent + USD/EUR
     4. analyses (cycle, hazard, amplitude, did, day-of-week, spatial,
        cross-border, brent-lag, hotelling, timeseries, animation)
     5. (optional) regenerate gpt-image-1 illustrations
     6. build the community notebook

   Each step is its own wolframscript so failures are localised.
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

steps = {
  (* data *)
  "wolfram/ingest_local.wls",
  (* fetch_tankerkoenig.wls is intentionally NOT here — it must be
     run manually with --i-have-read-the-rules, see its header. *)
  "wolfram/fetch_fuelfinder.wls",
  "wolfram/fetch_brent.wls",
  (* analyses *)
  "wolfram/intraday_cycle.wls",
  "wolfram/headline_figure.wls",
  "wolfram/hazard_12uhr.wls",
  "wolfram/amplitude.wls",
  "wolfram/did_de_uk.wls",
  "wolfram/day_of_week.wls",
  "wolfram/spatial.wls",
  "wolfram/cross_border.wls",
  "wolfram/brent_lag.wls",
  "wolfram/hotelling.wls",
  "wolfram/timeseries_de_uk.wls",
  "wolfram/germany_animation.wls",
  "wolfram/uk_snapshot.wls",
  "wolfram/germany_station_network.wls",
  "wolfram/germany_2019_animation.wls",
  "wolfram/build_2019_video.wls",
  (* illustrations + notebook *)
  "wolfram/generate_petrol_figures.wls",
  "community/build_notebook.wls"
};

skipImageGen = !MemberQ[$ScriptCommandLine, "--with-image-gen"];

Do[
  If[skipImageGen && s == "wolfram/generate_petrol_figures.wls",
    Print["\n[run_all] SKIP ", s, " (pass --with-image-gen to enable)"],
    Print["\n========== ", s, " =========="];
    rc = Run["wolframscript -file " <> s];
    If[rc =!= 0,
      Print["[run_all] ", s, " exited ", rc, " — aborting."];
      Exit[rc]]
  ],
  {s, steps}];

Print["\n[run_all] all steps completed."];
