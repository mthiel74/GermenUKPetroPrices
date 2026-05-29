#!/usr/bin/env wolframscript
(* generate_petrol_figures.wls — Wolfram-native gpt-image-2 generator.

   Generates five editorial illustrations for the notebook via the
   OpenAI Image Generation API. Uses SystemCredential["OPENAI_API_KEY"].

   Output: docs/images/figures-generated/*.png  (committed)
   This script is one-off — the PNGs are kept in the repository so a
   normal rebuild does not need the OpenAI key.

   Usage:
     wolframscript -file wolfram/generate_petrol_figures.wls
     wolframscript -file wolfram/generate_petrol_figures.wls --force
       (--force regenerates even if PNG already exists)
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

force = MemberQ[$ScriptCommandLine, "--force"];
outDir = "docs/images/figures-generated";
If[!FileExistsQ[outDir], CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

key = SystemCredential["OPENAI_API_KEY"];
If[!StringQ[key] || StringLength[key] < 20,
  Print["[img] OPENAI_API_KEY not found in SystemCredential. Aborting."];
  Exit[1]];
Print["[img] OpenAI key loaded (length ", StringLength[key], ")"];

prompts = <|
  "intraday_cycle_motif.png" ->
    "Two stacked editorial line charts comparing the intraday price " <>
    "cycle of a German filling station BEFORE and AFTER a regulatory " <>
    "rule. Both charts share the same x-axis labeled 00:00 - 06:00 - " <>
    "12:00 - 18:00 - 24:00 with tick marks at every six hours. " <>
    "TOP CHART (title 'BEFORE 12-Uhr-Regel'): a smooth line in teal " <>
    "blue rising slowly from midnight, then a SHARP MORNING PEAK at " <>
    "07:00 marked '+10 c/L', then gradual decline through the day " <>
    "reaching a trough near 21:00. The morning peak is the highest " <>
    "point. " <>
    "BOTTOM CHART (title 'AFTER 12-Uhr-Regel'): a flat early-morning " <>
    "line low and steady, then a VERY TALL VERTICAL JUMP exactly at " <>
    "12:00 (the dominant feature of this chart, much taller than " <>
    "anything in the top chart), highlighted with an ember-orange " <>
    "vertical bar and labeled '12:00 +8 c/L'. After the jump, the " <>
    "line slowly decays through the afternoon. " <>
    "Clean information-design aesthetic, muted teal-and-ember " <>
    "palette, slate grey gridlines, no photographs, no logos, no real " <>
    "data values beyond the two labels mentioned.",
  "station_network_map.png" ->
    "Stylized topographic illustration of a network of filling " <>
    "stations across Germany. Each station is a small filled dot, " <>
    "colored on a divergent scale from cool blue (low price) to warm " <>
    "ember (high price). Subtle base map showing the German country " <>
    "outline only. Thin connecting lines suggest spatial " <>
    "autocorrelation between nearby stations. Top-banner reads " <>
    "'spatial autocorrelation of pump prices'. Information-design " <>
    "aesthetic. No real text labels. No photo realism.",
  "cross_border_arbitrage.png" ->
    "Schematic infographic cross-section showing a European border. " <>
    "Three filling stations on the German side, three on the " <>
    "neighbouring side (suggest Luxembourg/France via flags). Above " <>
    "each station a small EUR/L price tag, with German tags visibly " <>
    "higher than the foreign tags. An arc of car icons flows across " <>
    "the border toward the cheaper side. Subtle road, border post, " <>
    "two unlabelled country flags. Editorial muted palette, no brand " <>
    "logos, no photo realism.",
  "brent_pump_lag.png" ->
    "Scientific figure-style schematic of two stacked time-series " <>
    "plots. Top trace is Brent crude oil price with a sudden upward " <>
    "spike then partial recovery. Bottom trace is retail pump diesel " <>
    "price with a *delayed* and *asymmetric* response — rising " <>
    "quickly to a higher peak, then falling slowly. Annotations: " <>
    "'fast rise' on the upward slope and 'slow fall' on the way down, " <>
    "illustrating the 'rockets and feathers' effect. Plot-on-grid " <>
    "look, muted teal/ember palette.",
  "hotelling_rule.png" ->
    "Economics-textbook style illustration of Hotelling's 1931 rule " <>
    "for exhaustible resources. A clean upward-sloping curve showing " <>
    "real oil price rising at the real interest rate over time. " <>
    "Below it, a second noisier curve labelled 'observed retail " <>
    "price' that diverges substantially — spiking on geopolitical " <>
    "events, flat for long stretches. Annotated to indicate the " <>
    "Hotelling path is the theoretical ideal, the observed path the " <>
    "messy reality. Editorial science infographic, no photographs, " <>
    "slate and ember palette."
|>;

generateOne[filename_, prompt_] := Module[{target, response, b64},
  target = FileNameJoin[{outDir, filename}];
  If[FileExistsQ[target] && !force,
    Print["[img] skip ", filename, " (exists)"]; Return[Null]];
  Print["[img] generating ", filename, " ..."];
  response = URLExecute[
    HTTPRequest[
      "https://api.openai.com/v1/images/generations",
      <|"Method" -> "POST",
        "Headers" -> {
          "Authorization" -> "Bearer " <> key,
          "Content-Type"  -> "application/json"},
        "Body" -> ExportString[
          <|"model" -> "gpt-image-1",
            "prompt" -> prompt,
            "size" -> "1536x1024",
            "n" -> 1|>,
          "RawJSON"]
      |>],
    "RawJSON"];
  If[AssociationQ[response] && KeyExistsQ[response, "data"],
    b64 = response["data"][[1]]["b64_json"];
    BinaryWrite[target, BaseDecode[b64]];
    Close[target];
    Print["[img]   wrote ", target, "  (", FileByteCount[target], " bytes)"],
    Print["[img]   ERROR response: ", Short[response, 6]]
  ]
];

KeyValueMap[generateOne, prompts];

Print["[img] all done."];
