# Project notes for Claude

## Purpose
Empirical study of German + British retail petrol prices. Headline
natural experiment: the German **12-Uhr-Regel** that took effect on
**1 April 2026** in response to the April-2026 Iran war and crude
spike. Output is a **Wolfram Community** post (notebook in
`community/`).

## Mirrors the structure of
- `~/Documents/GitHub/Contiguous-Cartograms/`
- `~/Documents/GitHub/ENSO-emergence/`

Pure Wolfram Language pipeline. Python only for `gpt-image-2`
illustrations (`docs/images/figures-generated/generate_petrol_figures.py`).
Five educational illustrations: intraday-cycle motif, station-network
map, cross-border arbitrage, Brent-response lag, Hotelling-rule
illustration. PNGs are committed; the script is one-off.

## Local data (mac-mini, Tailscale)
- `ssh thiel@100.102.204.52` reaches `marcos-mac-mini.local`.
- `/Volumes/Lexar/fuel-data/Paderborn_33102_r10km/` — 29 German
  stations, one `prices.csv` per station, ~7900 rows each,
  2025-09-14 → present (~30 readings/day).
  Columns: `timestamp_utc,timestamp_local,station_id,station_name,brand,place,lat,lon,dist_km,is_open,price_diesel,price_e5,price_e10`.
- `/Volumes/Lexar/uk-fuel-data/Aberdeen_r50km/` — 36 UK stations,
  one `prices.csv` per station, ~8000 rows each.
  Columns: `timestamp_utc,timestamp_local,retailer,site_id,brand,address,postcode,lat,lon,dist_km,price_e10,price_e5,price_b7,price_sdv`.
  UK prices are in pence; German in EUR.

The `wolfram/ingest_local.wls` script rsyncs both trees into
`data/raw/local/` and produces tidy long-format Parquet/CSV in
`data/`.

## Country-wide coverage
The Paderborn 10 km radius is fine for the intraday-cycle and
12-Uhr-Regel analyses, but the *country-wide animated GeoGraphics*
needs every German station. That comes from the Tankerkönig
historical bulk dump (CC BY-NC-SA 4.0, ~65 GB total). For our window
(2025-09 → 2026-05) we sparse-checkout only the relevant `prices/YYYY/MM/`
and `stations/YYYY/MM/` directories.

## 12-Uhr-Regel — what to test
1. **Histogram of intra-day price-change times** before vs after
   2026-04-01. Pre: should be roughly uniform with 3 peaks
   (morning, midday, evening). Post: should concentrate at 12:00.
2. **Daily amplitude** `max - min` per station per day. Did the
   amplitude shrink, grow, or stay the same?
3. **Difference-in-differences** with UK as the control:
   `E[p_diesel | DE, post] - E[p_diesel | DE, pre]`
   minus the equivalent UK contrast, with Brent and time-of-week
   controls.

## Brent + wholesale
Use the EIA Brent daily spot series (`RBRTE`) — it's free, machine
readable. Rotterdam ARA gasoline / diesel wholesale prices are ideal
but commercial; for the notebook we fall back to a Spreadsheet
S&P Global excerpt or note the gap.

## License footers
Every figure caption ends with: *Data: Tankerkönig (CC BY-NC-SA 4.0);
UK Fuel Finder (OGL v3.0); EIA Brent.*

## Don't
- Don't commit `data/raw/` (it's git-ignored).
- Don't put OpenAI keys anywhere except the user's shell environment.
- Don't claim a finding without showing the regression / plot it came
  from.
