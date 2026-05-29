# German + British Petrol Prices — an Empirical Study

A live, reproducible study of retail petrol-and-diesel prices in
**Germany** (Tankerkönig, minute-resolution, ~16 000 stations) and the
**United Kingdom** (CMA Fuel Finder / pump-watch, post-2023 daily
retailer feeds), built around three natural experiments:

1. **The German 12-Uhr-Regel** — on **1 April 2026**, in response to
   the April-2026 US-Iran war and the Strait-of-Hormuz crude spike,
   Germany made it illegal for filling stations to *raise* prices more
   than once per working day, at 12:00 local. Our locally collected
   Paderborn data straddles this rule (2025-09 → 2026-05), so before /
   after is a clean comparison.
2. **The intraday cycle** — German pump prices traditionally show a
   three-peak, four-trough daily pattern with ~25 c/L amplitude. The
   12-Uhr-Regel was designed to flatten this. Did it?
3. **DE vs UK** — the UK retail-fuel market is structurally different
   (supermarket-led, daily revision, no minute-resolution price wars).
   We document the gap.

Inspired by, and structured after,
[Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms)
and [ENSO-emergence](https://github.com/mthiel74/ENSO-emergence) —
pure Wolfram Language pipeline with a buildable Wolfram-Community
notebook, plus optional Python only for `gpt-image-2` educational
illustrations.

## What the project does

1. **Ingests the locally collected data** from `/Volumes/Lexar/`
   (Paderborn 33102, 10 km radius, 29 stations; Aberdeen, 50 km, 36
   stations) — 30 readings/day each, Sep 2025 → May 2026.
2. **Fetches the Tankerkönig historical CSV dump**
   (`creativecommons.tankerkoenig.de`, CC BY-NC-SA 4.0) and the live
   UK **Fuel Finder** statutory feed
   (`fuel-finder.service.gov.uk`, OGL v3.0) for country-wide coverage
   and longer histories.
3. **Pulls Brent and Rotterdam wholesale prices** from EIA / ICE for
   the supply-shock identification.
4. **Computes** the canonical estimands:
   - Three-peak intraday cycle (E5, E10, diesel, all hours)
   - Day-of-week effects
   - Spatial autocorrelation (Moran's *I*, Geary's *C*, semivariogram)
   - Cross-border arbitrage near Aachen / Strasbourg / Basel
   - Response lag to Brent ($\beta_k$ in
     $\Delta p_t = \sum_k \beta_k \Delta b_{t-k} + \varepsilon_t$)
   - Hotelling's rule test on the long-run real-price drift
   - **12-Uhr-Regel difference-in-differences** with UK as the control
5. **Produces a country-wide animated GeoGraphics** of the price field
   evolving across Germany week by week.
6. **Generates educational illustrations** via `gpt-image-2`
   (committed PNGs; the script is one-off).
7. **Builds the Wolfram-Community notebook** in `community/`.

## Repository layout

| path | content |
| --- | --- |
| `wolfram/fetch_*.wls` | data fetchers (Tankerkönig bulk, Fuel Finder live, Brent, station master) |
| `wolfram/ingest_local.wls` | rsync-and-load for `/Volumes/Lexar/fuel-data` and `/Volumes/Lexar/uk-fuel-data` |
| `wolfram/*.wls` | analyses + figure renderers |
| `wolfram/*.wl` | shared packages (loaded by the scripts) |
| `data/` | tidy CSV / JSON output of the fetchers — **not committed**: the underlying price feeds are licensed for non-commercial use by their providers and are not ours to redistribute. Regenerate locally with the `fetch_*`/`ingest_*` scripts (see Data sources below). |
| `data/raw/` | bulk raw downloads (git-ignored) |
| `wolfram/Kriging.wl` | general ordinary-kriging package (pluggable distance, variogram-model choice, kriging variance, smooth `KrigedGeoGraphics` render) |
| `community/build_notebook.wls` | builds `community/german_uk_petrol_prices.nb` |
| `community/build_bundle.wls` | concatenates the reusable packages into the single uploadable `community/PetrolPrices.wl` |
| `community/PetrolPrices.wl` | **generated** one-file library uploaded alongside the notebook (PetrolData\` + Kriging\`) |
| `docs/images/` | figures referenced from the notebook + README |
| `docs/images/figures-generated/` | gpt-image-1 illustrations (PNGs committed) |
| `wolfram/generate_petrol_figures.wls` | one-off Wolfram regenerator (uses `SystemCredential["OPENAI_API_KEY"]`) |
| `tests/` | shape & invariant checks |

## The empirical headlines

### Headline 1 — The 12-Uhr-Regel inverted the cycle, it didn't flatten it

The German rule (effective 2026-04-01) prohibited price *increases*
except once per working day at 12:00. The result, visible in
`docs/images/intraday_cycle_prepost_1d.png`:

| Period | Cycle shape (workday) |
| --- | --- |
| **Pre 12-Uhr-Regel** | Sharp morning rise at 07:00 (+10 c/L for diesel), then steady erosion through the day to a small evening trough at ~21:00. |
| **Post 12-Uhr-Regel** | Morning *trough* at 07:00, then a sharp jump at 12:00 (+8 c/L), followed by exponential decay back through the afternoon. |

In `docs/images/hazard_rate_prepost.png` the price-*up* hazard
collapses post-rule into a single 280-events/station/hour spike at
12:00 (~10× higher than the next-busiest minute). Compliance with the
12-Uhr-Regel is near-total in Paderborn. The cycle itself, however,
is not gone — it has been time-shifted from a morning-led cycle into
a noon-led one.

### Other headlines (each backed by a run, plot, and CSV)

| § | Finding | Number | Figure |
| --- | --- | --- | --- |
| 3 | Daily amplitude (max−min) median **18 → 15 c/L**, Cliff's δ = 0.28, Mann-Whitney p < 10⁻⁴⁹; bootstrap 95 % CI on shift [3.0, 4.0] c/L | -3 c/L | `amplitude_prepost.png` |
| 4 | DiD coefficient (DE × Post, UK as control) **τ = −1.34 c/L** (95 % CI [−2.36, −0.32]), p ≈ 0.01 — DE rose less than UK over the same shock | -1.34 c/L | `did_de_uk.png` |
| 5 | DE day-of-week effect: Mon +2.4 c/L, midweek trough; UK: weekend slightly above mean, otherwise flat | weekday effect ≤ 0.03 c/L | `day_of_week.png` |
| 6 | Moran's I: **DE 10 km cluster ≈ 0** (no distance-decay within market basin), **UK 50 km ≈ 0.27** (positive spatial clustering) | 0 vs 0.27 | `moran_paderborn.png`, `variogram.png` |
| 7 | Station density vs nearest foreign border: highest in the 50–100 km band — cross-border fuel-shopping signature | 282 / 800 in 50–100 km bin | `cross_border.png` |
| 8 | Brent → diesel asymmetric DL regression — suggestive rockets-and-feathers pattern but underpowered with 9-month overlap (n = 90) | qualitative | `brent_lag.png` |
| 9 | Real Brent grew **1.4 %/yr** since 1987, **below** the Hotelling 2 % path by ~$25/bbl by 2026; log-log r = 0.62 | 1.4 vs 2.0 %/yr | `hotelling.png` |
| 10 | Country-wide station network + country-wide kriged daily-diesel animation (Nov 2019, ~14 500 stations) shows the spatial price field and its daily breathing | qualitative | `germany_snapshot.png`, `germany_2019_animation.gif` |

## Reproducing

```sh
# 1. Refresh the locally collected Paderborn + Aberdeen feeds
#    from the mac-mini Lexar drive into ./data/raw/local/
wolframscript -file wolfram/ingest_local.wls

# 2. Fetch the country-wide Tankerkönig live snapshot
#    (~3 minutes, demo API key — returns real coords, placeholder prices)
wolframscript -file wolfram/fetch_tankerkoenig.wls

# 3. Fetch Brent + USD/EUR
wolframscript -file wolfram/fetch_brent.wls

# 4. Run every analysis + render figures
wolframscript -file wolfram/run_all.wls

# Or the whole pipeline:
wolframscript -file wolfram/run_all.wls --with-image-gen
```

## Image generation (optional)

```sh
wolframscript -file wolfram/generate_petrol_figures.wls
```

Requires `SystemCredential["OPENAI_API_KEY"]` set inside Mathematica's
SystemCredential store. The generator skips files that already exist;
pass `--force` to regenerate.

## Data sources & licenses

| Source | URL | License |
| --- | --- | --- |
| Tankerkönig historical bulk | `dev.azure.com/tankerkoenig/_git/tankerkoenig-data` | CC BY-NC-SA 4.0 |
| Tankerkönig live API | `creativecommons.tankerkoenig.de` | CC BY 4.0 |
| UK Fuel Finder (statutory, live 2 Feb 2026) | `fuel-finder.service.gov.uk` | OGL v3.0 |
| Brent crude (daily) | EIA `RBRTE` series | Public domain |
| Rotterdam ARA gasoline / diesel wholesale | ICE / S&P Global | Use under fair-use for analysis |
| Locally collected DE + UK feeds | `/Volumes/Lexar/...` (mac-mini) | Same as upstream |

Attribution string used in figures: *"Data: Tankerkönig
(creativecommons.tankerkoenig.de, CC BY-NC-SA 4.0); UK Fuel Finder
(fuel-finder.service.gov.uk, OGL v3.0); EIA Brent series."*

## Status

Active. Targeting a Wolfram Community staff-pick.

## Acknowledgements

This project would not exist without the work of the
**[Tankerkönig](https://creativecommons.tankerkoenig.de/)** team and
the **Markttransparenzstelle für Kraftstoffe / Bundeskartellamt**.
They have run, for more than a decade, a piece of public-data
infrastructure that is genuinely rare in Europe: minute-resolution,
country-wide, free, openly-licensed retail-fuel data, served over a
clean REST API and a daily-CSV git archive. Almost every figure in
this repository — the inverted intraday cycle, the 12-Uhr-Regel
hazard-rate compression, the diesel-vs-UK difference-in-differences,
the country-wide kriged animation — exists because they make that
data available. Thank you.

Thanks also to **[gustavz](https://github.com/gustavz/tankerkoenig_dataset)**
for the public GitHub mirror of the historical Tankerkönig archive,
which provides the country-wide kriging data used in §13–§14 of the
notebook; to the **UK Competition and Markets Authority** for the
legacy retailer-JSON feeds that make the UK control cluster tractable;
and to the **US Energy Information Administration** for the long-run
RBRTE Brent spot-price series.

Errors and limitations in this repository are mine. The Tankerkönig
data, the careful long-running collection effort behind it, and the
public-spirited licensing are theirs.

## Related

- [Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms)
- [ENSO-emergence](https://github.com/mthiel74/ENSO-emergence)
