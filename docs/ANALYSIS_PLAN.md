# Analysis plan

Each section names: **(a)** the data slice, **(b)** the model /
statistic, **(c)** the figure produced, **(d)** the success criterion
for "this is real, not noise".

The natural experiment everything else is built around: the German
**12-Uhr-Regel**, effective **1 April 2026**, only allows price
*increases* once per working day, at 12:00. Our local Paderborn /
Aberdeen feeds span 2025-09-14 → 2026-05-17, giving ~6.5 months
pre-rule and ~1.5 months post-rule.

---

## 1. The three-peak intraday cycle (pre-rule baseline)

**Data.** German stations, all readings 2025-09-14 → 2026-03-31.
Working days only (Mon–Fri, non-holiday).

**Model.** For each station × hour-of-day × fuel, the median price
relative to that station's daily mean. Aggregate over stations.

**Figure.** A heatmap (hour × weekday) of relative price, plus a 1-D
slice for the canonical weekday. The 3-peak / 4-trough motif should
be visible in E5 + diesel.

**Success.** ≥ 3 local maxima in the hourly median profile,
amplitude ≥ 5 c/L.

---

## 2. Hazard rate of price changes, pre vs post

**Data.** Per-station price change events (any of E5, E10, diesel
changes). German stations.

**Model.** For each minute-of-day, the empirical hazard
$\lambda(t)$ that a *price-up* event occurs. Compute separately for
pre-rule (2025-09-14 → 2026-03-31) and post-rule (2026-04-01 → end).
Same for price-down events.

**Figure.** Two-panel polar / linear plot of $\lambda(t)$ over a
24-h cycle, pre + post, up + down separately.

**Success.** Post-rule up-hazard sharply concentrates at 12:00 ±
ε minutes; down-hazard largely unchanged.

---

## 3. Daily amplitude before vs after

**Data.** Per station per day, $A_{i,d} = \max_t p_{i,d,t} - \min_t p_{i,d,t}$.

**Model.** Two-sample mean / median comparison, pre vs post; Cliff's δ
for effect size; bootstrap CI.

**Figure.** Strip plot or violin of $A_{i,d}$ pre vs post; per-station
shift arrows.

**Success.** Direction is interpretable (smaller amplitude = rule
worked; larger = retailers "use" their 12:00 slot to maximum effect).
Magnitude reported with CI.

---

## 4. 12-Uhr-Regel difference-in-differences

**Data.** Daily mean diesel price per station, German + UK (UK as the
control market).

**Model.**
$$ p_{i,d} = \alpha_i + \gamma_d + \tau\,\mathrm{Post}_d \cdot \mathrm{DE}_i + \beta\,\text{Brent}_d + \varepsilon_{i,d} $$
clustered SE by station.

**Figure.** Pre-post line of DE-UK mean diesel margin (over Brent),
with the 1 April 2026 cut-off marked.

**Success.** $|\tau|$ is reported with 95 % CI. Sign and magnitude
discussed honestly; null result also reportable.

---

## 5. Day-of-week effects (DE and UK separately)

**Data.** Daily station-level means.

**Model.** Two-way fixed effects (station, week) with day-of-week
dummies. Holiday dummies for DE / UK separately.

**Figure.** Day-of-week coefficients with CI, two markets side by side.

**Success.** Distinguishes structural day-of-week pattern from
post-rule artefact (e.g. Sunday/Saturday-driven price moves).

---

## 6. Spatial autocorrelation (within DE, within UK)

**Data.** Cross-section of daily-mean diesel price by station,
typically the latest week.

**Model.**
- Moran's *I* with a row-stochastic inverse-distance weight matrix
  (cap at 30 km, no diagonals).
- Geary's *C*.
- Empirical semivariogram $\gamma(h)$ on lag $h \in \{1, 2, 5, 10, 20, 50\}$ km.

**Figure.** Choropleth of price across Paderborn / Aberdeen (live
sample), plus the empirical semivariogram with a fitted spherical /
exponential model.

**Success.** Moran's $I > 0$ with $p < 0.01$; nugget < sill; finite
range identifiable.

---

## 7. Cross-border arbitrage (DE-NL/BE near Aachen; DE-FR near Strasbourg)

**Data.** Tankerkönig historical stations within 30 km of NL/BE/FR
borders, vs price feeds across the border. Cross-border data is
sparse; this section uses publicly available price-indicator scrapes
and reports limitations.

**Model.** Cross-section of (Δ price across border) vs distance from
border. If the border explains a discontinuity larger than the local
station-to-station noise, that's the arbitrage gap.

**Figure.** Scatter of station price vs *signed* distance to nearest
border crossing; a smoothed band per border.

**Success.** Clear discontinuity at zero distance with magnitude
consistent with tax-differential predictions (e.g. DE-LU diesel-tax
gap ≈ 23 c/L → expect a step of ≈ 12 c/L after smoothing).

---

## 8. Response lag to Brent (rockets-and-feathers)

**Data.** Daily national-mean pump diesel (Tankerkönig national avg
or our local cross-section), Brent daily spot in €/L.

**Model.** Asymmetric distributed-lag (Bacon 1991):
$$ \Delta p_t = \sum_{k=0}^{30} (\beta_k^+ \Delta b_{t-k}^+ + \beta_k^- \Delta b_{t-k}^-) + \varepsilon_t $$
with $\Delta b^+ = \max(\Delta b, 0)$, $\Delta b^- = \min(\Delta b, 0)$.

**Figure.** Cumulative impulse responses $\sum_{k=0}^h \beta_k^\pm$
out to 30 days.

**Success.** $\sum \beta^+ > \sum \beta^-$ at 1-day, 3-day, 7-day
horizons (the canonical asymmetry).

---

## 9. Hotelling's rule (long-horizon)

**Data.** Annual mean German real diesel price, 1972–2026 (use
Destatis Verbraucherpreisindex + AMI / Statista historical petrol
price series). Real interest rate proxy: ECB / Bundesbank 10-y real
yield.

**Model.** Test whether real net-of-tax pump price grows at the real
interest rate (Hotelling 1931).

**Figure.** Real net-of-tax pump price (log scale) vs cumulative real
return on a riskless bond; deviations annotated with major events
(1973, 1979, 1990, 2008, 2022, 2026).

**Success.** *Honest* finding: Hotelling is *not* recovered at the
retail level — confounded by tax, refining margin, demand cycles.
We document the gap, don't manufacture a positive.

---

## 10. Country-wide animated price field

**Data.** Tankerkönig historical for all of Germany, weekly mean per
station, 2025-09 → 2026-05.

**Model.** Inverse-distance-weighted interpolation onto a regular
~10 km grid over Germany, clipped to the polygon.

**Figure.** `GeoGraphics` frames stitched into an MP4 / GIF; shows
the supply-shock wave moving across the country.

**Success.** Frame-to-frame coherence (no flicker); legible week
labels; the 1 April 2026 rule cut-off and the Iran-spike peak visible.

---

## Figures index (committed)

| ID | Source | Filename |
| --- | --- | --- |
| F1 | analysis §1 | `docs/images/intraday_cycle_pre.png` |
| F2 | analysis §2 | `docs/images/hazard_rate_prepost.png` |
| F3 | analysis §3 | `docs/images/amplitude_prepost.png` |
| F4 | analysis §4 | `docs/images/did_de_uk.png` |
| F5 | analysis §5 | `docs/images/day_of_week.png` |
| F6 | analysis §6 | `docs/images/moran_paderborn.png` + `docs/images/variogram.png` |
| F7 | analysis §7 | `docs/images/cross_border.png` |
| F8 | analysis §8 | `docs/images/brent_lag.png` |
| F9 | analysis §9 | `docs/images/hotelling.png` |
| F10 | analysis §10 | `docs/images/germany_animation.gif` (+ MP4) |
| G1–G5 | gpt-image-2 | `docs/images/figures-generated/*.png` |
