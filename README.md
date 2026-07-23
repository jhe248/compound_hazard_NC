# Compound hazard exposure, loss and insurance in North Carolina

Code for the analysis of compound hurricane hazards — wind, storm surge and
inland flood — over North Carolina's single-family (RES1) housing stock, across
a 604-member synthetic hurricane ensemble.

Authors: Jiahang He, Ian Sue Wing.

---

## Repository layout

| Script | Role | Produces |
|---|---|---|
| `config.R` | Paths, package check, hazard-file index | — |
| `run_all.R` | Master script; sources 00–08 in order | — |
| `00_loss_calculation.R` | Building × storm wind / flood / surge damages (Hazus 7) | intermediate data |
| `01_extensive_margin.R` | Exposure probabilities; Table 1 Panels A, B, C | tables |
| `02_intensive_margin.R` | Conditional damage statistics | tables |
| `03_main_figures_exposure.R` | **Figures 2, 3, 4** | main figures |
| `04_main_figures_tricolore.R` | **Figure 5 (a, b, c)** | main figures |
| `05_insurance_data.R` | Insurance merge, priced-sample damages, sample coverage | intermediate data |
| `06_main_figures_insurance.R` | **Figure 6** | main figure |
| `07_SI_figures_hazard.R` | Supplementary hazard figures | SI figures |
| `08_SI_figures_insurance.R` | Supplementary insurance figures and tables | SI figures, tables |

Two standalone extras, not part of the pipeline:

| Script | Role |
|---|---|
| `09_kevin_zone_losses.R` | Building-level losses in the coastal pricing zones (external deliverable) |
| `10_flood_diagnostics.R` | Sanity checks of the inland-flood signal against known NC flood geography |

---

## How to run

Set the working directory to the project root, then:

```r
source("code/run_all.R")
```

`run_all.R` sources `config.R` (which defines all paths and installs any missing
packages) and then runs 00–08 in order. Edit `project_dir` and `hazard_dir_csv`
at the top of `config.R` to point at your analysis root and the hazard CSVs.

The standalone extras need the same path setup and step 00's outputs:

```r
source("code/config.R"); source("code/09_kevin_zone_losses.R")
```

Steps 00 and 05 are the expensive ones — they loop over the eight latitude-bin
hazard chunks. Everything from 01 onwards reads intermediate `.qs` files, so the
figure and table scripts can be re-run on their own without repeating step 00.

### Dependencies

R (≥ 4.4) and: `qs2`, `data.table`, `dplyr`, `ggplot2`, `patchwork`, `scales`,
`cowplot`, `bit64`, `sf`, `stringr`, `pammtools`, `tricolore`, `ggtern`,
`extrafont`, `tigris`, `magick`, `readxl`. `config.R` installs whatever is
missing. `tigris` downloads the county basemap on first use and caches it.

---

## Inputs

```
input_data/
├── inventories/nc_building_inventory.qs             NC building inventory
├── damage_functions/wind_damage_func_hazus7.csv     Hazus 7 wind curves
├── damage_functions/flood_damage_func_hazus7.csv    Hazus 7 flood curves
├── crosswalk/nhgis_blk2010_blk2020_ge_37.csv        2010 → 2020 block crosswalk
├── shapefiles/nc_blk_shp/, nc_county_shp/           block and county boundaries
├── shapefiles/nc_tract_shp.gpkg                     tract polygons (cached; rebuilt if absent)
├── insurance/Insured_wind_flood_surge_afford.xlsx   priced insurance sample
├── insurance/zones_1mile.shp, zones_1to2miles.shp   coastal pricing zones
└── new_construction/*.csv                           tract population and ACS housing age
```

The hazard CSVs are **not** under `input_data/`; they are read directly from the
shared folder named by `hazard_dir_csv` (604 storms across 8 latitude bins).

---

## Outputs

Output location encodes the manuscript's structure: `output_files/figures/`
holds **only** the main-text figures, and everything else is supplementary.

### Main text

Figure 1 is a schematic assembled outside this repository. Figures 2–6 are
produced here:

| Manuscript figure | File | Script |
|---|---|---|
| 2 | `figures/probability_exposure_histograms.png` | 03 |
| 3 | `figures/hist_exposure_loss_individual_hazards.png` | 03 |
| 4 | `figures/bivariate_exposure_loss.png` | 03 |
| 5a / 5b / 5c | `figures/panel_RES1_{50th,95th,99th}_percentile.png` | 04 |
| 6 | `figures/insurance_panels.png` | 06 |

`figures/tricolore_parts/` holds the map, ternary-key and legend PNGs that
Figure 5 is composited from; they are intermediates, not manuscript figures.

### Supplementary figures — `output_files/figures/SI_figures/`

From `07_SI_figures_hazard.R`:

- `SI_FigA1a`, `SI_FigA1b` — damage probability at 250 m and block resolution
- `SI_FigA2a–c` — wind, flood and surge intensity at 250 m, by percentile
- `SI_FigA3a–d` — block-level loss ratios, by percentile
- `SI_FigA4a–d` — block-level loss totals, by percentile
- `SI_FigA5a–g` — any-loss percentile maps decomposed into wind / flood / surge
- `SI_FigA8_new_construction_overlay.png` — residential growth vs the compound-risk coast
- `probability_exposure_map.png` — block loss probability by hazard combination

From `08_SI_figures_insurance.R`:

- `worstcase_uninsured_crosstab_combined.png` — worst-case uninsured loss by market
- `insurance_affordability_map.png` — homes whose offered premium exceeds 5% of value
- `insurance_price_density.png` — offered-premium distribution by coverage product
- `stratification_6panel_p95_p99.png` — P95 / P99 event exposure and damage by zone

### Tables — `output_files/tables/`

Extensive margin (01), intensive margin (02), hazard correlations (03), sample
coverage (05), zone statistics (06), and the worst-case, affordability and
stratification tables (08). `RES1_extensive_panelBC_latex.txt` and
`stratification_by_haztype_latex.txt` are paste-ready LaTeX table rows.

---

## Method notes

Points that are easy to misread from the code alone.

**Compound loss is not additive.** Per building and storm, peril damages are
each capped at structure and contents value; the combined loss is the sum
capped at total value, so a home cannot lose more than it is worth. Where a
single figure needs one number per building, `pmax` over the three perils is
used instead, and the two conventions are compared in
`RES1_intensive_table4_aggregation_comparison.csv`.

**"Any loss" probability is a sum, not a maximum.** The seven hazard categories
are mutually exclusive per storm, so the probability of any loss is their sum.
Using `pmax` would understate it. (Panel A's nonzero test is the one place the
two agree, so it uses `pmax`.)

**Quantiles are property-weighted.** Hazard is uniform within a 250 m cell, so
every home in a cell shares its cell's probability, but the Panel B and C
distributions are taken over homes — each cell weighted by its building count —
rather than over cells weighted equally.

**Percentiles are conditional on damage.** P95 and P99 are taken over damaging
events only, in the tricolore panels (04), the stratification table (08) and the
zone maps (08) alike. Buildings never damaged in the ensemble contribute zero.

**Insurance coverage is nested, not exclusive.** A household holding wind+flood
is still wind-insured. Moving from a wind-only market to a wind+flood market
only adds flood and surge coverage, so uninsured loss can never rise; the two
uninsured measures in 08 are guarded by an assertion enforcing this.

**Offered premiums are reconstructed, not observed.** The zone price applies per
dollar of insured expected loss, and the flood policy covers flood *and* surge,
so full coverage costs `price_per_loss × (E[wind] + E[flood] + E[surge])`. Both
identities are asserted against the spreadsheet in 08, so a re-priced input
fails loudly rather than silently mis-reconstructing premiums.

**Panel B of SI Figure A8 is housing-stock age, not permits.** The Census
Building Permits Survey publishes at place level and above, so there is no
tract-level permit series. ACS "Year Structure Built" is the tract-resolved,
time-matched substitute.

**Hazard data.** The 07-2026 hazard package re-runs the ensemble after a
dry-start initialization bug in the inland-flood layer. Its files carry an
18-line comment header with no column-name row, which is why 00 reads them with
`skip = 18, header = FALSE`. Wind and surge are unchanged from the earlier
package; inland flood differs, mostly in the northern basins.

---

## Conventions

Scripts are sourced into the global environment in the order above, and each
defines what it needs before using it. Section headers use a single style;
inline comments are reserved for things the code cannot state itself. Scripts
write files and assert invariants rather than printing to the console, so a run
is silent unless something is wrong.
