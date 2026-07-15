# Simulated Multimodal Network Access to Dental Care in Megacities

[![License: MIT](https://img.shields.io/badge/Code%20License-MIT-blue.svg)](LICENSE)
[![Data: CC BY 4.0](https://img.shields.io/badge/Data%20License-CC%20BY%204.0-lightgrey.svg)](DATA_LICENSE.md)
[![R 4.5.2](https://img.shields.io/badge/R-4.5.2-276DC3.svg)](SESSION.md)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21381692.svg)](https://doi.org/10.5281/zenodo.21381692)

Analysis code and public input data for a simulation study comparing
**multimodal public-transit** (walking + Riyadh Metro + bus/BRT) against the
**private car** for reaching dental facilities in Riyadh, Saudi Arabia. The
repository accompanies the manuscript *"Simulated Multimodal Network Access to
Dental Care in Megacities"* (under revision at *Community Dentistry and Oral
Epidemiology*).

> **Data governance:** per-district population figures for Riyadh are legally
> restricted (GASTAT) and are **not** distributed here in any form. The facility
> inventory is limited to 10 non-identifying fields. See
> [Data availability and reproducibility limits](#data-availability-and-reproducibility-limits).

## Overview

Random demand points are sampled across populated Riyadh districts and routed to
dental facilities over an actual road network derived from OpenStreetMap. For
each origin the study computes and compares:

- **Direct car** travel (with a parking-search sensitivity layer), and
- **Multimodal transit** chains: an access leg (walk or car park-and-ride), a
  line-haul leg on metro and/or bus/BRT with realistic waits and transfer rules,
  and an egress walk.

Facilities are approached at four **anchors**: nearest, median, farthest, and a
random facility (averaged over repeated draws), for both **private** and
**public** ownership, under **unweighted** (uniform) and **population-weighted**
sampling. Headline outcomes include mean travel time, the share of transit trips
faster than car, time savings, the **break-even car speed** at which car
overtakes transit, and facility reachability, each with robustness analyses
(walking speed, wait maxima, a traffic-speed sweep, peak/off-peak crowding, and
missing-distance imputation method).

## Repository structure

```
.
├── Code/     R analysis scripts (see "Code" below)
├── Data/     Public input data (see "Data" and DATA_DICTIONARY.md)
├── README.md
├── DATA_DICTIONARY.md   field-level documentation of every data file
├── SESSION.md           R version + exact package versions
├── CITATION.cff         how to cite this repository
├── LICENSE              MIT (applies to Code/)
├── DATA_LICENSE.md      CC BY 4.0 (author data) + third-party terms
└── .gitignore
```

## Data

Field-level documentation for every file is in **[DATA_DICTIONARY.md](DATA_DICTIONARY.md)**.

| File | What it is | Source |
|---|---|---|
| `all facilities.xlsx` | Dental-facility inventory: 910 facilities, 10 fields (766 provide dental services). | Author-compiled field/registry data |
| `2025_06_22_List_of_Riyadh_dsitricts_stripped_of_population.xlsx` | District name / region reference list, **stripped of population**. | Author-compiled |
| `districts.geojson` | District boundary polygons. | Riyadh open data |
| `metro-stations-...2024.geojson` | Riyadh Metro stations. | Riyadh open data |
| `bus-stops-...2024.geojson` | Riyadh bus stops (route / direction / shelter). | Riyadh open data |
| `riyadh_bus_routes_by_code.rds`, `Final_Routes_Processed.csv` | Processed bus-route sequences. | Derived from the stop data |
| `_official_district_labels.geojson`, `_official_district_linework.geojson` | Map overlays for figures. | Riyadh open data |

**Not included** (see [reproducibility limits](#data-availability-and-reproducibility-limits)):
the restricted population table; the ~244 MB OpenStreetMap extract; and large
regenerable intermediates (routing graph, snapped roads, sampled points,
travel-time result tables).

## Code

Run from **`Code/`** (scripts assume `Data/` is a sibling directory).

**Main pipeline**

- **`Analysis clean actual road distance.R`** runs the full pipeline: data prep,
  network build, facility anchors, per-origin routing, travel-time and
  competitiveness tables, and figures. It `source()`s the revision modules below.
- `Bus routes sequence and line build up.R` builds the bus-route object
  (`riyadh_bus_routes_by_code.rds`) consumed by the main script.
- `run_pop_safe.R` is an optional runner that executes the pipeline while
  **guaranteeing the restricted population table is never read** (it loads
  supplied intermediates instead of rebuilding them, and refuses to start if any
  are missing).

**Revision analysis modules** (sourced by the main script):
`_r14_mm_reach_full.R`, `_r14_mm_reach_fix.R`, `_r14_imputation.R`,
`_r14_chain_be.R`, `_r14_orchestrate.R`, `_r13_permode.R`, `_r13_orchestrate.R`,
`_r13_enhanced.R`, `_r13_bounds.R`, `_r13_enh_se.R`.

**Figure / table modules** (run standalone). The versions used for the final
manuscript are the `*_corrected` / `*_signed_corrected` ones:
`_r13_manuscript_figs_corrected.R`, `_r13_tornado_signed_corrected.R`,
`_r14_impact_figure.R`, `_r14_reachability_figure.R`,
`_region_of_origin_figure.R`, `_region_of_origin_metrics.R`,
`_region_of_origin_breakeven.R`. Earlier variants of these (e.g.
`_r13_manuscript_figs.R`, `_r13_tornado_recolor*.R`,
`_r14_method_impact_figure.R`, `_r14_recompute_optionA.R`,
`_random_*_convergence*.R`) are retained for provenance.

Development test/validation scripts are not included in this release.

## Requirements

R **4.5.2** and the packages listed in **[SESSION.md](SESSION.md)** (from CRAN).
The geospatial packages (`sf`, `lwgeom`, `dodgr`, `osmextract`) require system
GDAL / GEOS / PROJ.

## How to run

1. **Clone** this repository.
2. **Set the working directory.** Near the top of
   `Analysis clean actual road distance.R` (and the companion), edit the
   `setwd(...)` line to point at this repo's `Data/` folder on your machine.
3. **Get the road network.** The routing graph is built from an OpenStreetMap
   extract that is too large to host here. Download a **GCC states** extract from
   [Geofabrik](https://download.geofabrik.de/asia/gcc-states.html) and place the
   `.osm.pbf` in `Data/` (the study used the 2025-12-11 snapshot). Alternatively,
   supply the pre-built graph/roads intermediates and run via `run_pop_safe.R`.
4. **Run** `Rscript "Code/Analysis clean actual road distance.R"`, or, to keep
   the population table out of the process entirely, `Rscript "Code/run_pop_safe.R"`.
   The full pipeline is computationally heavy (network routing over tens of
   thousands of origins); several sections take from minutes to hours.

## Data availability and reproducibility limits

- **Restricted (never shared): per-district population.** Riyadh per-district
  population (GASTAT) cannot be redistributed. The scripts reference the
  population file only inside guarded rebuild blocks; the population-safe runner
  bypasses them. Consequently the **population-weighted** results cannot be fully
  regenerated from the public data. The **unweighted (uniform)** analyses and all
  region-level summaries are reproducible.
- **Large external input: OpenStreetMap.** The ~244 MB `.osm.pbf` is not tracked;
  download it from Geofabrik (link above). Road-network results depend on the OSM
  snapshot date.
- **Regenerable intermediates.** The routing graph, snapped road layer, sampled
  points, and travel-time result tables are produced by the scripts and are not
  committed (to keep the repository small and to avoid embedding
  population-derived samples). Regenerate them by running the pipeline, or
  request them from the author.

## Citation

Archived on Zenodo: **[10.5281/zenodo.21381692](https://doi.org/10.5281/zenodo.21381692)**
(concept DOI; always resolves to the latest version).
See **[CITATION.cff](CITATION.cff)** (GitHub's "Cite this repository" button).
Please also cite the accompanying manuscript once published.

## Releasing / minting a DOI

To make the archive citable with a DOI:

1. Enable the repository in [Zenodo](https://zenodo.org) (Zenodo → GitHub → flip
   the repo on).
2. Create a tagged GitHub release (e.g. `v1.0.0`). Zenodo archives it and mints a
   DOI.
3. Add the DOI badge to the top of this README (a placeholder is commented in) and
   the DOI to `CITATION.cff`.

## License

- **Code** (`Code/`): [MIT](LICENSE).
- **Data** (`Data/`): [CC BY 4.0](DATA_LICENSE.md) for author-created data;
  third-party layers (Riyadh open data, OpenStreetMap/ODbL) retain their original
  terms.

## Contact

Talal Alshihayb · <tshihayb@gmail.com>
