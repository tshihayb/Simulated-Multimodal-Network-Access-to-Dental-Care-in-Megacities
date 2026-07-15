# Computational environment

The analysis was run with **R 4.5.2 (2025-10-31 ucrt)** on
`x86_64-w64-mingw32` (Windows). The package versions used are listed below.
To reproduce the environment, install R 4.5.2 and these packages from CRAN
(the geospatial packages `dodgr`, `osmextract`, `sf`, and `lwgeom` have system-level
dependencies: GDAL, GEOS, PROJ).

| Package | Version | | Package | Version |
|---|---|---|---|---|
| dodgr | 0.4.3 | | osmdata | 0.3.0 |
| dplyr | 1.1.4 | | osmextract | 0.5.3 |
| flextable | 0.9.10 | | patchwork | 1.3.2 |
| ggh4x | 0.3.1 | | purrr | 1.2.1 |
| ggnewscale | 0.5.2 | | ragg | 1.5.0 |
| ggplot2 | 4.0.2 | | readxl | 1.4.5 |
| ggspatial | 1.1.10 | | scales | 1.4.0 |
| htmlwidgets | 1.6.4 | | sf | 1.0-24 |
| igraph | 2.2.1 | | stringr | 1.6.0 |
| janitor | 2.2.1 | | tibble | 3.3.1 |
| labelled | 2.16.0 | | tidyr | 1.3.2 |
| leaflet | 2.2.3 | | tidyverse | 2.0.0 |
| lwgeom | 0.2-15 | | units | 1.0-0 |
| magick | 2.9.0 | | viridisLite | 0.4.3 |
| matrixStats | 1.5.0 | | | |
| mice | 3.19.0 | | | |
| officer | 0.7.3 | | | |

`grid`, `stats`, and `tools` ship with base R.

## Notes on the geometry engine

The main script forces the planar GEOS engine for lon/lat polygon validation:

```r
sf::sf_use_s2(FALSE)
```

This is only needed on machines where an OS security policy (e.g. Windows Smart
App Control) blocks the unsigned `s2.dll`. All quantitative work is done in
projected UTM (EPSG:32638), which never uses s2, so results are unaffected. If
your machine loads `s2.dll` fine, this line can be removed.

To capture your own environment, run `sessionInfo()` after loading the packages.
