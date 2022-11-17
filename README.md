
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeopackage

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rgeopackage)](https://CRAN.R-project.org/package=rgeopackage)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/r-spatial/rgeopackage/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-spatial/rgeopackage?branch=main)
<!-- badges: end -->

The goal of rgeopackage is to support reading and writing metadata
associated with GeoPackage (gpkg) files and assist in writing
reproducible GeoPackage files.

## Installation

You can install the development version of rgeopackage from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("r-spatial/rgeopackage")
```

## Overview

### Updating file timestamps

- `preset_timestamp()`: presets the file timestamp for usage by GDAL by
  setting the environment variable `OGR_CURRENT_DATE`. The timestamp is
  adopted by GDAL during the entire session, unless `unset_timestamp()`
  is called.
- `amend_timestamp()`: overwrites timestamps in the `gpkg_contents` and
  `gpkg_metadata_reference` tables of an existing GeoPackage file.

By default, GDAL sets timestamps corresponding to system time, so
GeoPackages change when rewriting. Both functions accept a `Date` or
`POSIXct` object and use standard formatting for GeoPackage files.

## Related R packages

- The [sf package](https://r-spatial.github.io/sf/) allows users to set
  GDAL configuration options in `sf::st_write()` using the
  `config_options` argument ([see sf issue
  \#1618](https://github.com/r-spatial/sf/issues/1618#issuecomment-811231056))
  and in `sf::st_read()` using the `options` argument (see [sf issue
  1157](https://github.com/r-spatial/sf/issues/1157)). sf also includes
  a set of GDAL functions (e.g. `sf::gdal_metadata()`) that are not
  intended to be called directly by the user but could be used to access
  and edit GeoPackage metadata.

- The [vapour package](https://hypertidy.github.io/vapour/index.html)
  provides access to the basic read functions available in GDAL
  including GeoPackage contents and extension tables.
