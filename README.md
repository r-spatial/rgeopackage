
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeopackage

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rgeopackage)](https://CRAN.R-project.org/package=rgeopackage)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rgeopackage is to support reading and writing metadata
associated with GeoPackage (gpkg) files and assist in writing
reproducible GeoPackage files more broadly.

## Installation

You can install the development version of rgeopackage from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("r-spatial/rgeopackage")
```

## Overview

### Reading file metadata

-   `read_gpkg_contents()` returns a data.frame with the “gpkg_contents”
    table.
-   `read_gpkg_metadata()` and `read_gpkg_schema()` return a list with
    the related tables for the corresponding GeoPackage extension.

### Updating file timestamps

-   `preset_timestamp()`: presets the file timestamp for usage by GDAL
    by setting the environment variable `OGR_CURRENT_DATE`. The
    timestamp is adopted by GDAL during the entire session, unless
    `unset_timestamp()` is called.
-   `amend_timestamp()`: overwrites timestamps in the `gpkg_contents`
    and `gpkg_metadata_reference` tables of an existing GeoPackage file.
    While directly editing a GeoPackage is not advised, this function is
    especially useful in the presence of the optional table
    `gpkg_metadata_reference`, as GDAL does not control its timestamps
    as of writing (for GDAL 3.1.3). See a corresponding
    [issue](https://github.com/OSGeo/gdal/issues/3537) in the GDAL
    source repository.

By default, GDAL sets timestamps corresponding to system time, so
GeoPackages change when rewriting.

Both functions accept a `Date` or `POSIXct` object and format the
timestamp in order to comply with the GeoPackage requirement.

### Reference data

This package also includes reference tables for GeoPackage extensions
(both registered `gpkg_extensions` and community
`community_gpkg_extensions`).

## Related approaches using spatial R packages

-   `sf::st_write()` is [now able
    to](https://github.com/r-spatial/sf/issues/1618#issuecomment-811231056)
    set GDAL configuration options through its `config_options`
    argument. So when using `sf` you can pass the timestamp on a
    per-write basis:

    ``` r
    library(sf)
    nc <- st_read(system.file('shape/nc.shp', package = 'sf'), quiet = TRUE)
    fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET")
    # or using a Date object:
    # fixed_time <- as.Date("2020-12-25")
    timestamp <- format(fixed_time, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")
    st_write(nc, 'nc.gpkg', config_options = c(OGR_CURRENT_DATE = timestamp))
    ```

Note that this does not affect the value of the environment variable
`OGR_CURRENT_DATE`: `config_options = c(OGR_CURRENT_DATE = timestamp)`
directly sets the GDAL `OGR_CURRENT_DATE` *configuration option* which,
if unset, inherits from the `OGR_CURRENT_DATE` environment variable.
Also, note that `st_write()` ends by unsetting the configuration option,
so set it in each `st_write()` statement as needed.

In this case please take care to format the timestamp exactly as
required by the GeoPackage standard; cf. example above and [Requirement
15](https://www.geopackage.org/spec120/#r15) in version 1.2.

-   for packages relying on `rgdal` - like `sp` - it should be possible
    to set `OGR_CURRENT_DATE` by using `rgdal::setCPLConfigOption()`
    before doing the write operation. Again, take care to format the
    timestamp exactly as required by the GeoPackage standard.
