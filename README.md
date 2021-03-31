## Helper tools to work with GeoPackage files in R

### Functionality

The package currently provides two functions that assist in writing reproducible GeoPackage files:

- `preset_timestamp()`: presets the file timestamp for usage by GDAL by setting the environment variable `OGR_CURRENT_DATE`.
- `amend_timestamp()`: overwrites timestamps in the `gpkg_contents` and `gpkg_metadata_reference` tables of an existing GeoPackage file.
While directly editing a GeoPackage is not advised, this function is especially useful in the presence of the optional table `gpkg_metadata_reference`, as GDAL does not control its timestamps as of writing (for GDAL 3.1.3).
See a corresponding [issue](https://github.com/OSGeo/gdal/issues/3537) in the GDAL source repository.

By default, GDAL sets timestamps corresponding to system time, so GeoPackages change when rewriting.

Both functions accept a `Date` or `POSIXct` object and format the timestamp in order to comply with the GeoPackage requiremant.
See the functions' documentation and examples to get a better understanding.

### Installation

```r
remotes::install_github("florisvdh/rgeopackage")
```

### Functionality offered by core spatial R packages

- `sf::st_write()` is [now able to](https://github.com/r-spatial/sf/issues/1618#issuecomment-811231056) set GDAL configuration options through its `config_options` argument.
So when using `sf` you're advised to pass the timestamp as follows:

  ```r
  library(sf)
  nc <- st_read(system.file('shape/nc.shp', package = 'sf'), quiet = TRUE)
  fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET")
  # or using a Date object:
  # fixed_time <- as.Date("2020-12-25")
  timestamp <- format(fixed_time, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")
  st_write(nc, 'nc.gpkg', config_options = c(OGR_CURRENT_DATE = timestamp))
  ```
  
  Note that `OGR_CURRENT_DATE` will be unset again after each write operation.
  
  In this case please take care to format the timestamp as required by the GeoPackage standard; cf. example above and [Requirement 15](https://www.geopackage.org/spec120/#r15) in version 1.2.
  
- for packages relying on `rgdal` it should be possible to set `OGR_CURRENT_DATE` by using `rgdal::setCPLConfigOption()` before doing the write operation.
Again, take care to format the timestamp as required by the GeoPackage standard.


