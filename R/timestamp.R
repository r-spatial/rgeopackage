#' Preset timestamp to reproducibly write GeoPackage files
#'
#' @description
#' Presets the timestamp for usage by GDAL by setting the environment variable
#' `OGR_CURRENT_DATE`.
#' After this, newly written GeoPackage files
#' created by the GDAL vector or raster driver (e.g. through
#' [sf::st_write()] or [stars::write_stars()])
#' will carry this timestamp.
#' As such [preset_timestamp()] assists in making a binary-reproducible
#' GeoPackage file.
#'
#' [unset_timestamp()] removes `OGR_CURRENT_DATE` from the
#' environment.
#'
#' @details
#' The function converts the timestamp to a very specific ISO 8601 format
#' that is required by the GeoPackage standard, including conversion to UTC.
#' Cf. [Requirement 15](https://www.geopackage.org/spec130/#r15) in
#' version 1.3.
#' GDAL uses the timestamp to set the `last_change` column of the
#' `gpkg_contents` table in newly written GeoPackage files.
#'
#' The timestamp set by [preset_timestamp()] is adopted by GDAL during
#' the entire session, unless [unset_timestamp()] is called.
#'
#' @param timestamp a `Date` or `POSIXct` object, used to generate
#' the timestamp.
#' For a `Date` object, time will be considered as `00:00:00 UTC`.
#'
#' @return
#' Previous value of environment variable `OGR_CURRENT_DATE` is returned
#' invisibly.
#'
#' @seealso
#' Other functions to control the GeoPackage timestamp(s):
#' [amend_timestamp()],
#' [sf::st_write()]
#'
#' @examples
#' library(sf)
#' library(openssl)
#' md5sum <- function(x) paste(md5(file(x)))
#'
#' # Using existing geopackage with vector layer:
#' filepath <- system.file("gpkg/b_pump.gpkg", package = "sf")
#' (md5_original <- md5sum(filepath))
#'
#' sf_layer <- read_sf(system.file("gpkg/b_pump.gpkg", package = "sf"))
#'
#' # A rewrite changes the checksum:
#' filepath_notimeset <- file.path(tempdir(), "b_pump_notimeset.gpkg")
#' # write 1:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset1 <- md5sum(filepath_notimeset))
#' # write 2:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset2 <- md5sum(filepath_notimeset))
#' # compare:
#' md5_notimeset1 == md5_notimeset2
#'
#' # Setting a fixed date
#' filepath_timeset <- file.path(tempdir(), "b_pump_timeset.gpkg")
#' (fixed_date <- as.Date("2020-12-25"))
#' preset_timestamp(fixed_date)
#' # write 1 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset1 <- md5sum(filepath_timeset)
#' # write 2 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset2 <- md5sum(filepath_timeset)
#' # compare:
#' all.equal(md5_timeset1, md5_timeset2)
#'
#' # Setting a fixed time
#' (fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET"))
#' preset_timestamp(fixed_time)
#' # write 3 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset3 <- md5sum(filepath_timeset)
#' # write 4 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset4 <- md5sum(filepath_timeset)
#' # compare:
#' all.equal(md5_timeset3, md5_timeset4)
#'
#' # Also works for GPKG 2D gridded coverage (with stars):
#' library(stars)
#' library(dplyr)
#'
#' filepath_stars <- file.path(tempdir(), "stars_2d.gpkg")
#' (fixed_time <- as.POSIXct("2010-02-14 12:00:00", tz = "CET"))
#' preset_timestamp(fixed_time)
#'
#' stars_2d <-
#'   system.file("tif/L7_ETMs.tif", package = "stars") %>%
#'   read_stars() %>%
#'   slice(band, 1)
#' # write 1:
#' stars_2d %>%
#'   write_stars(filepath_stars, driver = "GPKG")
#' md5_stars1 <- md5sum(filepath_stars)
#' # write 2:
#' stars_2d %>%
#'   write_stars(filepath_stars, driver = "GPKG")
#' md5_stars2 <- md5sum(filepath_stars)
#' # compare:
#' all.equal(md5_stars1, md5_stars2)
#'
#' @author Floris Vanderhaeghe, <https://github.com/florisvdh>
#'
#' @export
preset_timestamp <- function(timestamp) {
  check_timestamp(timestamp)

  timestamp <- fmt_timestamp(timestamp)

  old <- Sys.getenv("OGR_CURRENT_DATE")
  Sys.setenv(OGR_CURRENT_DATE = timestamp)

  invisible(old)
}


#' @rdname preset_timestamp
#' @export
unset_timestamp <- function() {
  Sys.unsetenv("OGR_CURRENT_DATE")
}


#' Amend the timestamp(s) in a GeoPackage file
#'
#' Overwrites all timestamps (column `last_change`) of the
#' `gpkg_contents` table in an existing GeoPackage file.
#' If the optional table `gpkg_metadata_reference` is present, does the
#' same with its `timestamp` column.
#' As such the function assists in making a binary-reproducible GeoPackage file.
#'
#' Internally the timestamp is converted to a specific ISO 8601 format
#' that is required by the GeoPackage standard.
#'
#' @note
#' Directly editing a GeoPackage is not advised; whenever possible use
#' [preset_timestamp()] since it goes via GDAL.
#'
#' However `amend_timestamp()` is especially useful when a
#' GeoPackage file also contains a timestamp in the optional table
#' `gpkg_metadata_reference`, as GDAL does not control that timestamp
#' as of writing (for GDAL 3.1.3).
#' See a corresponding [issue](https://github.com/OSGeo/gdal/issues/3537)
#' in the GDAL source repository.
#'
#' @param dsn the path to the GeoPackage file (*.gpkg)
#' @param timestamp a `Date` or `POSIXct` object, used to generate
#' the timestamp.
#' For a `Date` object, time will be considered as `00:00:00 UTC`.
#' Defaults to system time, however must be set explicitly for reproducible
#' workflows.
#' @param quiet If `TRUE`,print informational message
#' @param verbose Deprecated. Logical. For each relevant table, prints a message with the number of affected rows.
#'
#' @return
#' `NULL` is returned invisibly.
#'
#' @seealso
#' Other functions to control the GeoPackage timestamp(s):
#' [preset_timestamp()],
#' [sf::st_write()]
#'
#' @examples
#' library(sf)
#' library(openssl)
#' md5sum <- function(x) paste(md5(file(x)))
#'
#' # Using existing geopackage with vector layer:
#' filepath <- system.file("gpkg/b_pump.gpkg", package = "sf")
#' (md5_original <- md5sum(filepath))
#'
#' sf_layer <- read_sf(system.file("gpkg/b_pump.gpkg", package = "sf"))
#'
#' # A rewrite changes the checksum:
#' filepath_notimeset <- file.path(tempdir(), "b_pump_notimeset.gpkg")
#' # write 1:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset1 <- md5sum(filepath_notimeset))
#' # write 2:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset2 <- md5sum(filepath_notimeset))
#' # compare:
#' md5_notimeset1 == md5_notimeset2
#'
#' # Setting a fixed date
#' filepath_timeset <- file.path(tempdir(), "b_pump_timeset.gpkg")
#' (fixed_date <- as.Date("2020-12-25"))
#' # write 1 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_date)
#' md5_timeset1 <- md5sum(filepath_timeset)
#' # write 2 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_date)
#' md5_timeset2 <- md5sum(filepath_timeset)
#' # compare:
#' all.equal(md5_timeset1, md5_timeset2)
#'
#' # Setting a fixed time
#' (fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET"))
#' # write 3 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_time)
#' md5_timeset3 <- md5sum(filepath_timeset)
#' # write 4 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_time)
#' md5_timeset4 <- md5sum(filepath_timeset)
#' # compare:
#' all.equal(md5_timeset3, md5_timeset4)
#'
#' # Also works for GPKG 2D gridded coverage (with stars):
#' library(stars)
#' library(dplyr)
#'
#' filepath_stars <- file.path(tempdir(), "stars_2d.gpkg")
#'
#' stars_2d <-
#'   system.file("tif/L7_ETMs.tif", package = "stars") %>%
#'   read_stars() %>%
#'   slice(band, 1)
#' # write 1:
#' stars_2d %>%
#'   write_stars(filepath_stars, driver = "GPKG")
#' amend_timestamp(filepath_stars, fixed_time)
#' md5_stars1 <- md5sum(filepath_stars)
#' # write 2:
#' stars_2d %>%
#'   write_stars(filepath_stars, driver = "GPKG")
#' amend_timestamp(filepath_stars, fixed_time)
#' md5_stars2 <- md5sum(filepath_stars)
#' # compare:
#' all.equal(md5_stars1, md5_stars2)
#'
#' @author Floris Vanderhaeghe, <https://github.com/florisvdh>
#'
#' @export
amend_timestamp <- function(dsn,
                            timestamp = Sys.time(),
                            quiet = FALSE,
                            verbose = TRUE) {
  stopifnot(is.logical(quiet), !is.na(quiet))
  if (verbose != !quiet) {
    cli_warn("{.arg verbose} is deprecated, use {.arg quiet} instead.")
  }

  timestamp <- fmt_timestamp(timestamp)
  con <- connect_gpkg(dsn = dsn)

  update_gpkg_table(
    con,
    table_name = "gpkg_contents",
    statement = glue_sql("SET last_change = {timestamp}", .con = con),
    message = "Updated {.file {basename(dsn)}} timestamp to: {.val {timestamp}}",
    quiet = quiet
  )

  has_metadata <-
    has_query_min_rows(
      con,
      "SELECT name FROM sqlite_master
      WHERE name == 'gpkg_metadata_reference'",
    )

  if (has_metadata) {
    update_gpkg_table(
      con,
      table_name = "gpkg_metadata_reference",
      statement = glue_sql("SET timestamp = {timestamp}", .con = con),
      quiet = quiet
    )
  }

  RSQLite::dbDisconnect(con)
  invisible(NULL)
}


#' Check if timestamp is a Date or POSIXct object
#'
#' @noRd
check_timestamp <- function(timestamp,
                            call = parent.frame()) {
  if (!inherits(timestamp, c("Date", "POSIXct"))) {
    cli_abort(
      "{.arg timestamp} must be a {.cls Date} or {.cls POSIXct} object,
        not a {.cls {class(timestamp)}} object.",
      call = call
    )
  }
}

#' Format timestamp
#'
#' @noRd
fmt_timestamp <- function(timestamp,
                          call = parent.frame()) {
  check_timestamp(timestamp, call)
  format(timestamp,
    format = "%Y-%m-%dT%H:%M:%S.000Z",
    tz = "UTC"
  )
}
