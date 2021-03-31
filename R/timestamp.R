#' Preset timestamp to reproducibly write GeoPackage files
#'
#' Presets the timestamp for usage by GDAL by setting the environment variable
#' \code{OGR_CURRENT_DATE}.
#' After this, newly written GeoPackage files
#' created by the GDAL vector or raster driver (e.g. through
#' \code{sf::st_write()} or \code{stars::write_stars()})
#' will carry this timestamp.
#' As such the function assists in making a binary-reproducible GeoPackage file.
#'
#' The function converts the timestamp to a very specific ISO 8601 format
#' that is required by the GeoPackage standard, including conversion to UTC.
#' Cf. \href{https://www.geopackage.org/spec130/#r15}{Requirement 15} in
#' version 1.3.
#' GDAL uses the timestamp to set the \code{last_change} column of the
#' \code{gpkg_contents} table in newly written GeoPackage files.
#'
#' @param timestamp a \code{Date} or \code{POSIXct} object, used to generate
#' the timestamp.
#' For a \code{Date} object, time will be considered as \code{00:00:00} local
#' time.
#'
#' @return
#' Previous value of system variable \code{OGR_CURRENT_DATE} is returned
#' invisibly.
#'
#' @family functions to control the GeoPackage timestamp(s)
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
#'   # write 1:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset1 <- md5sum(filepath_notimeset))
#'   # write 2:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset2 <- md5sum(filepath_notimeset))
#'   # compare:
#' md5_notimeset1 == md5_notimeset2
#'
#' # Setting a fixed date
#' filepath_timeset <- file.path(tempdir(), "b_pump_timeset.gpkg")
#' (fixed_date <- as.Date("2020-12-25"))
#' preset_timestamp(fixed_date)
#'   # write 1 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset1 <- md5sum(filepath_timeset)
#'   # write 2 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset2 <- md5sum(filepath_timeset)
#'   # compare:
#' all.equal(md5_timeset1, md5_timeset2)
#'
#' # Setting a fixed time
#' (fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET"))
#' preset_timestamp(fixed_time)
#'   # write 3 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset3 <- md5sum(filepath_timeset)
#'   # write 4 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' md5_timeset4 <- md5sum(filepath_timeset)
#'   # compare:
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
#' 	system.file("tif/L7_ETMs.tif", package = "stars") %>%
#' 	read_stars() %>%
#' 	slice(band, 1)
#'   # write 1:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' md5_stars1 <- md5sum(filepath_stars)
#'   # write 2:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' md5_stars2 <- md5sum(filepath_stars)
#'   # compare:
#' all.equal(md5_stars1, md5_stars2)
#'
#' @author Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @export
preset_timestamp <- function(timestamp) {
	if (!inherits(timestamp, c("Date", "POSIXct"))) {
		stop("timestamp must be a Date or POSIXct object")
	}

	timestamp <- format(timestamp,
						format = "%Y-%m-%dT%H:%M:%S.000Z",
						tz = "UTC")

	old <- Sys.getenv("OGR_CURRENT_DATE")
	Sys.setenv(OGR_CURRENT_DATE = timestamp)

	return(invisible(old))
}
















#' Amend the timestamp(s) in a GeoPackage file
#'
#' Overwrites all timestamps (column \code{last_change}) of the
#' \code{gpkg_contents} table in an existing GeoPackage file.
#' If the optional table \code{gpkg_metadata_reference} is present, does the
#' same with its \code{timestamp} column.
#' As such the function assists in making a binary-reproducible GeoPackage file.
#'
#' Internally the timestamp is converted to a specific ISO 8601 format
#' that is required by the GeoPackage standard.
#'
#' @note
#' Directly editing a GeoPackage is not advised; whenever possible use
#' \code{\link{preset_timestamp}()} since it goes via GDAL.
#'
#' However \code{amend_timestamp()} is especially useful when a
#' GeoPackage file also contains a timestamp in the optional table
#' \code{gpkg_metadata_reference}, as GDAL does not control that timestamp
#' as of writing (for GDAL 3.1.3).
#' See a corresponding \href{https://github.com/OSGeo/gdal/issues/3537}{issue}
#' in the GDAL source repository.
#'
#' @param dsn the path to the GeoPackage file (*.gpkg)
#' @param timestamp a \code{Date} or \code{POSIXct} object, used to generate
#' the timestamp.
#' For a \code{Date} object, time will be considered as \code{00:00:00} local
#' time.
#' Defaults to system time, however must be set explicitly for reproducible
#' workflows.
#' @param verbose Logical.
#' For each relevant table, prints a message with the number of affected rows.
#'
#' @return
#' \code{NULL} is returned invisibly.
#'
#' @family functions to control the GeoPackage timestamp(s)
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
#'   # write 1:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset1 <- md5sum(filepath_notimeset))
#'   # write 2:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset2 <- md5sum(filepath_notimeset))
#'   # compare:
#' md5_notimeset1 == md5_notimeset2
#'
#' # Setting a fixed date
#' filepath_timeset <- file.path(tempdir(), "b_pump_timeset.gpkg")
#' (fixed_date <- as.Date("2020-12-25"))
#'   # write 1 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_date)
#' md5_timeset1 <- md5sum(filepath_timeset)
#'   # write 2 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_date)
#' md5_timeset2 <- md5sum(filepath_timeset)
#'   # compare:
#' all.equal(md5_timeset1, md5_timeset2)
#'
#' # Setting a fixed time
#' (fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET"))
#'   # write 3 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_time)
#' md5_timeset3 <- md5sum(filepath_timeset)
#'   # write 4 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' amend_timestamp(filepath_timeset, fixed_time)
#' md5_timeset4 <- md5sum(filepath_timeset)
#'   # compare:
#' all.equal(md5_timeset3, md5_timeset4)
#'
#' # Also works for GPKG 2D gridded coverage (with stars):
#' library(stars)
#' library(dplyr)
#'
#' filepath_stars <- file.path(tempdir(), "stars_2d.gpkg")
#'
#' stars_2d <-
#' 	system.file("tif/L7_ETMs.tif", package = "stars") %>%
#' 	read_stars() %>%
#' 	slice(band, 1)
#'   # write 1:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' amend_timestamp(filepath_stars, fixed_time)
#' md5_stars1 <- md5sum(filepath_stars)
#'   # write 2:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' amend_timestamp(filepath_stars, fixed_time)
#' md5_stars2 <- md5sum(filepath_stars)
#'   # compare:
#' all.equal(md5_stars1, md5_stars2)
#'
#' @author Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @export
amend_timestamp <- function(dsn,
                            timestamp = Sys.time(),
                            verbose = TRUE) {
    stopifnot(file.exists(dsn))
    stopifnot(is.logical(verbose), !is.na(verbose))
    # soft checking file format:
    if (!grepl("\\.gpkg$", dsn)) {
        stop("Expecting a file with extension '.gpkg'")
    }
    if (!inherits(timestamp, c("Date", "POSIXct"))) {
        stop("timestamp must be a Date or POSIXct object")
    }

    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("Package \"RSQLite\" is needed when using this function. ",
             "Please install it.",
             call. = FALSE)
    }

    timestamp <- format(timestamp,
                        format = "%Y-%m-%dT%H:%M:%S.000Z",
                        tz = "UTC")

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
    updatequery <- sprintf("UPDATE gpkg_contents SET last_change = '%s'",
                           timestamp)
    rows <- RSQLite::dbExecute(con, updatequery)
    if (verbose) {
        message(
            rows,
            " row(s) of the gpkg_contents table have been set with timestamp ",
            timestamp)
    }

    has_metadata <-
        nrow(RSQLite::dbGetQuery(con, "SELECT name FROM sqlite_master
						WHERE name == 'gpkg_metadata_reference'")) > 0
    if (has_metadata) {
        updatequery <-
            sprintf("UPDATE gpkg_metadata_reference SET timestamp = '%s'",
                    timestamp)
        rows <- RSQLite::dbExecute(con, updatequery)
        if (verbose) {
            message(
                rows,
                " row(s) of the gpkg_metadata_reference table have ",
                "been set with timestamp ",
                timestamp)
        }
    }

    RSQLite::dbDisconnect(con)
    return(invisible(NULL))
}
