
#' Read GeoPackage tables associated with an extension
#'
#' Supports the metadata extension
#' <http://www.geopackage.org/guidance/extensions/metadata.html> and the Schema
#' extension <http://www.geopackage.org/guidance/extensions/schema.html>
#'
#' @param extension Extension name
#' @param table_name One or more table names required for the corresponding
#'   extension.
#' @inheritParams read_gpkg_table
#' @export
#' @importFrom RSQLite dbDisconnect
read_gpkg_extension <- function(dsn = NULL,
                                con = NULL,
                                extension,
                                table_name = NULL,
                                call = .envir,
                                .envir = parent.frame(),
                                ...) {
  con <- connect_gpkg(dsn, con, call, .envir)

  check_gpkg_extension(
    dsn = NULL, con = con,
    extension, table_name, call, .envir
  )

  extension_tables <-
    lapply(
      table_name,
      function(x) {
        read_gpkg_table(
          dsn = NULL, con = con,
          table_name = x, call, .envir
        )
      }
    )

  RSQLite::dbDisconnect(con)
  extension_tables
}

#' @name read_gpkg_schema
#' @rdname read_gpkg_extension
#' @export
read_gpkg_schema <- function(dsn, ...) {
  read_gpkg_extension(
    dsn,
    "gpkg_schema",
    c("gpkg_data_columns", "gpkg_data_column_constraints"),
    ...
  )
}

#' @name read_gpkg_schema
#' @rdname read_gpkg_extension
#' @export
read_gpkg_metadata <- function(dsn, ...) {
  read_gpkg_extension(
    dsn,
    "gpkg_metadata",
    c("gpkg_metadata", "gpkg_metadata_reference"),
    ...
  )
}

#' Check if extension is in gpkg_extensions table and GeoPackage file has
#' extension related table names
#'
#' @param extension Extension name
#' @param table_name One or more table names required for the corresponding
#'   extension.
#' @noRd
#' @importFrom RSQLite dbReadTable dbExistsTable dbDisconnect
check_gpkg_extension <- function(dsn = NULL,
                                 con = NULL,
                                 extension,
                                 table_name = NULL,
                                 call = .envir,
                                 .envir = parent.frame()) {
  con <- connect_gpkg(dsn, con)

  gpkg_extensions <- RSQLite::dbReadTable(con, "gpkg_extensions")

  extension_tables <-
    gpkg_extensions[gpkg_extensions$extension_name %in% extension, ]

  if (nrow(extension_tables) == 0) {
    cli_abort(
      "{.arg extension} {.val {extension}} can't be found
      in the {.val gpkg_extensions} table.",
      call = call,
      .envir = .envir
    )
  }

  has_tables <-
    vapply(
      table_name,
      function(x) {
        RSQLite::dbExistsTable(con, x) &&
          (x %in% extension_tables$table_name)
      },
      FUN.VALUE = TRUE
    )

  RSQLite::dbDisconnect(con)

  if (all(has_tables)) {
    return(invisible(NULL))
  }

  cli_abort(
    "{.arg table_name} {.val {table_name[!has_tables]}} can't be found for extension {.val {extension}}.",
    call = call,
    .envir = .envir
  )
}
