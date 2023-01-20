.onLoad <- function(lib, pkg) {
  utils::data(
    list = c(
      "gpkg_extensions",
      "community_gpkg_extensions"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

#' Is x an GeoPackage filename or path?
#'
#' @param x Character string to check if file name or path name
#' @noRd
is_gpkg <- function(x, ignore.case = TRUE) {
  grepl("\\.gpkg$", x, ignore.case = ignore.case)
}

#' Check if x is a GeoPackage file
#'
#' @inheritParams is_gpkg
#' @inheritParams cli::cli_abort
#' @noRd
#' @importFrom rlang caller_arg
check_gpkg <- function(x,
                       arg = caller_arg(x),
                       call = parent.env()) {
  if (!is_gpkg(x)) {
    cli_abort(
      "{.arg {arg}} must be a {.file *.gpkg} (GeoPackage) file.",
      call = call
    )
  }

  if (!file.exists(x)) {
    cli_abort(
      "{.arg {arg}} must be a filename or path to an existing file.",
      call = call
    )
  }
  invisible(NULL)
}

#' Connect to GeoPackage data source
#'
#' @param dsn Path or url for GeoPackage file. Optional if con is provided.
#' @param con Connection from [DBI::dbConnect()]. Optional if dsn is provided.
#' @inheritParams cli::cli_abort
#' @export
#' @importFrom RSQLite dbConnect
connect_gpkg <- function(dsn = NULL,
                         con = NULL,
                         call = parent.frame()) {
  if (!is.null(dsn)) {
    check_gpkg(dsn, call = call)

    if (!is.null(con)) {
      cli_abort(
        "Exactly one of {.arg con} or {.arg dsn} must be supplied.",
        call = call
      )
    }
  }

  con %||% RSQLite::dbConnect(RSQLite::SQLite(), dsn)
}


#' Does the named table exist for this connection?
#'
#' @noRd
check_table_exists <- function(con,
                               table_name = NULL,
                               call = parent.frame()) {
  if (!is.null(table_name) && RSQLite::dbExistsTable(con, table_name)) {
    return(invisible(NULL))
  }

  cli_abort(
    "Table {.val {table_name}} can't be found for the provided
        {.arg dsn} or {.arg con}.",
    call = call
  )
}

#' Does the query return the same or greater than the minimum number of rows?
#'
#' @noRd
#' @importFrom RSQLite dbGetQuery
has_query_min_rows <- function(con,
                               query = NULL,
                               min_rows = 1) {
  nrow(RSQLite::dbGetQuery(con, glue_sql(query, .con = con))) >= min_rows
}
