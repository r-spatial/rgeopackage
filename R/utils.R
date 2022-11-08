#' Is x an GeoPackage filename or path?
#'
#' @param x File name or path name
#' @noRd
is_gpkg <- function(x) {
  grepl("\\.gpkg$", x)
}

#' Check if x is a GeoPackage file
#'
#' @inheritParams is_gpkg
#' @inheritParams cli::cli_abort
#' @noRd
#' @importFrom rlang caller_arg
check_gpkg <- function(x,
                       arg = caller_arg(),
                       call = .envir,
                       .envir = parent.frame()) {
  if (!file.exists(x)) {
    cli_abort(
      "{.arg {arg}} must be a valid filename or path.",
      call = call,
      .envir = .envir
    )
  }

  if (!is_gpkg(x)) {
    cli_abort(
      "{.arg {arg}} must be a GeoPackage (gpkg) file.",
      call = call,
      .envir = .envir
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
                         call = .envir,
                         .envir = parent.frame()) {
  if (!is.null(dsn)) {
    check_gpkg(dsn, call = call, .envir = .envir)

    if (!is.null(con)) {
      cli_abort(
        "Exactly one of {.arg con} or {.arg dsn} must be supplied.",
        call = call,
        .envir = .envir
      )
    }
  }

  con %||% RSQLite::dbConnect(RSQLite::SQLite(), dsn)
}

#' Does the query return the same or greater than the minimum number of rows?
#'
#' @noRd
#' @importFrom RSQLite dbGetQuery
has_query_rows <- function(con,
                           query = NULL,
                           min_rows = 1) {
  nrow(RSQLite::dbGetQuery(con, glue_sql(query, .con = con))) >= min_rows
}
