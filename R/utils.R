#' Connect to GeoPackage data source with RSQLite
#'
#' @param dsn Path or url for GeoPackage file. Optional if con is provided.
#' @param con Connection from [DBI::dbConnect()]. Optional if dsn is provided.
#' @inheritParams cli::cli_abort
#' @export
connect_gpkg <- function(dsn = NULL,
                         con = NULL,
                         call = parent.frame()) {
    rlang::check_installed("RSQLite")
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


#' Does the query return the same or greater than the minimum number of rows?
#'
#' @noRd
has_query_min_rows <- function(con,
                               query = NULL,
                               min_rows = 1) {
    rlang::check_installed("RSQLite")
    nrow(RSQLite::dbGetQuery(con, glue_sql(query, .con = con))) >= min_rows
}


#' Update a GeoPackage table with RSQLite
#'
#' @noRd
update_gpkg_table <- function(con,
                              table_name = NULL,
                              statement = NULL,
                              message = NULL,
                              quiet = FALSE,
                              .envir = parent.frame()) {
    rlang::check_installed("RSQLite")
    statement <- glue_sql("UPDATE {`table_name`} {statement}", .con = con)
    n_rows <- RSQLite::dbExecute(con, statement)
    if (!quiet) {
        if (is.null(message)) {
            cli_inform(
                "Updated {n_rows} row{?s} in the {.val {table_name}} table."
            )
        } else {
            cli_inform(
                message,
                .envir = .envir
            )
        }
    }
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
}


#' @noRd
get_spec <- function(table_name = NULL, v = "1.3.1", ...) {
    if (v != "1.3.1") {
        cli_warn(
            "{.pkg rgeopackage} supports version: 1.3.1 of the OGC GeoPackage spec."
        )
    }
    spec131[[table_name]]
}


#' @noRd
#' @importFrom rlang as_function global_env
map <- function(.x, .f, ...) {
    .f <- rlang::as_function(.f, env = rlang::global_env())
    lapply(.x, .f, ...)
}
