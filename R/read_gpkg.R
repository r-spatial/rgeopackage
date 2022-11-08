#' Read a GeoPackage table
#'
#' @inheritParams connect_gpkg
#' @param table_name Name of a GeoPackage table to read
#' @param quiet If `TRUE`, do not show informational messages. Defaults to
#'   `FALSE`.
#' @export
read_gpkg_table <- function(dsn = NULL,
                            con = NULL,
                            table_name = NULL,
                            call = .envir,
                            .envir = parent.frame(),
                            quiet = FALSE) {
  con <- connect_gpkg(dsn, con, call, .envir)

  if (is.null(table_name)) {
    cli_abort(
      "{.arg table_name} must be provided.",
      call = call,
      .envir = .envir
    )
  }

  if (!quiet) {
    cli_inform("Accessing the {.val {table_name}} table.")
  }
  table <- RSQLite::dbReadTable(con, table_name)
  RSQLite::dbDisconnect(con)
  table
}


#' Does the named table exist for this connection?
#'
#' @noRd
check_table_exists <- function(con,
                               table_name = NULL,
                               call = .envir,
                               .envir = parent.frame()) {
    if (!is.null(table_name) && RSQLite::dbExistsTable(con, table_name)) {
        return(invisible(NULL))
    }

    cli_abort(
        "Table {.val {table_name}} can't be found for the provided
        {.arg dsn} or {.arg con}.",
        call = call,
        .envir = .envir
    )
}

#' Update a selected table using a provided query string
#'
#' @noRd
#' @importFrom RSQLite dbExecute
update_gpkg_table <- function(con,
                              table_name = NULL,
                              statement = NULL,
                              message = NULL,
                              quiet = FALSE,
                              .envir = parent.frame()) {
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

