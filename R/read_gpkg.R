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
                            call = parent.frame(),
                            quiet = FALSE) {
  con <- connect_gpkg(dsn, con, call)

  if (is.null(table_name)) {
    cli_abort(
      "{.arg table_name} must be provided.",
      call = call
    )
  }

  if (!quiet) {
    cli_inform("Accessing the {.val {table_name}} table.")
  }
  table <- RSQLite::dbReadTable(con, table_name)
  RSQLite::dbDisconnect(con)
  table
}
