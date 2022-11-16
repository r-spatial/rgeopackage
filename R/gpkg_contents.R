#' Read and update GeoPackage contents table
#'
#' @inheritParams read_gpkg_table
#' @export
read_gpkg_contents <- function(dsn,
                               quiet = FALSE) {
  read_gpkg_table(
    dsn = dsn,
    table_name = "gpkg_contents",
    quiet = quiet
  )
}

#' @name update_gpkg_contents
#' @rdname read_gpkg_contents
#' @param identifier Identifier for GeoPackage file.
#' @param description Description for GeoPackage file.
#' @inheritParams amend_timestamp
#' @param title Title for GeoPackage file.
#' @export
#' @importFrom RSQLite dbDisconnect
update_gpkg_contents <- function(dsn,
                                 identifier = NULL,
                                 description = NULL,
                                 timestamp = Sys.time(),
                                 title = identifier,
                                 quiet = FALSE) {
  con <- connect_gpkg(dsn)
  table_name <- "gpkg_contents"
  check_table_exists(con, table_name)

  if (!is.null(identifier)) {
    update_gpkg_table(
      con,
      table_name = table_name,
      statement = glue_sql("SET identifier = {identifier}", .con = con),
      quiet = quiet
    )
  }

  if (!is.null(title)) {
    update_gpkg_table(
      con,
      table_name = table_name,
      statement = glue_sql("SET title = {title}", .con = con),
      quiet = quiet
    )
  }

  if (!is.null(description)) {
    update_gpkg_table(
      con,
      table_name = table_name,
      statement = glue_sql("SET description = {description}", .con = con),
      quiet = quiet
    )
  }

  if (!is.null(timestamp)) {
    timestamp <- fmt_timestamp(timestamp)

    update_gpkg_table(
      con,
      table_name = table_name,
      statement = glue_sql("SET last_change = {timestamp}", .con = con),
      quiet = quiet
    )
  }

  RSQLite::dbDisconnect(con)
  invisible(NULL)
}
