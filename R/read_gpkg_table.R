#' Read a GeoPackage table with sf
#'
#' @param dsn Data source name. Must be a GeoPackage file.
#' @param name Name of a GeoPackage table to read.
#' @param query If `NULL`, query defaults to "SELECT {select} FROM {name}". If
#'   query is provided, select and name are ignored.
#' @param where If not `NULL`, where is appended to the end of query as "{query}
#'   WHERE {where}".
#' @param call Passed to [rlang::check_required()]
#' @param ... Additional parameters passed to [sf::read_sf()]. Not required.
#' @export
#' @importFrom rlang check_required check_installed
read_gpkg_table <- function(dsn,
                            name,
                            query = NULL,
                            select = NULL,
                            where = NULL,
                            call = parent.frame(),
                            ...) {
  rlang::check_required(dsn, call = call)
  if (is.null(query)) {
    rlang::check_required(name, call = call)
    select <- select %||% "*"
    query <- glue("SELECT {select} FROM {name}")
  }

  if (!is.null(where)) {
    query <- glue("{query} WHERE {where}")
  }

  rlang::check_installed("sf", call = call)
  sf::read_sf(
    dsn,
    query = query,
    drivers = "GPKG",
    ...
  )
}
