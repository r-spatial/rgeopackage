#' Read or list GeoPackage metadata with sf
#'
#' @inheritParams read_gpkg_table
#' @param scope Metadata scope.
#' @param as_xml If `TRUE` (default), parse the metadata to xml with
#'   [xml2::read_xml()]. If the specified scope matches more than one metadata
#'   entry, this function returns a list of xml objects.
#' @export
#' @importFrom rlang arg_match check_installed
read_gpkg_metadata <- function(dsn,
                               scope = "dataset",
                               as_xml = TRUE,
                               ...) {
  scope <-
    rlang::arg_match(
      scope,
      get_spec("gpkg_metadata")[["name_md_scopes"]][["name_md_scope"]]
    )

  metadata <-
    read_gpkg_table(
      dsn,
      name = "gpkg_metadata",
      select = "metadata",
      where = glue("md_scope='{scope}'"),
      ...
    )

  if (!as_xml) {
    return(metadata)
  }

  rlang::check_installed("xml2")
  if (length(metadata[, 1]) == 1) {
    return(xml2::read_xml(metadata[, 1][[1]]))
  }

  map(
    metadata[, 1],
    ~ xml2::read_xml(
      .x
    )
  )
}

#' @rdname read_gpkg_metadata
#' @name list_gpkg_metadata
#' @export
list_gpkg_metadata <- function(dsn,
                               ...) {
    read_gpkg_table(dsn, name = "gpkg_metadata", ...)
}
