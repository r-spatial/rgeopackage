#' Read GeoPackage tables associated with an extension
#'
#' Supports the metadata extension
#' <http://www.geopackage.org/guidance/extensions/metadata.html> and the Schema
#' extension <http://www.geopackage.org/guidance/extensions/schema.html>
#'
#' @param extension Extension name. Required.
#' @param table_name One or more table names required for the corresponding
#'   extension. If `NULL`, there is no check to
#' @inheritParams read_gpkg_table
#' @param ... Additional parameters passed to [read_gpkg_extension()] by
#'   [read_gpkg_metadata()], [read_gpkg_schema()], or
#'   [read_gpkg_related_tables()].
#' @return A list of dataframes including any available table associated with
#'   the named extension.
#' @export
#' @importFrom RSQLite dbDisconnect
read_gpkg_extension <- function(dsn = NULL,
                                con = NULL,
                                extension,
                                table_name = NULL,
                                call = parent.frame()) {
  con <- connect_gpkg(dsn, con, call)

  check_gpkg_extension(
    dsn = NULL, con = con,
    extension, table_name,
    TRUE, call
  )

  extension_tables <-
    lapply(
      table_name,
      function(x) {
        read_gpkg_table(
          dsn = NULL, con = con,
          table_name = x, call
        )
      }
    )

  RSQLite::dbDisconnect(con)

  if (length(extension_tables) == length(table_name)) {
    names(extensions_tables) <- table_name
  }

  extensions_tables
}

#' @name read_gpkg_schema
#' @rdname read_gpkg_extension
#' @export
read_gpkg_metadata <- function(dsn, ...) {
  extension <- "gpkg_metadata"
  read_gpkg_extension(
    dsn,
    extension = extension,
    table_name = extension_table_names(extension),
    ...
  )
}

#' @name read_gpkg_schema
#' @rdname read_gpkg_extension
#' @export
read_gpkg_schema <- function(dsn, ...) {
  extension <- "gpkg_schema"
  read_gpkg_extension(
    dsn,
    extension = extension,
    table_name = extension_table_names(extension),
    ...
  )
}

#' @name read_gpkg_related_tables
#' @rdname read_gpkg_extension
#' @export
read_gpkg_related_tables <- function(dsn, ...) {
  extension <- "gpkg_related_tables"
  read_gpkg_extension(
    dsn,
    extension = extension,
    table_name = extension_table_names(extension),
    ...
  )
}

#' Check if extension is in gpkg_extensions table and GeoPackage file has
#' extension related table names
#'
#' @param extension Extension name. Required.
#' @param table_name One or more table names required for the corresponding
#'   extension. Required if check_table is `TRUE`.
#' @param check_table If `TRUE` (default), error if table_name is `NULL`. If
#'   `FALSE`, do not check for existence of table regardless of table_name
#'   value.
#' @noRd
#' @importFrom RSQLite dbReadTable dbExistsTable dbDisconnect
check_gpkg_extension <- function(dsn = NULL,
                                 con = NULL,
                                 extension,
                                 table_name,
                                 check_table = TRUE,
                                 call = parent.frame()) {
  con <- connect_gpkg(dsn, con)

  con_gpkg_extensions <- RSQLite::dbReadTable(con, "gpkg_extensions")

  if (missing(extension)) {
    cli_abort(
      "{.arg extension} must be provided.",
      call = call
    )
  }

  extension_tables <-
    con_gpkg_extensions[con_gpkg_extensions$extension_name %in% extension, ]

  if (nrow(extension_tables) == 0) {
    cli_abort(
      "{.arg extension} {.val {extension}} can't be found
      in the {.val gpkg_extensions} table.",
      call = call
    )
  }

  if (!check_table) {
    return(invisible(NULL))
  } else if (is.null(table_name)) {
    cli_abort(
      "{.arg table_name} must be provided.",
      call = call
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
    "{.arg table_name} {.val {table_name[!has_tables]}}
    can't be found for extension {.val {extension}}.",
    call = call
  )
}


#' Get extension table names from the reference table of registered or community
#' GeoPackage extensions
#'
#' @keywords internal
extension_table_names <- function(extension,
                                  community = FALSE,
                                  exclude = "gpkg_extensions") {
  table_ref <- extension_tables(extension, community)
  table_names <- names(table_ref)

  if (is.null(exclude)) {
    return(table_names)
  }

  table_names[!(table_names %in% exclude)]
}

#' Get reference tables from gpkg_extensions or community_gpkg_extensions
#'
#' @keywords internal
extension_tables <- function(extension,
                             community = FALSE) {
  if (community) {
    community_gpkg_extensions$reference[[extension]]
  } else {
    gpkg_extensions$reference[[extension]]
  }
}

#' Use a GeoPackage extension for a file
#'
#' Add an entry to the "gpkg_extensions" table for a file.
#'
#' @inheritParams connect_gpkg
#' @param extension Extension name to use. Required.
#' @param table_name One or more table names to add to "gpkg_extensions" table.
#' @param column_name Column name.
#' @param definition Table definition. Required.
#' @param scope Table scope. Required. If length 1, scope is recycled for all
#'   tables.
#' @noRd
use_gpkg_extension <- function(dsn = NULL,
                               con = NULL,
                               extension,
                               table_name = NULL,
                               column_name = NULL,
                               definition = NULL,
                               scope = NULL,
                               reference = TRUE) {
  con <- connect_gpkg(dsn, con)

  if (reference) {
    if (!is.null(table_name)) {
      cli_abort(
        "{.arg table_name} must be {.code NULL}
          if {.arg reference} is {.code TRUE}."
      )
    }

    extension_ref <- extension_tables(extension)[["gpkg_extensions"]]
    table_name <- extension_ref$table_name

    if (any(vapply(c(column_name, definition, scope), is.null, TRUE))) {
      overwrite_arg <- c("column_name", "definition", "scope")
      cli_warn(
        "Replacing provided values for {.arg {overwrite_arg}}
        with reference values."
      )
    }

    column_name <- extension_ref$column_name
    definition <- extension_ref$definition
    scope <- extension_ref$scope
  }

  num_tables <- length(table_name)

  if ((length(scope) == 1) && num_tables > 1) {
    scope <- rep(scope, num_tables)
  }

  multiple <- FALSE
  if (length(scope) > 1) {
    multiple <- TRUE
  }

  scope_opts <- c("read-write", "read-only", "write-only")
  scope <- match.arg(scope, scope_opts, multiple)

  if (!is.null(definition) && (num_tables > 0) && (length(definition) != num_tables)) {
    cli_abort(
      "{.arg definition} length must match {.arg table_name} length."
    )
  }

  gpkg_extensions_rows <-
    list(
      "table_name" = table_name,
      "column_name" = column_name,
      "definition" = definition,
      "extension_name" = rep(extension, num_tables),
      "scope" = scope
    )

  append_gpkg_table(
    dsn = NULL,
    con = con,
    table_name = "gpkg_extensions",
    table_data = as.data.frame(gpkg_extensions_rows)
  )

  RSQLite::dbDisconnect(con)
}
