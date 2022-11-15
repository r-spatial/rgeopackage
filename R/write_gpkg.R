#' Create, append, or update a GeoPackage table
#'
#' @inheritParams read_gpkg_table
#' @inheritParams DBI::dbWriteTable
#' @inheritDotParams DBI::dbWriteTable
#' @name write_gpkg_table
#' @export
#' @importFrom RSQLite dbWriteTable dbDisconnect
write_gpkg_table <- function(dsn = NULL,
                             con = NULL,
                             table_name = NULL,
                             table_data,
                             overwrite = FALSE,
                             append = FALSE,
                             header = TRUE,
                             temporary = FALSE,
                             quiet = FALSE,
                             call = parent.frame(),
                             ...) {
  con <- connect_gpkg(dsn, con, call)
  if (is.list(table_data) && !is.data.frame(table_data)) {
    if (!header) {
      if (!quiet) {
        cli_inform(
          c(
            "*" = "{.arg header} must be {.code TRUE} if
                  {.arg table_data} is a named {.cls list}.",
            " " = "{.arg header} changed to {.code TRUE}."
          )
        )
      }
      header <- TRUE
    }
  }

  table_data <- check_table_data(table_data)

  # if (overwrite | append) {
  #   check_table_exists(con, table_name)
  # }

  RSQLite::dbWriteTable(
    con = con,
    name = table_name,
    value = table_data,
    overwrite = overwrite,
    append = append,
    header = header,
    temporary = temporary,
    ...
  )
  RSQLite::dbDisconnect(con)
}

#' @name update_gpkg_table
#' @rdname write_gpkg_table
#' @export
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

#' @name append_gpkg_table
#' @rdname write_gpkg_table
#' @export
#' @importFrom RSQLite dbAppendTable dbDisconnect
append_gpkg_table <- function(dsn = NULL,
                              con = NULL,
                              table_name,
                              table_data,
                              call = parent.frame(),
                              ...) {
  con <- connect_gpkg(dsn, con, call)
  RSQLite::dbAppendTable(con, table_name, table_data, ...)
  RSQLite::dbDisconnect(con)
}

#' @name create_gpkg_table
#' @rdname write_gpkg_table
#' @inheritParams DBI::dbCreateTable
#' @export
#' @importFrom RSQLite dbCreateTable dbExistsTable dbDisconnect
create_gpkg_table <- function(dsn = NULL,
                              con = NULL,
                              table_name,
                              fields,
                              quiet = FALSE,
                              temporary = FALSE,
                              ...) {
  con <- connect_gpkg(dsn, con)
  RSQLite::dbCreateTable(
    con = con,
    name = table_name,
    fields = fields,
    temporary = temporary,
    ...
  )

  if (!quiet) {
    filename <- "file"
    if (!is.null(dsn)) {
      filename <- "{.file {basename(dsn)}}"
    }

    if (RSQLite::dbExistsTable(con, table_name)) {
      cli_inform(
        c("v" = paste0("Successfully created {.val {table_name}}
              in ", filename, "."))
      )
    }
  }
  RSQLite::dbDisconnect()
}

#' Check if table_data is a named list or data frame
#'
#' @noRd
#' @importFrom rlang is_named set_names
check_table_data <- function(table_data,
                             call = parent.frame()) {
  if (!is.data.frame(table_data)) {
    if (is.list(table_data) && rlang::is_named(table_data)) {
      table_data <-
        rlang::set_names(
          as.data.framel(table_data),
          names(table_data)
        )
    } else {
      cli_abort(
        "{.arg table_data} must be a named {.cls list} or
        {.cls data.frame}, not a {.cls {class(table_data)}} object.",
        call = call
      )
    }
  }

  table_data
}
