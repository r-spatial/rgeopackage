#' @importFrom rlang caller_env `%||%`
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom glue glue glue_sql
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
