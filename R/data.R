#' Registered GeoPackage Extensions
#'
#' Reference table of registered GeoPackage extensions from
#' <http://www.geopackage.org/guidance/getting-started.html#registered-extensions>.
#' Note that the information in the reference column is an incomplete
#' representation of the requirements for extension implementation. This data is
#' provided to support a (yet to be developed) functions that allow the addition
#' of extensions to exported GeoPackage files.
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{`pos`}{Position/order}
#'   \item{`extension`}{Extension title}
#'   \item{`url`}{Detail URL}
#'   \item{`reference`}{List column with reference tables for
#'   gpkg_extensions and other extension-specific tables.}
#' }
"gpkg_extensions"

#' Community GeoPackage Extensions
#'
#' Reference table of community GeoPackage extensions from
#' <http://www.geopackage.org/extensions.html>. Note that the information in the
#' reference column is an incomplete representation of the requirements for
#' extension implementation. This data is provided to support a (yet to be
#' developed) functions that allow the addition of extensions to exported
#' GeoPackage files.
#'
#' @format A data frame with 15 rows and 5 variables:
#' \describe{
#'   \item{`pos`}{Position/order}
#'   \item{`extension`}{Extension title}
#'   \item{`description`}{Extension description}
#'   \item{`url`}{Detail URL}
#'   \item{`reference`}{List column with reference tables for
#'   gpkg_extensions and other extension-specific tables.}
#' }
"community_gpkg_extensions"
