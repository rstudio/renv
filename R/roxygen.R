
#' @param project The project directory. If `NULL`, then the active project will
#'   be used. If no project is currently active, then the current working
#'   directory is used instead.
#'
#' @param type The type of package to install ("source" or "binary"). Defaults
#'   to the value of `getOption("pkgType")`.
#'
#' @param library The \R library to be used. When `NULL`, the active project
#'  library will be used instead.
#'
#' @param prompt Boolean; prompt the user before taking any action? For backwards
#'   compatibility, `confirm` is accepted as an alias for `prompt`.
#'
#' @param ... Unused arguments, reserved for future expansion. If any arguments
#'   are matched to `...`, `renv` will signal an error.
#'
#' @param clean Boolean; remove packages not recorded in the lockfile from
#'   the target library? Use `clean = TRUE` if you'd like the library state
#'   to exactly reflect the lockfile contents after `restore()`.
#'
#' @param rebuild Force packages to be rebuilt, thereby bypassing any installed
#'   versions of the package available in the cache? This can either be a
#'   boolean (indicating that all installed packages should be rebuilt), or a
#'   vector of package names indicating which packages should be rebuilt.
#'
#' @return The project directory, invisibly. Note that this function is normally
#'   called for its side effects.
#'
#' @name renv-params
NULL

#' @param library The library into which packages should be installed.
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as required.
#'
#' @name install-params
NULL

renv_roxygen_config_section <- function() {

  # read config
  config <- yaml::read_yaml("inst/config.yml")

  # generate items
  items <- map_chr(config, function(entry) {

    # extract fields
    name <- entry$name
    type <- entry$type
    default <- entry$default
    description <- entry$description

    # deparse default value
    default <- case(
      identical(default, list()) ~ "NULL",
      TRUE                       ~ deparse(default)
    )

    # generate table row
    fmt <- "\\item{\\bold{\\code{renv.config.%s}}}{%s Defaults to \\code{%s}.}"
    sprintf(fmt, name, description, default)

  })

  c(
    "@section Configuration:",
    "",
    "The following `renv` configuration options are available:",
    "",
    "\\describe{",
    items,
    "}",
    ""
  )

}
