
#' @param project The project directory. If `NULL`, then the active project will
#'   be used. If no project is currently active, then the current working
#'   directory is used instead.
#'
#' @param library The \R library to be used. When `NULL`, the active project
#'  library will be used instead.
#'
#' @param confirm Boolean; prompt the user before taking any action?
#'
#' @param ... Unused arguments, reserved for future expansion. If any arguments
#'   are matched to `...`, `renv` will signal an error.
#'
#' @return The project directory, invisibly. Note that this function is normally
#'   called for its side effects.
#'
#' @name renv-params
NULL

#' @param library The library into which packages should be installed.
#'
#' @param rebuild Force packages to be rebuilt, thereby bypassing any installed
#'   versions of the package available in the cache? This can either be a
#'   boolean (indicating that the requested package(s) should be rebuilt), or a
#'   vector of package names indicating which packages should be rebuilt.
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as required.
#'
#' @name install-params
NULL
