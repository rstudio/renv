
#' Install required system libraries
#'
#' Equip your system with libraries commonly-used during compilation of
#' base and recommended \R packages. This was previously useful with older
#' versions of R on windows, but is no longer terribly helpful.
#'
#' @return This function is normally called for its side effects.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' # download useful build tools
#' renv::equip()
#'
#' }
equip <- function() {

  renv_scope_error_handler()

  case(
    renv_platform_windows() ~ renv_equip_windows(),
    renv_platform_macos()   ~ renv_equip_macos(),
    renv_platform_linux()   ~ renv_equip_linux()
  )

  invisible(NULL)

}

renv_equip_windows <- function() {
  invisible(renv_extsoft_install() && renv_extsoft_use())
}

renv_equip_linux <- function() {
  stopf("renv::equip() not yet implemented for Linux")
}
