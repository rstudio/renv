
#' Install Required System Libraries
#'
#' Equip your system with libraries commonly-used during compilation of
#' \R packages. Currently only supported on Windows.
#'
#' @return This function is normally called for its side effects.
#' @export
#'
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
