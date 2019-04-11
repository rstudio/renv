
#' Install Required System Libraries
#'
#' Equip your system with libraries commonly-used during compilation of
#' \R packages.
#'
#' @export
equip <- function() {
  renv_scope_error_handler()

  case(
    renv_platform_windows() ~ renv_equip_windows(),
    renv_platform_macos()   ~ renv_equip_macos(),
    renv_platform_linux()   ~ renv_equip_linux()
  )
}

renv_equip_windows <- function() {
  renv_extsoft_install()
  renv_extsoft_use()
}

renv_equip_macos <- function() {
  stopf("renv::equip() not yet implemented for macOS")
}

renv_equip_linux <- function() {
  stopf("renv::equip() not yet implemented for Linux")
}
