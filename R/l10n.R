
renv_l10n_mbcs <- function() {
  info <- l10n_info()
  info$MBCS
}

renv_l10n_utf8 <- function() {
  info <- l10n_info()
  info$`UTF-8`
}

renv_l10n_latin1 <- function() {
  info <- l10n_info()
  info$`Latin-1`
}
