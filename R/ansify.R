
ansify <- function(text) {
  if (renv_ansify_enabled())
    renv_ansify_enhanced(text)
  else
    renv_ansify_default(text)
}

renv_ansify_enabled <- function() {

  override <- Sys.getenv("RENV_ANSIFY_ENABLED", unset = NA)
  if (!is.na(override))
    return(as.logical(override))

  pane <- Sys.getenv("RSTUDIO_CHILD_PROCESS_PANE", unset = NA)
  if (identical(pane, "build"))
    return(FALSE)

  testthat <- Sys.getenv("TESTTHAT", unset = "false")
  if (tolower(testthat) %in% "true")
    return(FALSE)

  iderun <- Sys.getenv("R_CLI_HAS_HYPERLINK_IDE_RUN", unset = "false")
  if (tolower(iderun) %in% "false")
    return(FALSE)

  TRUE

}

renv_ansify_default <- function(text) {
  text
}

renv_ansify_enhanced <- function(text) {

  # R help links
  pattern <- "`\\?(renv::(?:[^`])+)`"
  replacement <- "`\033]8;;ide:help:\\1\a?\\1\033]8;;\a`"
  text <- gsub(pattern, replacement, text, perl = TRUE)

  # runnable code
  pattern <- "`(renv::(?:[^`])+)`"
  replacement <- "`\033]8;;ide:run:\\1\a\\1\033]8;;\a`"
  text <- gsub(pattern, replacement, text, perl = TRUE)

  # return ansified text
  text

}

renv_ansify_init <- function() {

  envir <- renv_envir_self()
  if (renv_ansify_enabled())
    assign("ansify", renv_ansify_enhanced, envir = envir)
  else
    assign("ansify", renv_ansify_default, envir = envir)

}
