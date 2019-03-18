
renv_debugging <- function() {

  debugging <- Sys.getenv("RENV_DEBUGGING", unset = NA)
  if (is.na(debugging))
    return(FALSE)

  parsed <- parse(text = debugging)[[1]]
  as.logical(parsed)

}
