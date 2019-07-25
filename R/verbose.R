
renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(as.logical(verbose))

  verbose <- Sys.getenv("RENV_VERBOSE", unset = NA)
  if (!is.na(verbose))
    return(as.logical(verbose))

  interactive() || !renv_testing()

}
