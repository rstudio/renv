
renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(verbose)

  verbose <- Sys.getenv("RENV_VERBOSE", interactive())
  if (verbose)
    return(TRUE)

  FALSE
}
