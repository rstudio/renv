
renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(verbose)

  verbose <- Sys.getenv("RENV_VERBOSE", interactive())
  if (verbose)
    return(TRUE)

  FALSE
}

renv_verbose_with <- function(verbose, expr) {
  renv.verbose <- getOption("renv.verbose")
  options(renv.verbose = verbose)
  on.exit(options(renv.verbose = renv.verbose), add = TRUE)
  expr
}
