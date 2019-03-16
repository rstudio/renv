
renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(verbose)

  verbose <- Sys.getenv("RENV_VERBOSE", unset = NA)
  if (!is.na(verbose))
    return(TRUE)

  interactive()

}

renv_verbose_with <- function(verbose, expr) {
  renv.verbose <- getOption("renv.verbose")
  options(renv.verbose = verbose)
  on.exit(options(renv.verbose = renv.verbose), add = TRUE)
  expr
}
