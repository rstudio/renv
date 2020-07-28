
renv_rstudio_available <- function() {

  # NOTE: detecting whether we're running within RStudio is a bit
  # tricky because not all of the expected RStudio bits have been
  # initialized when the R session is being initialized (e.g.
  # when the .Rprofile is being executed)
  args <- commandArgs(trailingOnly = FALSE)
  args[[1L]] == "RStudio" || .Platform$GUI == "RStudio"

}

renv_rstudio_loading <- function() {
  renv_rstudio_available() && !identical(.Platform$GUI, "RStudio")
}

renv_rstudio_fixup <- function() {

  # if RStudio's tools are on the search path, we should try
  # to fix them up so that renv's own routines don't get seen
  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(FALSE)

  helper <- tools[[".rs.clearVar"]]
  if (is.null(helper))
    return(FALSE)

  # if the helper environment has been fixed up (as e.g. by
  # newer versions of RStudio) then nothing to do
  if (identical(tools, environment(helper)))
    return(FALSE)

  # put common tools from base into the environment
  envir <- environment(helper)
  for (var in c("assign", "exists", "get", "remove", "paste"))
    envir[[var]] <- get(var, envir = baseenv())

  TRUE

}
