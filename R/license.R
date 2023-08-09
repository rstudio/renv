
# used to generate the CRAN-compatible license file in R CMD build
renv_license_generate <- function() {

  # only done if we're building
  if (!building())
    return(FALSE)

  contents <- c(
    paste("YEAR:", format(Sys.Date(), "%Y")),
    "COPYRIGHT HOLDER: Posit Software, PBC"
  )

  writeLines(contents, con = "LICENSE")
  return(TRUE)

}

if (identical(.packageName, "renv"))
  renv_license_generate()

