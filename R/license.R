
# used to generate the CRAN-compatible license file in R CMD build
renv_license_generate <- function() {

  isbuild <-
    renv_envvar_exists("R_CMD") &&
    grepl("Rbuild", basename(dirname(getwd())))

  if (!isbuild)
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

