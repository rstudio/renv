
# used to generate the CRAN-compatible license file in R CMD build
renv_license_generate <- function() {

  isbuild <-
    !is.na(Sys.getenv("R_CMD", unset = NA)) &&
    grepl("Rbuild", getwd())

  if (!isbuild)
    return(FALSE)

  contents <- c(
    paste("YEAR:", format(Sys.Date(), "%Y")),
    "COPYRIGHT HOLDER: RStudio, PBC"
  )

  writeLines(contents, con = "LICENSE")

  return(TRUE)

}

renv_license_generate()
