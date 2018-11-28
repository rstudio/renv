#' Define an renv
#'
#' Define an `renv`.
#'
#' The fields currently supported are:
#'
#' \tabular{ll}{
#' \strong{r_version} \tab The \R version to be used for this project.    \cr
#' \strong{r_libs}    \tab The \R libraries to activate for this project. \cr
#' }
#'
#' @export
renv_config <- function(r_version = getRversion(),
                        r_libs    = NULL)
{
  list(
    r_version = r_version,
    r_libs    = r_libs
  )
}

renv_config_read <- function(path) {

  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  idx <- regexpr(":", contents, fixed = TRUE)

  keys <- trimws(substring(contents, 1, idx - 1))
  vals <- trimws(substring(contents, idx + 2))

  config <- lapply(vals, function(val) {
    strsplit(val, "\\s*,\\s*")[[1]]
  })
  names(config) <- renv_config_translate(keys)
  config

}

renv_config_write <- function(config, path) {

  keys <- pad_right(renv_config_translate(names(config)))
  vals <- lapply(config, paste, collapse = ", ")
  contents <- paste(keys, vals, sep = " : ")

  writeLines(enc2utf8(contents), con = path, useBytes = TRUE)

}

renv_config_translate <- function(entries) {

  aliases <- bimap(
    "R Version"   = "r_version",
    "R Libraries" = "r_libs"

  )

  as.character(aliases[entries])

}
