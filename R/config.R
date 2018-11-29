#' Define an renv
#'
#' Define an `renv`.
#'
#' The fields currently supported are:
#'
#' \tabular{lll}{
#' \strong{renv_version}   \tab \code{character[1]} \tab The version of the `renv` package to be used with this project. \cr
#' \strong{r_version}      \tab \code{character[1]} \tab The \R version to be used for this project.    \cr
#' \strong{r_libs}         \tab \code{character[n]} \tab The \R libraries to activate for this project. \cr
#' \strong{r_libs_overlay} \tab \code{logical[1]}   \tab Overlay requested libraries on top of the default R libraries. \cr
#' }
#'
#' @export
renv_config <- function(renv_version   = packageVersion("renv"),
                        r_version      = getRversion(),
                        r_libs         = character(),
                        r_libs_overlay = FALSE)
{
  args <- mget(ls(envir = environment()), envir = environment())
  args <- args[names(formals())]

  defns <- renv_config_definitions()
  enumerate(args, function(key, val) {
    validate <- defns[[key]]$validate
    if (!validate(val)) {
      fmt <- "'%s' is not a valid setting for '%s'"
      stopf(fmt, deparse(val), key)
    }
  })

  args
}

renv_ved_version <- function(comment) {
  list(
    validate = function(x) inherits(x, "numeric_version"),
    encode   = function(x) format(x),
    decode   = function(x) numeric_version(x),
    comment  = comment
  )
}

renv_ved_character <- function(comment) {
  list(
    validate = function(x) is.character(x),
    encode   = function(x) paste(x, collapse = ", "),
    decode   = function(x) strsplit(x, "\\s*,\\s*")[[1]],
    comment  = comment
  )
}

renv_ved_logical <- function(comment) {
  list(
    validate = is.logical,
    encode   = format,
    decode   = as.logical,
    comment  = comment
  )
}

renv_config_definitions <- function() {

  list(
    renv_version   = renv_ved_version("The requested 'renv' version."),
    r_version      = renv_ved_version("The requested R version."),
    r_libs         = renv_ved_character("The requested R libraries"),
    r_libs_overlay = renv_ved_logical("Overlay requested libraries over the default R libraries?")
  )
}

renv_config_read <- function(path) {

  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- grep("^\\w", contents, perl = TRUE, value = TRUE)

  idx <- regexpr(":", lines, fixed = TRUE)

  keys <- trimws(substring(lines, 1, idx - 1))
  vals <- trimws(substring(lines, idx + 2))
  names(vals) <- keys

  defns <- renv_config_definitions()
  config <- enumerate(vals, function(key, val) {
    defns[[key]]$decode(val)
  })

  config

}

renv_config_write <- function(config, path) {

  defns <- renv_config_definitions()
  contents <- enumerate(config, function(key, val) {
    comment <- paste("#", defns[[key]]$comment)
    encoded <- defns[[key]]$encode(val)
    entry <- paste(key, encoded, sep = ": ")
    paste(comment, entry, sep = "\n")
  })

  contents <- paste(contents, collapse = "\n\n")
  writeLines(enc2utf8(contents), con = path, useBytes = TRUE)

}
