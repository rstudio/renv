

renv_exports_attach <- function() {

  # only done if we're renv
  if (!identical(.packageName, "renv"))
    return()

  # guard against intermediate case where config hasn't been generated
  renv <- asNamespace("renv")
  if (!exists("config", envir = renv))
    return()

  # ignored when running tests and in load_all
  # we don't iterate over `calls` directly to avoid
  # https://github.com/rstudio/renv/issues/930
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (identical(call[[1L]], quote(devtools::test)) ||
        identical(call[[1L]], quote(devtools::document)) ||
        identical(call[[1L]], quote(devtools::load_all)))
      return()
  }

  # read exports
  exports <- renv$config$exported.functions()
  if (identical(exports, "*"))
    return()

  # remove anything that wasn't explicitly exported
  envir <- as.environment("package:renv")
  all <- ls(envir = envir, all.names = TRUE)
  removed <- setdiff(all, exports)
  rm(list = removed, envir = envir, inherits = FALSE)

}
