
renv_exports_attach <- function() {

  # guard against intermediate case where config hasn't been generated
  renv <- asNamespace("renv")
  if (!exists("config", envir = renv))
    return()

  # ignored when running tests
  calls <- sys.calls()
  for (call in calls)
    if (identical(call[[1L]], quote(devtools::test)) ||
        identical(call[[1L]], quote(devtools::document)))
      return()

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
