
job <- function(callback, data = list()) {

  # unquote things in the callback
  body(callback) <- renv_expr_impl(body(callback), envir = parent.frame())

  # set up job directory
  jobdir <- tempfile("renv-job-")
  ensure_directory(jobdir)

  # set up paths
  paths <- list(
    options   = file.path(jobdir, "options.rds"),
    workspace = file.path(jobdir, "workspace.Rdata"),
    script    = file.path(jobdir, "script.R"),
    result    = file.path(jobdir, "result.rds")
  )

  # save options
  names <- list("download.file.method", "download.file.extra", "pkgType", "repos")
  saveRDS(do.call(options, names), file = paths$options)

  # save callback and data
  save(callback, data, file = paths$workspace)

  # find path where renv is installed
  library <- if (devmode() || testing()) {
    dirname(renv_package_find("renv"))
  } else {
    dirname(renv_namespace_path("renv"))
  }

  # create a script that will load this data and run it
  code <- expr({

    # load renv, and make internal functions visible
    renv <- loadNamespace("renv", lib.loc = !!library)
    renv$summon()

    # invoke the provided callback
    result <- catch({
      options(readRDS(!!paths$options))
      base::load(!!paths$workspace)
      do.call(callback, data)
    })

    # write result to file
    saveRDS(result, file = !!paths$result)

  })

  # write code to script
  writeLines(deparse(code), con = paths$script)

  # run that code
  renv_scope_envvars(RENV_WATCHDOG_ENABLED = FALSE)
  args <- c("--vanilla", "-s", "-f", renv_shell_path(paths$script))
  status <- r(args)
  if (status != 0L)
    stopf("error executing job [error code %i]", status)

  # collect the result
  result <- readRDS(paths$result)
  if (inherits(result, "error"))
    stop(result)

  result

}
