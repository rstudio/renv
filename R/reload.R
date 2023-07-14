
reload <- function() {

  # remove running tasks, etc.
  renv_task_unload()
  renv_watchdog_unload()

  # unload renv
  renv_namespace_unload("renv")

  # reload renv
  requireNamespace("renv", quietly = TRUE)

}
