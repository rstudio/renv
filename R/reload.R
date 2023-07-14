
reload <- function() {

  # remove running tasks, etc.
  renv_task_unload()
  renv_watchdog_unload()

  # unload renv
  renv_namespace_unload("renv")

  # try loading renv in an exit handler
  requireNamespace("renv", quietly = TRUE)

}
