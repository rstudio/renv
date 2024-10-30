
renv_task_create <- function(callback, name = NULL) {

  # create name for task callback
  name <- name %||% as.character(substitute(callback))
  name <- paste("renv", name, sep = ":::")

  # remove an already-existing task of the same name
  removeTaskCallback(name)

  # otherwise, add our new task
  addTaskCallback(
    renv_task_callback(callback, name),
    name = name
  )

}

renv_task_callback <- function(callback, name) {

  force(callback)
  force(name)

  function(...) {

    status <- tryCatch(callback(), error = identity)
    if (inherits(status, "error")) {
      caution("Error in background task '%s': %s", name, conditionMessage(status))
      caution("Background task '%s' will be stopped.", name)
      return(FALSE)
    }

    TRUE

  }

}

renv_task_unload <- function() {
  callbacks <- getTaskCallbackNames()
  for (callback in callbacks)
    for (prefix in c("renv_", "renv:::"))
      if (startsWith(callback, prefix))
        removeTaskCallback(callback)
}
