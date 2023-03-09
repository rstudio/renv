
renv_task_create <- function(callback, name = NULL) {

  # create name for task callback
  name <- name %||% as.character(substitute(callback))
  id <- paste("renv", name, sep = ":::")

  # remove an already-existing task of the same name
  removeTaskCallback(id)

  # otherwise, add our new task
  addTaskCallback(
    renv_task_callback(callback, name),
    name = name
  )

}

renv_task_remove <- function(callback, name = NULL) {

  # create name for task callback
  name <- name %||% as.character(substitute(callback))
  id <- paste("renv", name, sep = ":::")

  # remove it
  removeTaskCallback(id)

}

renv_task_callback <- function(callback, name) {

  force(callback)
  force(name)

  function(...) {

    status <- tryCatch(callback(), error = identity)
    if (inherits(status, "error")) {
      fmt <- "renv background task '%s' failed; it will be stopped"
      msg <- sprintf(fmt, name)
      warning(msg, call. = FALSE)
      return(FALSE)
    }

    TRUE

  }

}

renv_task_unload <- function() {
  callbacks <- getTaskCallbackNames()
  for (callback in callbacks)
    if (startswith(callback, "renv:::"))
      removeTaskCallback(callback)
}
