
if (interactive()) {

  # create a 'done' object that, when printed, will
  # run any pending defer handlers
  envir <- attach(NULL, name = "tools:renv")
  envir$done <- structure(list(), class = "renv_done")
  registerS3method("print", "renv_done", function(x, ...) {
    renv:::renv_defer_execute(teardown_env())
  })

  # detach when we're done
  defer(detach("tools:renv"), scope = teardown_env())

}
