
renv_diagnose <- function(project) {

  if (!file.exists(file.path(project, "renv"))) {

    fmt <- lines(
      "Project '%s' has no associated virtual environment.",
      "Call `renv_activate(<environment>)` to activate a virtual environment."
    )

    msg <- sprintf(fmt, aliased_path(project))
    stop(msg, call. = FALSE)

  }

  if (!file.exists(file.path(project, "renv/manifest"))) {

    fmt <- lines(
      "Project '%s' has no manifest.",
      "Have you called `renv_snapshot()` yet?"
    )

    msg <- sprintf(fmt, aliased_path(project))
    stop(msg, call. = FALSE)

  }

}
