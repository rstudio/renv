
renv_difftime_format <- function(time, digits = 2L) {

  elapsed <- format(unclass(signif(time, digits = digits)))

  units <- switch(
    attr(time, "units"),
    secs  = "seconds",
    mins  = "minutes",
    hours = "hours",
    days  = "days",
    weeks = "weeks"
  )

  paste(elapsed, units)

}
