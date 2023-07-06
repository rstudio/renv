
renv_difftime_format <- function(time, digits = 2L) {

  if (is_testing())
    return("XXXX seconds")

  units <- attr(time, "units") %||% ""
  if (units == "secs" && time < 0.1) {
    time  <- time * 1000
    units <- "milliseconds"
  }

  units <- switch(
    units,
    secs  = "seconds",
    mins  = "minutes",
    hours = "hours",
    days  = "days",
    weeks = "weeks",
    units
  )

  elapsed <- format(unclass(signif(time, digits = digits)))
  if (elapsed %in% c("1", "1.0"))
    units <- substring(units, 1L, nchar(units) - 1L)

  paste(elapsed, units)

}

renv_difftime_format_short <- function(time, digits = 2L) {

  if (is_testing())
    return("XXs")

  elapsed <- signif(time, digits = digits)
  if (nchar(elapsed) == 1L)
    elapsed <- paste(elapsed, ".0", sep = "")

  units <- switch(
    attr(time, "units"),
    secs  = "s",
    mins  = "m",
    hours = "h",
    days  = "d",
    weeks = "w"
  )

  paste(elapsed, units, sep = "")

}
