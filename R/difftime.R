
renv_difftime_format <- function(time, digits = 2L) {

  units <- attr(time, "units") %||% ""
  if (units == "secs" && time < 0.1) {
    time  <- time * 1000
    units <- "millisecond"
  }

  units <- switch(
    units,
    secs  = "second",
    mins  = "minute",
    hours = "hour",
    days  = "day",
    weeks = "week",
    units
  )

  elapsed <- format(unclass(signif(time, digits = digits)))
  paste(elapsed, plural(units, as.integer(elapsed)))

}

renv_difftime_format_short <- function(time, digits = 2L) {

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
