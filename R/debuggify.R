
debuggify <- function(expr) {
  withCallingHandlers(expr, interrupt = renv_debuggify_dump)
}

renv_debuggify_dump <- function(cnd) {

  # print a backtrace
  status <- sys.status()
  calls <- head(status$sys.calls, n = -2L)
  frames <- head(status$sys.frames, n = -2L)
  traceback <- renv_error_format(calls, frames)
  caution(traceback)

  # print information about each frame
  n <- length(calls)
  for (i in seq_along(calls)) {
    renv_debuggify_dump_impl(
      index  = n - i + 1,
      call   = calls[[i]],
      frame  = frames[[i]]
    )
  }

}

renv_debuggify_dump_impl <- function(index, call, frame) {
  writeLines(header(paste("Frame", index)))
  vars <- ls(envir = frame, all.names = TRUE)
  lapply(vars, renv_debuggify_dump_impl_one, call = call, frame = frame)
  writeLines("")
}

renv_debuggify_dump_impl_one <- function(var, call, frame) {

  if (var %in% c("expr"))
    return("<promise>")

  str(frame[[var]])

}
