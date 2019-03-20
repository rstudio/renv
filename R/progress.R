
# given some function 'f' to be called repeatedly 'max' times,
# write a small progress counter (using 'emit') if we haven't
# finished execution after 'wait' seconds. note that we assume
# that the callback 'f' doesn't also attempt to write output
# to the same stream used by the progress emitter
renv_progress <- function(f, max, wait = 1.0, emit = renv_progress_emit) {

  # since we're returning a function need to force all arguments here
  force(f); force(max); force(wait); force(emit)

  # prepare some state tracking progress
  count <- 0
  progress <- ""
  start <- Sys.time()

  function(...) {

    # check for and print progress
    count <<- count + 1
    if (Sys.time() - start > wait) {
      backspaces <- paste(rep("\b", nchar(progress)), collapse = "")
      progress <<- sprintf("[%i/%i] ", count, max)
      emit(paste(backspaces, progress, sep = ""))
    }

    # invoke callback
    f(...)

  }

}

renv_progress_emit <- function(...) {
  cat(..., file = stdout(), sep = "")
}
