
renv_progress_create <- function(max, wait = 1.0) {

  # local variables for closure
  count <- 0L
  max <- max
  message <- ""
  start <- Sys.time()

  function() {

    # check for and print progress
    count <<- count + 1L

    # if not enough time has elapsed yet, nothing to do
    if (Sys.time() - start < wait)
      return()

    # create message
    backspaces <- paste(rep("\b", nchar(message)), collapse = "")
    message <<- sprintf("[%i/%i] ", count, max)
    all <- paste(backspaces, message, sep = "")
    cat(all, file = stdout(), sep = "")

  }

}

renv_progress_callback <- function(callback, max, wait = 1.0) {
  tick <- renv_progress_create(max, wait)
  function(...) { tick(); callback(...) }
}
