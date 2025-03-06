
renv_parallel_cores <- function() {

  if (renv_platform_windows())
    return(1L)

  parallel <- config$updates.parallel()
  value <- if (identical(parallel, TRUE)) {
    requireNamespace("parallel", quietly = TRUE)
    getOption("mc.cores", default = 2L)
  } else if (identical(parallel, FALSE)) {
    1L
  } else {
    parallel
  }

  ok <- is.numeric(value) && length(value) == 1L && !is.na(value)
  if (ok) value else 1L

}

renv_parallel_exec <- function(data, callback) {
  cores <- renv_parallel_cores()
  if (cores > 1L)
    parallel::mclapply(data, callback, mc.cores = cores)
  else
    lapply(data, callback)
}
