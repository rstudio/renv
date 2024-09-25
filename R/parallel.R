
renv_parallel_cores <- function() {

  if (renv_platform_windows())
    return(1L)

  value <- config$updates.parallel()
  if (identical(value, TRUE)) {
    parallel <- requireNamespace("parallel", quietly = TRUE)
    getOption("mc.cores", default = if (parallel) 2L else 1L)
  } else if (identical(value, FALSE)) {
    1L
  } else {
    as.integer(value)
  }

}

renv_parallel_exec <- function(data, callback) {
  cores <- renv_parallel_cores()
  if (cores > 1L)
    parallel::mclapply(data, callback, mc.cores = cores)
  else
    lapply(data, callback)
}
