
renv_parallel_cores <- function() {

  if (renv_platform_windows())
    return(1L)

  value <- config$updates.parallel()
  case(
    identical(value, TRUE)  ~ getOption("mc.cores", default = 2L),
    identical(value, FALSE) ~ 1L,
    ~ as.integer(value)
  )

}

renv_parallel_exec <- function(data, callback) {
  cores <- renv_parallel_cores()
  if (config$updates.parallel())
    parallel::mclapply(data, callback, mc.cores = cores)
  else
    lapply(data, callback)
}
