
renv_parallel_cores <- function() {

  if (renv_platform_windows())
    return(1L)

  config <- renv_config("updates.parallel", default = TRUE)
  case(
    identical(config, TRUE)  ~ getOption("mc.cores", default = 2L),
    identical(config, FALSE) ~ 1L,
    ~ as.integer(config)
  )

}

renv_parallel_exec <- function(data, callback) {
  cores <- renv_parallel_cores()
  if (renv_config("updates.parallel", default = TRUE))
    parallel::mclapply(data, callback, mc.cores = cores)
  else
    lapply(data, callback)
}
