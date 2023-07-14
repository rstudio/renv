
renv_release_preflight <- function() {

  ok <- all(
    renv_release_preflight_urlcheck()
  )

  if (!ok)
    stop("one or more pre-flight release checks failed")

  ok

}

renv_release_preflight_urlcheck <- function() {

  # check for bad URLs
  urlchecker <- renv_namespace_load("urlchecker")
  result <- urlchecker$url_check()

  # report to user
  print(result)

  # return success
  nrow(result) == 0L

}
