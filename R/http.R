
renv_http_useragent <- function() {

  # https://github.com/rstudio/renv/issues/1787
  agent <- getOption("renv.http.useragent", default = getOption("HTTPUserAgent"))
  if (is.character(agent) && length(agent) == 1L)
    return(agent)

  renv_http_useragent_default()

}

renv_http_useragent_default <- function() {
  version <- getRversion()
  platform <- with(R.version, paste(version, platform, arch, os))
  sprintf("R/%s R (%s)", version, platform)
}
