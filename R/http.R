
renv_http_useragent <- function() {
  agent <- getOption("renv.http.useragent", default = getOption("HTTPUserAgent"))
  agent %||% renv_http_useragent_default()
}

renv_http_useragent_default <- function() {
  version <- getRversion()
  platform <- with(R.version, paste(version, platform, arch, os))
  sprintf("R/%s R (%s)", version, platform)
}
