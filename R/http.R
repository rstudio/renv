
renv_http_useragent <- function() {
  agent <- getOption("renv.http.useragent", default = getOption("HTTPUserAgent"))
  agent %||% renv_http_useragent_default()
}

renv_http_useragent_default <- function() {
  fmt <- "R (%s %s %s)"
  sprintf(fmt,  getRversion(), R.version$platform, R.version$os)
}
