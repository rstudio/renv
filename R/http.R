
renv_http_useragent <- function() {
  getOption("renv.http.useragent", default = getOption("HTTPUserAgent"))
}
