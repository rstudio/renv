
download <- function(url, destfile) {
  download.file(url, destfile, quiet = TRUE, mode = "wb")
}
