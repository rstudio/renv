
renv_md5sum_old <- function(text) {

  tempfile <- tempfile("renv-md5sum-")
  con <- file(tempfile, open = "wb")
  writeLines(enc2utf8(text), con = con, useBytes = TRUE)
  flush(con)
  close(con)

  hash <- unname(md5sum(tempfile))
  unlink(tempfile)

  hash

}

renv_md5sum_new <- function(text) {

  # The old implementation worked by writing a string out to a file using
  # writeLines(), which would ensure a trailing newline was included in
  # the generated output. We append a newline here to preserve that behavior.
  newline <- as.raw(0x0a)
  bytes <- c(charToRaw(enc2utf8(text)), newline)

  # silence R CMD check warning -- this only gets invoked for R >= 4.5.0
  (md5sum)(bytes = bytes)

}

md5 <- if (getRversion() < "4.5.0") {
  renv_md5sum_old
} else {
  renv_md5sum_new
}
