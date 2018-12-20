
renv_dcf_read <- function(file, ...) {
  ctype <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
  as.data.frame(read.dcf(file, ...), stringsAsFactors = FALSE)
}
