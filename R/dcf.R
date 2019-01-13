
renv_dcf_read <- function(file, ...) {
  # older versions of R could mutate LC_CTYPE (without resetting it)
  # when reading DCF files, so be sure to manage that here
  ctype <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
  as.data.frame(read.dcf(file, ...), stringsAsFactors = FALSE)
}
