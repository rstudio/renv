
renv_dcf_read <- function(file, ...) {

  # if the file is empty, then nothing to do (guard against NA
  # file sizes if for some reason the filesystem / OS doesn't report it)
  if (is.character(file)) {
    info <- file.info(file, extra_cols = FALSE)
    if (identical(as.numeric(info$size), 0))
      return(data.frame())
  }

  # older versions of R could mutate LC_CTYPE (without resetting it)
  # when reading DCF files, so be sure to manage that here
  ctype <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

  # read the file
  dcf <- as.data.frame(read.dcf(file, ...), stringsAsFactors = FALSE)
  lapply(dcf, trimws)

}

renv_dcf_write <- function(x, file = "") {
  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)
}
