renv_manifest_read <- function(file) {

  contents <- read(file)
  parts <- strsplit(contents, "\n{2,}")[[1]]
  manifest <- lapply(parts, function(part) {
    pieces <- strsplit(part, "\n", fixed = TRUE)[[1]]
    dcf_fields_read(pieces)
  })
  manifest

}

renv_manifest_write <- function(manifest, file) {

  serialized <- paste(vapply(manifest, function(item) {
    paste(names(item), item, sep = ": ", collapse = "\n")
  }, character(1)), collapse = "\n\n")

  writeLines(serialized, con = file)

}

renv_manifest_create_entry <- function(path) {

  if (!file.exists(path)) {
    fmt <- "No DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, path)
    return(simpleError(msg))
  }

  # TODO: Check for tempfiles that sneak into library path, e.g. 'file<abcd>'
  # Report and skip?
  dcf <- tryCatch(read.dcf(path, all = TRUE), error = identity)
  if (inherits(dcf, "error"))
    return(dcf)

  if (is.null(dcf[["Repository"]]))
    dcf[["Repository"]] <- "<unknown>"

  fields <- c("Package", "Version", "Repository")
  missing <- setdiff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "Required fields %s missing from DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  fields <- c(fields, grep("^Remote", names(dcf)))
  dcf[fields]

}
