
renv_hash_text <- function(text) {
  renv_bootstrap_hash_text(text)
}

renv_hash_description <- function(path) {
  filebacked(
    context  = "renv_hash_description",
    path     = path,
    callback = renv_hash_description_impl
  )
}

renv_hash_description_impl <- function(path) {

  dcf <- case(
    is.character(path) ~ renv_description_read(path),
    is.list(path)      ~ path,
    ~ stop("unexpected path '%s'", path)
  )

  # include default fields
  fields <- c(
    "Package", "Version", "Title", "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )

  # add remotes fields
  remotes <- renv_hash_description_remotes(dcf)

  # retrieve these fields
  subsetted <- dcf[renv_vector_intersect(c(fields, remotes), names(dcf))]

  # sort names (use C locale to ensure consistent ordering)
  ordered <- subsetted[csort(names(subsetted))]

  # write to tempfile (use binary connection to ensure unix-style
  # newlines for cross-platform hash stability)
  tempfile <- tempfile("renv-description-hash-")
  contents <- paste(names(ordered), ordered, sep = ": ", collapse = "\n")

  # remove whitespace -- it's possible that tools (e.g. Packrat) that
  # mutate a package's DESCRIPTION file may also inadvertently change
  # the structure of whitespace within some fields; that whitespace is
  # normally not semantically meaningful so we remove that so such
  # DESCRIPTIONS can obtain the same hash value. (this ultimately
  # arises as 'write.dcf()' allows both 'indent' and 'width' to be
  # configured based on the 'width' option)
  contents <- gsub("[[:space:]]", "", contents)

  # create the file connection (use binary so that unix newlines are used
  # across platforms, for more stable hashing)
  con <- file(tempfile, open = "wb")

  # write to the file
  writeLines(enc2utf8(contents), con = con, useBytes = TRUE)

  # flush to ensure we've written to file
  flush(con)

  # close the connection and remove the file
  close(con)

  # ready for hasing
  hash <- unname(md5sum(tempfile))

  # remove the old file
  unlink(tempfile)

  # return hash
  invisible(hash)

}

renv_hash_description_remotes <- function(dcf) {

  # ignore other remote fields for cranlike remotes
  if (renv_record_cranlike(dcf))
    return(character())

  # otherwise, include any other discovered remote fields
  grep("^Remote", names(dcf), value = TRUE)

}
