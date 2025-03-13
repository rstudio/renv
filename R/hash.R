
renv_hash_text <- function(text) {
  renv_bootstrap_hash_text(text)
}

renv_hash_fields <- function(dcf) {
  c(
    renv_hash_fields_default(),
    renv_hash_fields_remotes(dcf)
  )
}

renv_hash_fields_default <- function() {
  c(
    "Package", "Version", "Title",
    "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )
}

renv_hash_fields_remotes <- function(dcf) {

  # if this seems to be a cran-like record, only keep remotes
  # when RemoteSha appears to be a hash (e.g. for r-universe)
  # note that RemoteSha may be a package version when installed
  # by e.g. pak
  if (renv_record_cranlike(dcf)) {
    sha <- dcf[["RemoteSha"]]
    if (is.null(sha) || nchar(sha) < 40L)
      return(character())
  }

  # grab the relevant remotes
  remotes <- grep("^Remote", names(dcf), perl = TRUE, value = TRUE)

  # don't include 'RemoteRef' if it's a non-informative remote
  if (identical(dcf[["RemoteRef"]], "HEAD"))
    remotes <- setdiff(remotes, "RemoteRef")

  remotes

}

renv_hash_description <- function(path) {
  filebacked(
    context  = "renv_hash_description",
    path     = path,
    callback = renv_hash_description_impl
  )
}

renv_hash_description_impl <- function(path) {
  record <- renv_description_read(path)
  renv_hash_record(record)
}

renv_hash_record <- function(record) {

  # find relevant fields for hashing
  fields <- renv_hash_fields(record)

  # collapse vector / list dependency fields
  depfields <- c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")
  for (depfield in depfields) {
    if (!is.null(record[[depfield]])) {
      value <- unlist(record[[depfield]])
      record[[depfield]] <- paste(value, collapse = ", ")
    }
  }

  # retrieve these fields
  subsetted <- record[renv_vector_intersect(fields, names(record))]

  # sort names (use C locale to ensure consistent ordering)
  ordered <- subsetted[csort(names(subsetted))]

  # paste together into single string
  contents <- paste(names(ordered), ordered, sep = ": ", collapse = "\n")

  # remove whitespace -- it's possible that tools (e.g. Packrat) that
  # mutate a package's DESCRIPTION file may also inadvertently change
  # the structure of whitespace within some fields; that whitespace is
  # normally not semantically meaningful so we remove that so such
  # DESCRIPTIONS can obtain the same hash value. (this ultimately
  # arises as 'write.dcf()' allows both 'indent' and 'width' to be
  # configured based on the 'width' option)
  contents <- gsub("[[:space:]]", "", contents)

  # compute the hash
  invisible(md5(contents))

}
