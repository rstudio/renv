
renv_hash_description <- function(path) {

  dcf <- renv_description_read(path)

  # include default fields
  fields <- c(
    "Package", "Version", "Title", "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )

  # add remotes fields
  remotes <- renv_hash_description_remotes(dcf)

  # retrieve these fields
  subsetted <- dcf[intersect(c(fields, remotes), names(dcf))]

  # include R version in hash
  subsetted[["RVersion"]] <- getRversion()

  # sort names (use C locale to ensure consistent ordering)
  ordered <- local({
    renv_scope_locale("LC_COLLATE", "C")
    subsetted[sort(names(subsetted))]
  })

  # write to tempfile (use binary connection to ensure unix-style
  # newlines for cross-platform hash stability)
  tempfile <- tempfile("renv-description-hash-")
  on.exit(unlink(tempfile), add = TRUE)
  contents <- paste(names(ordered), ordered, sep = ": ", collapse = "\n")

  # create the file connection (use binary so that unix newlines are used
  # across platforms, for more stable hashing)
  con <- file(tempfile, open = "wb")
  on.exit(close(con), add = TRUE)

  # write to the file (be sure to flush since we don't close the connection
  # until exit)
  writeLines(enc2utf8(contents), con = con, useBytes = TRUE)
  flush(con)

  # ready for hasing
  tools::md5sum(tempfile)

}

renv_hash_description_remotes <- function(dcf) {

  type <- dcf[["RemoteType"]]
  if (is.null(type))
    return(character())

  if (type == "standard")
    return(character())

  grep("^Remote", names(dcf), value = TRUE)

}
