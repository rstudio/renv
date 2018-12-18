renv_hash_description <- function(path) {

  # accept package directory as well as DESCRIPTION file itself
  if (file.exists(file.path(path, "DESCRIPTION")))
    path <- file.path(path, "DESCRIPTION")

  # read DESCRIPTION and keep only pertitent fields for hashing
  dcf <- renv_description_read(path)

  # include default fields
  fields <- c(
    "Package", "Version", "Title", "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )

  # add remotes fields
  remotes <- grep("^Remote", names(dcf), value = TRUE)

  # retrieve these fields
  subsetted <- dcf[intersect(c(fields, remotes), names(dcf))]

  # sort names (use C locale to ensure consistent ordering)
  collate <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = "C")
  ordered <- subsetted[sort(names(subsetted))]
  Sys.setlocale(category = "LC_COLLATE", locale = collate)

  # write to tempfile
  file <- tempfile("renv-description-hash-")
  on.exit(unlink(file), add = TRUE)
  contents <- paste(names(ordered), ordered, sep = ": ", collapse = "\n")
  writeLines(enc2utf8(contents), con = file, useBytes = TRUE)

  # ready for hasing
  tools::md5sum(file)

}
