
# check for problems in the project's private library (e.g. broken symlinks
# to the cache or similar)
renv_library_diagnose <- function(project, libpath) {

  children <- list.files(libpath, full.names = TRUE)
  if (empty(children))
    return(TRUE)

  # if all symlinks are broken, assume the cache is missing or has been moved
  missing <- !file.exists(children)
  if (all(missing)) {
    msg <- lines(
      "The project library's symlinks to the cache are all broken.",
      "Has the cache been removed, or is it otherwise inaccessible?",
      paste("Cache root:", shQuote(renv_paths_cache()))
    )
    warning(msg, call. = FALSE)
    return(FALSE)
  }

  # if only some symlinks are broken, report to user
  if (any(missing)) {

    renv_pretty_print(
      basename(children[missing]),
      "The following package(s) are missing entries in the cache:",
      "These packages will need to be reinstalled.",
      warningf
    )

    return(FALSE)

  }

  TRUE

}
