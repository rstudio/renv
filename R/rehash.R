
#' Re-Hash Packages in the renv Cache
#'
#' Re-hash packages in the `renv` cache, ensuring that any previously-cached
#' packages are copied to a new cache location appropriate for this version of
#' `renv`. This can be useful if the cache scheme has changed in a new version
#' of `renv`, but you'd like to preserve your previously-cached packages.
#'
#' Any packages which are re-hashed will retain links to the location of the
#' newly-hashed package, ensuring that prior installations of `renv` can still
#' function as expected.
#'
#' @inheritParams renv-params
#'
#' @export
rehash <- function(prompt = interactive(), ...) {
  renv_scope_error_handler()
  renv_dots_check(...)
  invisible(renv_rehash_impl(prompt))
}

renv_rehash_impl <- function(prompt) {

  # check for cache migration
  oldcache <- renv_paths_cache(version = renv_cache_version_previous())
  newcache <- renv_paths_cache(version = renv_cache_version())
  if (file.exists(oldcache) && !file.exists(newcache))
    renv_rehash_cache(oldcache, prompt, renv_file_copy, "copied")

  # re-cache packages as necessary
  renv_rehash_cache(newcache, prompt, renv_file_move, "moved")

}

renv_rehash_cache <- function(cache, prompt, action, label) {

  # re-compute package hashes
  old <- renv_cache_list(cache = cache)

  vprintf("* Re-computing package hashes ... ")
  new <- map_chr(old, renv_progress(renv_cache_path, length(old)))
  vwritef("Done!")

  changed <- which(old != new & file.exists(old) & !file.exists(new))
  if (empty(changed)) {
    vwritef("* Your cache is already up-to-date -- nothing to do.")
    return(TRUE)
  }

  if (prompt) {

    fmt <- "%s [%s -> %s]"
    packages <- basename(old)[changed]
    oldhash <- renv_path_component(old[changed], 2L)
    newhash <- renv_path_component(new[changed], 2L)
    renv_pretty_print(
      sprintf(fmt, format(packages), format(oldhash), format(newhash)),
      "The following packages will be re-cached:",
      sprintf("Packages will be %s to their new locations in the cache.", label),
      wrap = FALSE
    )

    if (prompt && !proceed()) {
      message("* Operation aborted.")
      return(FALSE)
    }

  }

  sources <- old[changed]
  targets <- new[changed]
  names(sources) <- targets
  names(targets) <- sources

  vprintf("* Re-caching packages ... ")
  enumerate(targets, renv_progress(action, length(targets)))
  vwritef("Done!")

  n <- length(targets)
  fmt <- "Successfully re-cached %i %s."
  vwritef(fmt, n, plural("package", n))

  renv_cache_clean_empty()

  TRUE
}
