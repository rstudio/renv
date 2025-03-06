
#' Re-hash packages in the renv cache
#'
#' Re-hash packages in the renv cache, ensuring that any previously-cached
#' packages are copied to a new cache location appropriate for this version of
#' renv. This can be useful if the cache scheme has changed in a new version
#' of renv, but you'd like to preserve your previously-cached packages.
#'
#' Any packages which are re-hashed will retain links to the location of the
#' newly-hashed package, ensuring that prior installations of renv can still
#' function as expected.
#'
#' @inheritParams renv-params
#'
#' @export
rehash <- function(prompt = interactive(), ...) {
  renv_scope_error_handler()
  renv_dots_check(...)
  renv_scope_verbose_if(prompt)
  invisible(renv_rehash_impl(prompt))
}

renv_rehash_impl <- function(prompt) {

  # check for cache migration
  oldcache <- renv_paths_cache(version = renv_cache_version_previous())[[1L]]
  newcache <- renv_paths_cache(version = renv_cache_version())[[1L]]
  if (file.exists(oldcache) && !file.exists(newcache))
    renv_rehash_cache(oldcache, prompt, renv_file_copy, "copied")

  # re-cache packages as necessary
  renv_rehash_cache(newcache, prompt, renv_file_move, "moved")

}

renv_rehash_cache <- function(cache, prompt, action, label) {

  # re-compute package hashes
  old <- renv_cache_list(cache = cache)

  printf("- Re-computing package hashes ... ")
  new <- map_chr(old, renv_progress_callback(renv_cache_path, length(old)))
  writef("Done!")

  changed <- which(old != new & file.exists(old) & !file.exists(new))
  if (empty(changed)) {
    writef("- Your cache is already up-to-date -- nothing to do.")
    return(TRUE)
  }

  if (prompt) {

    fmt <- "%s [%s -> %s]"
    packages <- basename(old)[changed]
    oldhash <- renv_path_component(old[changed], 2L)
    newhash <- renv_path_component(new[changed], 2L)
    bulletin(
      "The following packages will be re-cached:",
      sprintf(fmt, format(packages), format(oldhash), format(newhash)),
      sprintf("Packages will be %s to their new locations in the cache.", label)
    )

    cancel_if(prompt && !proceed())

  }

  sources <- old[changed]
  targets <- new[changed]
  names(sources) <- targets
  names(targets) <- sources

  printf("- Re-caching packages ... ")
  enumerate(targets, renv_progress_callback(action, length(targets)))
  writef("Done!")

  n <- length(targets)
  fmt <- "Successfully re-cached %s."
  writef(fmt, nplural("package", n))
  renv_cache_clean_empty()

  TRUE
}
