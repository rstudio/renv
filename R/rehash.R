
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
rehash <- function(confirm = interactive()) {
  renv_scope_error_handler()
  invisible(renv_rehash_impl(confirm))
}

renv_rehash_impl <- function(confirm) {

  # check for cache migration
  oldcache <- renv_paths_cache(version = renv_cache_version_previous())
  newcache <- renv_paths_cache(version = renv_cache_version())
  if (file.exists(oldcache) && !file.exists(newcache))
    renv_rehash_cache(oldcache, confirm, renv_file_copy)

  # re-cache packages as necessary
  renv_rehash_cache(newcache, confirm, renv_file_move)

}

renv_rehash_cache <- function(cache, confirm, action) {

  # re-compute package hashes
  old <- renv_cache_list(cache = cache)

  callback <- function(path) {
    renv_snapshot_description(path) %>% renv_cache_package_path()
  }

  vprintf("* Re-computing package hashes ... ")
  new <- map_chr(old, renv_progress(callback, length(old)))
  vwritef("Done!")

  changed <- which(old != new & file.exists(old) & !file.exists(new))
  if (empty(changed)) {
    vwritef("* Your cache is already up-to-date -- nothing to do.")
    return(TRUE)
  }

  if (confirm) {

    fmt <- "%s [%s -> %s]"
    packages <- basename(old)[changed]
    oldhash <- renv_path_component(old[changed], 2L)
    newhash <- renv_path_component(new[changed], 2L)
    renv_pretty_print(
      sprintf(fmt, format(packages), format(oldhash), format(newhash)),
      "The following packages will be re-cached:",
      "Packages will be copied to their new locations in the cache.",
      wrap = FALSE
    )

    if (confirm && !proceed()) {
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

  fmt <- "* %i %s have been re-cached."
  vwritef(fmt, length(targets), plural("package", length(targets)))

  renv_cache_clean_empty()

  TRUE
}
