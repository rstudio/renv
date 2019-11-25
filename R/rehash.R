
#' Re-Hash Packages in the renv Cache
#'
#' Re-hash packages in the `renv` cache, ensuring that any previously-cached
#' packages are copied (or moved) to a new cache location appropriate for this
#' version of `renv`.
#'
#' This can be useful if the cache scheme has changed in a new version of
#' `renv`, but you'd like to preserve your previously-cached packages.
#'
#' @inheritParams renv-params
#'
rehash <- function(cache   = paths$cache(),
                   clean   = FALSE,
                   confirm = TRUE)
{
  renv_scope_error_handler()
  invisible(renv_rehash_impl(cache, clean, confirm))
}

renv_rehash_impl <- function(cache   = paths$cache(),
                             clean   = FALSE,
                             confirm = TRUE)
{
  # re-compute package hashes
  old <- renv_cache_list(cache = cache)

  callback <- function(path) {
    renv_snapshot_description(path) %>% renv_cache_package_path()
  }

  vprintf("* Re-computing package hashes ... ")
  new <- map_chr(old, renv_progress(callback, length(old)))
  vwritef("Done!")

  # remove any old packages which appear to have already been re-cached
  if (clean && !renv_rehash_clean(old, new, confirm))
    return(FALSE)

  changed <- which(old != new & file.exists(old) & !file.exists(new))
  if (empty(changed)) {
    vwritef("* Your cache is already up-to-date -- nothing to do.")
    return(TRUE)
  }

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

  sources <- old[changed]
  targets <- new[changed]
  names(targets) <- sources

  callback <- if (clean)
    renv_file_move
  else
    renv_file_copy

  vprintf("* Re-caching packages ... ")
  enumerate(targets, renv_progress(callback, length(targets)))
  vwritef("Done!")

  fmt <- "* %i package(s) have been re-cached."
  vwritef(fmt, length(targets))

  renv_cache_clean_empty()

  TRUE
}

renv_rehash_clean <- function(old, new, confirm) {

  # check for any changes
  changed <- old != new
  if (!any(changed))
    return(TRUE)

  # check which of these changes have already been re-cached
  changed <- changed & file.exists(new)
  if (!any(changed))
    return(TRUE)

  old <- old[changed]
  new <- new[changed]

  packages <- basename(old)
  oldhash <- renv_path_component(old, 2L)
  newhash <- renv_path_component(new, 2L)

  fmt <- "%s [%s -> %s]"
  renv_pretty_print(
    sprintf(fmt, format(packages), format(oldhash), format(newhash)),
    "The following package(s) have already been re-hashed:",
    "The previously-cached version of the package will be removed.",
    wrap = FALSE
  )

  if (confirm && !proceed()) {
    message("* Operation aborted.")
    return(FALSE)
  }

  vprintf("* Removing old packages from the cache ... ")
  unlink(dirname(old), recursive = TRUE, force = TRUE)
  vwritef("Done!")
  TRUE

}
