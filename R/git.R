
git <- function() {

  gitpath <- Sys.which("git")
  if (!nzchar(gitpath))
    stop("failed to find git executable on the PATH")

  gitpath

}

renv_git_preflight <- function() {
  if (!nzchar(Sys.which("git")))
    stopf("'git' is not available on the PATH")
}

renv_git_sha <- function(path) {

  renv_scope_wd(path)

  # git may print warnings (e.g. about its own configuration) on stderr,
  # so read only stdout, and check that we got a commit hash back
  args <- c("rev-parse", "HEAD")
  output <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE))

  sha <- trimws(output)
  sha <- sha[grepl("^[[:xdigit:]]{40,64}$", sha)]
  if (length(sha) != 1L)
    stopf("error reading git commit in '%s'", path)

  sha

}

renv_git_commit_exists <- function(path, sha) {

  renv_scope_wd(path)

  args <- c("cat-file", "-e", sha)
  status <- suppressWarnings(system2("git", args, stdout = FALSE, stderr = FALSE))
  identical(status, 0L)

}

# the commit-ish that should be fetched for a git record
renv_git_rev <- function(record) {

  sha <- record$RemoteSha %||% ""
  ref <- record$RemoteRef %||% ""

  case(
    nzchar(sha) ~ sha,
    nzchar(ref) ~ ref,
    "HEAD"
  )

}

# clones of git repositories are cached for the duration of an operation (e.g.
# install() or restore()), so that each commit is cloned at most once; for
# example, the clone made while resolving a remote (to read its DESCRIPTION)
# is re-used to install that remote. clones are removed when the operation
# that created the cache completes, so operations which hand clones back to
# their caller (e.g. retrieve()) shouldn't use one.
renv_scope_git_clones <- function(scope = parent.frame()) {

  # share the cache of an enclosing operation, if any
  clones <- the$git_clones
  if (!is.null(clones))
    return(invisible(clones))

  clones <- env(keys = list(), paths = character())
  the$git_clones <- clones

  defer({
    unlink(clones$paths, recursive = TRUE, force = TRUE)
    the$git_clones <- NULL
  }, scope = scope)

  invisible(clones)

}

renv_git_clone_key <- function(record) {
  paste(record$RemoteUrl, renv_git_rev(record), sep = "@")
}

# pin a git record to the commit checked out by its clone, unless it's pinned
# already; e.g. records written by older versions of renv have no sha
# https://github.com/rstudio/renv/issues/2378
renv_git_record_pin <- function(record, path) {

  if (nzchar(record$RemoteSha %||% ""))
    return(record)

  record$RemoteSha <- renv_git_sha(path)

  # make the clone available by commit as well, so that later steps of this
  # operation (e.g. installing the record) can use it rather than cloning again
  renv_git_clone_register(record, path)

  record

}

renv_git_clone_register <- function(record, path) {

  clones <- the$git_clones
  if (!is.null(clones))
    clones$keys[[renv_git_clone_key(record)]] <- path

  invisible(path)

}

# adopt a clone made outside of the active cache (e.g. in a forked process), so
# that it can be re-used, and is removed along with the others
renv_git_clone_adopt <- function(record, path) {

  clones <- the$git_clones
  if (is.null(clones))
    return(invisible(path))

  if (!path %in% clones$paths)
    clones$paths <- c(clones$paths, path)

  renv_git_clone_register(record, path)

}

renv_git_clone <- function(record) {

  clones <- the$git_clones

  # re-use a clone made earlier in this operation, if any
  path <- clones$keys[[renv_git_clone_key(record)]]
  if (!is.null(path) && dir.exists(path))
    return(path)

  # track the clone before it's made, so that it's removed even if cloning
  # fails; without an active cache, the caller owns the clone
  path <- tempfile("renv-git-")
  if (!is.null(clones))
    clones$paths <- c(clones$paths, path)

  renv_retrieve_git_impl(record, path)
  renv_git_clone_register(record, path)

}
