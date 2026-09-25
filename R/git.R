
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
  renv_system_exec("git", c("rev-parse", "HEAD"), action = "reading git commit")
}

# clones made while resolving a git remote are kept in 'the$git_clones',
# keyed on the repository URL and commit, so that a following retrieve can
# install from the same clone rather than fetching the same commit again
renv_git_clone_key <- function(record) {
  paste(record$RemoteUrl, record$RemoteSha, sep = "@")
}

renv_git_clone_register <- function(record, path) {
  key <- renv_git_clone_key(record)
  the$git_clones <- the$git_clones %||% list()
  the$git_clones[[key]] <- path
  invisible(path)
}

renv_git_clone_take <- function(record) {

  key <- renv_git_clone_key(record)
  path <- the$git_clones[[key]]
  if (is.null(path))
    return(NULL)

  # hand the clone over exactly once; the installer owns it from here
  the$git_clones[[key]] <- NULL
  if (!dir.exists(path))
    return(NULL)

  path

}
