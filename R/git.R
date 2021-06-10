
renv_git_preflight <- function() {
  if (!nzchar(Sys.which("git")))
    stopf("'git' is not available on the PATH")
}

renv_git_root <- function(project) {

  project <- normalizePath(project, winslash = "/", mustWork = FALSE)
  renv_file_find(project, function(parent) {
    gitroot <- file.path(parent, ".git")
    if (file.exists(gitroot))
      return(gitroot)
  })

}
