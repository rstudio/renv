
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

renv_git_root <- function(project) {

  project <- renv_path_normalize(project)
  renv_file_find(project, function(parent) {
    gitroot <- file.path(parent, ".git")
    if (file.exists(gitroot))
      return(gitroot)
  })

}
