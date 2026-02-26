
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
