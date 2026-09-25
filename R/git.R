
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
