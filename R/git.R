
renv_git_preflight <- function() {
  if (!nzchar(Sys.which("git")))
    stopf("'git' is not available on the PATH")
}
