
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


renv_git_clone <- function(record, path) {
  renv_git_preflight()

  template <- c(
    "cd \"${DIR}\"",
    "git init --quiet",
    "git remote add origin \"${ORIGIN}\"",
    "git fetch --quiet origin \"${REF}\"",
    "git reset --quiet --hard FETCH_HEAD"
  )

  data <- list(
    DIR    = renv_path_normalize(path),
    ORIGIN = record$RemoteUrl,
    REF    = record$RemoteSha %||% record$RemoteRef
  )

  commands <- renv_template_replace(template, data)
  command <- paste(commands, collapse = " && ")
  if (renv_platform_windows())
    command <- paste(comspec(), "/C", command)


  status <- local({
    renv_scope_auth(record)
    renv_scope_git_auth()
    system(command)
  })

  if (status != 0L) {
    fmt <- "cannot retrieve package '%s' from '%s' [status code %i]"
    stopf(fmt, record$Package, record$RemoteUrl, status)
  }

  TRUE
}
