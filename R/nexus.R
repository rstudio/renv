
renv_nexus_enabled <- function(repo) {

  # first, check a global option
  enabled <- getOption("renv.nexus.enabled", default = FALSE)
  if (enabled)
    return(TRUE)

  # otherwise, check cached repository information
  info <- renv_repos_info(repo)
  identical(info$nexus, TRUE)

}
