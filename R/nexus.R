
renv_nexus_enabled <- function(repo) {

  # first, check a global option
  enabled <- getOption("renv.nexus.enabled", default = FALSE)
  if (enabled)
    return(TRUE)

  # next, check repository headers
  headers <- renv_repos_info(repo)
  if (inherits(headers, "error"))
    return(FALSE)

  # check for server header
  server <- headers$server %||% "(unknown)"
  grepl("Nexus", server, ignore.case = TRUE)

}
