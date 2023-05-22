skip_if_no_GitHub_auth <- function() {
  skip_if(is.na(Sys.getenv("GITHUB_PAT", unset = NA)), "GITHUB_PAT not set")
}
