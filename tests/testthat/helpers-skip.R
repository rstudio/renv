skip_if_no_github_auth <- function() {
  skip_if(is.na(Sys.getenv("GITHUB_PAT", unset = NA)), "GITHUB_PAT not set")
}

skip_if_not_Rversion <- function(version) {
  version <- getRversion()[1, 1:2]
  skip_if_not(version == version, paste0("only run on R ", version))
}
