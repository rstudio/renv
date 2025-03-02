
# aliases used primarily for nicer / normalized text output
the$aliases <- c(
  bioc         = "Bioconductor",
  bioconductor = "Bioconductor",
  bitbucket    = "Bitbucket",
  cellar       = "Cellar",
  cran         = "CRAN",
  git2r        = "Git",
  github       = "GitHub",
  gitlab       = "GitLab",
  local        = "Local",
  repository   = "Repository",
  standard     = "Repository",
  url          = "URL",
  xgit         = "Git"
)

alias <- function(text, aliases = the$aliases) {
  matches <- text %in% names(aliases)
  text[matches] <- aliases[text[matches]]
  text
}
