
# aliases used primarily for nicer / normalized text output
the$aliases <- list(
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

alias <- function(text) {
  the$aliases[[text]] %||% text
}
