
# aliases used primarily for nicer / normalized text output
`_renv_aliases` <- list(
  bioconductor = "Bioconductor",
  bitbucket    = "Bitbucket",
  cran         = "CRAN",
  git2r        = "Git",
  github       = "GitHub",
  gitlab       = "GitLab",
  local        = "Local",
  repository   = "Repository",
  standard     = "CRAN",
  url          = "URL",
  xgit         = "Git"
)

renv_alias <- function(text) {
  `_renv_aliases`[[text]] %||% text
}
