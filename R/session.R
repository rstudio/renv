
renv_session_quiet <- function() {

  args <- commandArgs(trailingOnly = FALSE)

  index <- match("--args", args)
  if (!is.na(index))
    args <- head(args, n = index - 1L)

  quiet <- c("-s", "-q", "--slave", "--no-echo", "--quiet", "--silent")
  any(quiet %in% args)

}
