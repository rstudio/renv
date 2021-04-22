
renv_session_quiet <- function() {

  args <- commandArgs(trailingOnly = FALSE)

  index <- match("--args", args)
  if (!is.na(index))
    args <- head(args, n = index - 1L)

  quiet <- c("-s", "--slave", "--no-echo")
  any(quiet %in% args)

}
