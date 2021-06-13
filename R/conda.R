
# given the path to a Python installation managed by conda, attempt to
# find the conda installation + executable used to create it
renv_conda_find <- function(python) {

  tryCatch(
    renv_conda_find_impl(python),
    error = function(e) {
      warning(e)
      ""
    }
  )

}

renv_conda_find_impl <- function(python) {

  # read the conda environment's history to try to find conda
  base <- dirname(python)
  if (!renv_platform_windows())
    base <- dirname(base)

  history <- file.path(base, "conda-meta/history")
  if (!file.exists(history))
    return("")

  contents <- readLines(history, n = 2L, warn = FALSE)
  if (length(contents) < 2)
    return("")

  line <- substring(contents[2L], 8L)
  index <- regexpr(" ", line, fixed = TRUE)
  if (index == -1L)
    return("")

  conda <- substring(line, 1L, index - 1L)
  if (renv_platform_windows())
    conda <- file.path(dirname(conda), "conda.exe")

  # prefer condabin if it exists
  condabin <- file.path(dirname(conda), "../condabin", basename(conda))
  if (file.exists(condabin))
    conda <- condabin

  # bail if conda wasn't found
  if (!file.exists(conda))
    return("")

  renv_path_canonicalize(conda)

}
