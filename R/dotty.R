
`_renv_dotdot` <- as.symbol("..")

. <- structure(list(), class = "__renv_dotty__")

dotty <- function(x, ..., value) {

  # get call parts
  envir <- parent.frame()
  call  <- sys.call()
  parts <- call[3L:(length(call) - 1L)]

  # run dotty
  renv_dotty_impl(
    parts = parts,
    value = value,
    envir = envir
  )

  # TODO: Consider cleaning up "." created in parent environment here.
  # # remove '.' variable created in parent env
  # rm(".", envir = envir)
  #
  # # make sure we clean up the binding on exit
  # makeActiveBinding(
  #   sym = ".",
  #   fun = function(value) rm(".", envir = envir),
  #   env = envir
  # )

  # return dotty
  invisible(.)

}

renv_dotty_impl <- function(parts, value, envir) {

  # search for a '..' placeholder; if we
  index <- renv_dotty_find(parts)
  if (is.null(index))
    return(renv_dotty_eval(parts, value, envir))

  # split into left parts, right parts
  nleft <- index - 1L
  nright <- length(parts) - index

  # evaluate left variables
  renv_dotty_eval(
    .subset(parts, seq_len(nleft)),
    .subset(value, seq_len(nleft)),
    envir
  )

  # evaluate right variables
  renv_dotty_eval(
    .subset(parts, length(parts) - seq_len(nright) + 1L),
    .subset(value, length(value) - seq_len(nright) + 1L),
    envir
  )

}

renv_dotty_eval <- function(parts, value, envir) {

  for (i in seq_along(parts)) {

    part <- parts[[i]]
    key <- names(parts)[[i]]

    if (is.character(key) && nzchar(key)) {
      result <- eval(part, envir = as.list(value), enclos = envir)
      assign(key, result, envir = envir)
    } else if (is.call(part)) {
      renv_dotty_impl(part[3L:length(part)], value[[i]], envir)
    } else {
      assign(as.character(part), value[[i]], envir = envir)
    }

  }

}

renv_dotty_find <- function(parts) {
  for (i in seq_along(parts))
    if (is.symbol(parts[[i]]) && parts[[i]] == `_renv_dotdot`)
      return(i)
}
