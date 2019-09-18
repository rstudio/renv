
renv_updates <- function(diff, old, new) {
  data <- list(diff = diff, old = old, new = new)
  class(data) <- "renv_updates"
  data
}

#' @export
print.renv_updates <- function(x, ...) {

  old <- x$old; new <- x$new; diff <- x$diff

  lhs <- renv_records(old)
  rhs <- renv_records(new)
  renv_pretty_print_records_pair(
    lhs[names(lhs) %in% names(diff)],
    rhs[names(rhs) %in% names(diff)]
  )

}
