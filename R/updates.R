
renv_updates <- function(diff, old, new) {
  data <- list(diff = diff, old = old, new = new)
  class(data) <- "renv_updates"
  data
}

#' @export
print.renv_updates <- function(x, ...) {

  old <- x$old; new <- x$new; diff <- x$diff

  lhs <- renv_records(old)[names(diff)]
  rhs <- renv_records(new)[names(diff)]

  renv_pretty_print_records_pair(lhs, rhs)

}
