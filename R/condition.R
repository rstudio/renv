
renv_condition_signal <- function(class = NULL) {
  condition <- list(message = character(), call = NULL)
  class(condition) <- c(class, "condition")
  signalCondition(condition)
}
