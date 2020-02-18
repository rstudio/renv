
renv_condition_signal <- function(class = NULL, data = NULL) {
  condition <- list(message = character(), call = NULL, data = data)
  class(condition) <- c(class, "renv.condition", "condition")
  signalCondition(condition)
}

renv_condition_data <- function(condition) {
  condition$data
}
