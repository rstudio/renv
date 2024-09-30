
recurse <- function(object, callback, ...) {
  
  queue <- vector("list", 8192L)
  queue[[1L]] <- object
  index <- 0L
  slot <- 1L
  
  while (index != slot) {
    
    index <- index + 1L
    result <- callback(queue[[index]], ...)
    
    if (is.recursive(object <- if (is.call(result)) result else queue[[index]])) {
      for (i in seq_along(object)) {
        slot <- slot + 1L
        queue[[slot]] <- object[[i]]
      }
    }
    
  }
  
}

