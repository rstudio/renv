
stack <- function(mode = "list") {

  .data <- list()
  storage.mode(.data) <- mode

  list(

    push = function(...) {
      dots <- list(...)
      for (data in dots) {
        if (is.null(data))
          .data[length(.data) + 1] <<- list(NULL)
        else
          .data[[length(.data) + 1]] <<- data
      }
    },

    pop = function() {
      item <- .data[[length(.data)]]
      length(.data) <<- length(.data) - 1
      item
    },

    peek = function() {
      .data[[length(.data)]]
    },

    contains = function(data) {
      data %in% .data
    },

    empty = function() {
      length(.data) == 0
    },

    get = function(index) {
      if (index <= length(.data)) .data[[index]]
    },

    set = function(index, value) {
      .data[[index]] <<- value
    },

    clear = function() {
      .data <<- list()
    },

    data = function() {
      .data
    }

  )

}
