
stack <- function() {

  .data <- list()

  list(

    push = function(data) {
      .data[[length(.data) + 1]] <<- data
    },

    pop = function() {
      item <- .data[[length(.data)]]
      .data[[length(.data)]] <<- NULL
    },

    peek = function() {
      .data[[length(.data)]]
    },

    contains = function(data) {
      data %in% .data
    },

    data = function() {
      .data
    }

  )

}
