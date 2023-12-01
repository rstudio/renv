
# basically just a mutable R list
mapping <- function() {

  .data <- vector("list", 0L)

  list(

    get = function(key) {
      .data[[key]]
    },

    contains = function(key) {
      key %in% names(.data)
    },

    insert = function(key, value) {
      .data[[key]] <<- value
    },

    data = function() {
      .data
    }

  )

}
