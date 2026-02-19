
timer <- function(units = "secs") {

  .time <- Sys.time()
  .units <- units

  list(

    now = function() {
      Sys.time()
    },

    elapsed = function() {
      difftime(Sys.time(), .time, units = .units)
    },

    tick = function() {
      result <- difftime(Sys.time(), .time, units = .units)
      .time <<- Sys.time()
      invisible(result)
    }

  )

}
