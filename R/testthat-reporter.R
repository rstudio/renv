
renv_tests_reporter <- function() {

  # if we're running on CI, use the default reporter
  if (!is.na(Sys.getenv("CI", unset = NA)))
    return(testthat::default_reporter())

  # if we can't load R6, use the default reporter
  if (!requireNamespace("R6", quietly = TRUE))
    return(testthat::default_reporter())

  # silence R CMD check
  self <- super <- NULL

  # otherwise, define and return instance of our reporter
  R6::R6Class(

    classname = "RenvReporter",
    inherit   = testthat::CheckReporter,

    lock_class   = FALSE,
    lock_objects = FALSE,

    public = list(

      .time = NULL,
      .expectations = list(),

      start_file = function(filename) {
        cli::cat_rule(filename)
        super$start_file(filename)
      },

      end_file = function() {

        if (empty(self$.expectations))
          cli::cat_line("[no test results to report]")

        cli::cat_line()
        super$end_file()

      },

      add_result = function(context, test, result) {

        # store our expectation result
        self$.expectations[[length(self$.expectations) + 1]] <- result

        # record skips but don't print when they occur
        if (inherits(result, "expectation_skip")) {
          message <- sprintf("[%s]: %s", test, result$message)
          self$n_skip <- self$n_skip + 1
          self$skips$push(message)
        } else {
          super$add_result(context, test, result)
        }

      },

      start_test = function(context, test) {
        self$.time <- Sys.time()
        self$.expectations <- list()
        super$start_test(context, test)
      },

      end_test = function(context, test) {
        elapsed <- difftime(Sys.time(), self$.time)
        renv_tests_report(test, elapsed, self$.expectations)
        super$end_test(context, test)
      }

    )

  )

}
