
# global variables
the <- new.env(parent = emptyenv())

# detect if we're running within R CMD build
building <- function() {
  nzchar(Sys.getenv("R_CMD")) &&
    grepl("Rbuild", basename(dirname(getwd())), fixed = TRUE)
}
