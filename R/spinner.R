
spinner <- function(label, n) {

  .label <- label
  .n <- n
  .i <- 0L
  .cursor <- TRUE

  reset <- function(label, n) {
    .label <<- label
    .n <<- n
    .i <<- 0L
  }

  update <- function(items) {

    if (.cursor) {
      cat("\033[?25l", file = stdout())
      .cursor <<- FALSE
    }

    max <- 4L
    if (length(items) > max)
      items <- c(head(items, max), "...")

    detail <- paste(items, collapse = ", ")
    width <- getOption("width", 80L)

    msg <- sprintf("  (%i/%i) %s: %s", .i, .n, .label, detail)
    printf("\r%s", format(strtrim(msg, width), width = width))
    flush(stdout())

  }

  clear <- function() {
    width <- getOption("width", 80L)
    printf("\r%s\r", strrep(" ", width))
    flush(stdout())
  }

  tick <- function() {
    .i <<- .i + 1L
  }

  done <- function() {
    .i >= .n
  }

  restore <- function() {
    if (!.cursor) {
      cat("\033[?25h", file = stdout())
      .cursor <<- TRUE
    }
  }

  list(
    reset   = reset,
    update  = update,
    clear   = clear,
    tick    = tick,
    done    = done,
    restore = restore
  )

}
