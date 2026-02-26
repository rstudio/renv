
spinner <- function(label, n) {

  .label <- label
  .n <- n
  .i <- 0L

  reset <- function(label, n) {
    .label <<- label
    .n <<- n
    .i <<- 0L
  }

  update <- function(items) {

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

  list(
    reset  = reset,
    update = update,
    clear  = clear,
    tick   = tick,
    done   = done
  )

}
