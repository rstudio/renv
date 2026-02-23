
spinner <- function(label, n, width) {

  .label <- label
  .n <- n
  .i <- 0L
  .width <- width

  reset <- function(label, n) {
    .label <<- label
    .n <<- n
    .i <<- 0L
  }

  update <- function(items) {

    max <- 4L
    detail <- if (length(items) <= max) {
      paste(items, collapse = ", ")
    } else {
      paste0(paste(head(items, max), collapse = ", "), ", ...")
    }

    suffix <- sprintf("(%i/%i)", .i, .n)
    body <- sprintf("  %s: %s", .label, detail)
    msg <- paste0(format(body, width = .width), suffix)
    printf("\r%s", format(msg, width = .width + nchar(suffix)))
    flush(stdout())

  }

  clear <- function() {
    printf("\r%s\r", strrep(" ", .width + 24L))
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
