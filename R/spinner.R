
spinner <- function(label, n, width) {

  .label <- label
  .n <- n
  .i <- 0L
  .width <- width
  .last <- 0L

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

    msg <- sprintf("  (%i/%i) %s: %s", .i, .n, .label, detail)
    msg <- strtrim(msg, .width)
    msg <- format(msg, width = max(nchar(msg), .last))
    printf("\r%s", msg)
    flush(stdout())
    .last <<- nchar(msg)
  }

  clear <- function() {
    printf("\r%s\r", strrep(" ", .last))
    flush(stdout())
    .last <<- 0L
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
