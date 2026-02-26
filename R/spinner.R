
spinner <- function(label, n, width) {

  .label <- label
  .n <- n
  .i <- 0L
  .width <- width
  .nwidth <- nchar(as.character(n))
  .last <- 0L

  reset <- function(label, n) {
    .label <<- label
    .n <<- n
    .i <<- 0L
    .nwidth <<- nchar(as.character(n))
  }

  update <- function(items) {

    max <- 4L
    detail <- if (length(items) <= max) {
      paste(items, collapse = ", ")
    } else {
      paste0(paste(head(items, max), collapse = ", "), ", ...")
    }

    suffix <- sprintf("(%*i/%i)", .nwidth, .i, .n)
    body <- sprintf("  %s: %s", .label, detail)
    body <- strtrim(body, .width)
    msg <- paste(format(body, width = .width), suffix)
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
