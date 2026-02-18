
spinner <- function(label, n, width = 80L) {

  .label <- label
  .n <- n
  .width <- width

  .frames <- c(
    "\u280B", "\u2819", "\u2839",
    "\u2838", "\u28B0", "\u28E0",
    "\u28E4", "\u2846", "\u2807"
  )

  .i <- 0L
  .done <- 0L

  .last <- 0L

  list(

    update = function(items) {

      .i <<- .i %% length(.frames) + 1L

      max <- 4L
      detail <- if (length(items) <= max) {
        paste(items, collapse = ", ")
      } else {
        paste0(paste(head(items, max), collapse = ", "), ", ...")
      }

      body <- sprintf("  %s %s: %s", .frames[[.i]], .label, detail)
      suffix <- sprintf("(%i/%i)", .done, .n)
      msg <- paste0(format(body, width = .width - nchar(suffix)), suffix)
      .last <<- nchar(msg)
      printf("\r%s", msg)
      flush(stdout())

    },

    completed = function() {
      .done <<- .done + 1L
    },

    clear = function() {
      printf("\r%s\r", strrep(" ", max(.width, .last)))
      flush(stdout())
    }

  )

}
