
renv_base64_table <- function() {
  renv_global("base64.table", {
    text <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
    as.integer(charToRaw(text))
  })
}

renv_base64_encode_main <- function(input) {

  ni <- as.integer(length(input))
  if (ni < 3L)
    return(integer())

  no <- ni %/% 3L * 4L
  output <- integer(no)

  i0 <- seq.int(1L, ni - 2L, by = 3L)
  i1 <- seq.int(2L, ni - 1L, by = 3L)
  i2 <- seq.int(3L, ni - 0L, by = 3L)

  o0 <- seq.int(1L, no - 3L, by = 4L)
  o1 <- seq.int(2L, no - 2L, by = 4L)
  o2 <- seq.int(3L, no - 1L, by = 4L)
  o3 <- seq.int(4L, no - 0L, by = 4L)

  t <- renv_base64_table()

  output[o0] <- t[1L + bitwShiftR(input[i0], 2L)]

  output[o1] <- t[1L + bitwOr(
    bitwShiftL(bitwAnd(input[i0], 0x03L), 4L),
    bitwShiftR(bitwAnd(input[i1], 0xF0L), 4L)
  )]

  output[o2] <- t[1L + bitwOr(
    bitwShiftL(bitwAnd(input[i1], 0x0FL), 2L),
    bitwShiftR(bitwAnd(input[i2], 0xC0L), 6L)
  )]

  output[o3] <- t[1L + bitwAnd(input[i2], 0x3FL)]

  output

}

renv_base64_encode_rest <- function(input) {

  ni <- as.integer(length(input))
  remaining <- ni %% 3L
  if (remaining == 0L)
    return(integer())

  output <- rep.int(61L, 4L)
  i <- ni - remaining + 1
  t <- renv_base64_table()

  output[1L] <- t[1L + bitwShiftR(input[i + 0L], 2L)]

  if (remaining == 1L) {

    output[2L] <- t[1L + bitwShiftL(bitwAnd(input[i + 0L], 0x03L), 4L)]

  } else if (remaining == 2L) {

    output[2L] <- t[1L + bitwOr(
      bitwShiftL(bitwAnd(input[i + 0L], 0x03L), 4L),
      bitwShiftR(bitwAnd(input[i + 1L], 0xF0L), 4L)
    )]

    output[3L] <- t[1L + bitwShiftL(bitwAnd(input[i + 1L], 0x0FL), 2L)]

  }

  output

}

renv_base64_encode <- function(text) {

  input <- if (is.character(text))
    as.integer(charToRaw(text))
  else
    as.integer(as.raw(text))

  encoded <- c(
    renv_base64_encode_main(input),
    renv_base64_encode_rest(input)
  )

  rawToChar(as.raw(encoded))
}
