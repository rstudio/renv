
`_renv_curl_valid` <- new.env(parent = emptyenv())

renv_curl_exe <- function() {

  curl <- Sys.getenv("RENV_CURL_EXECUTABLE", unset = NA)
  if (is.na(curl))
    curl <- Sys.which("curl")

  if (!nzchar(curl))
    return(renv_curl_exe_missing(curl))

  renv_curl_validate(curl)

}

renv_curl_validate <- function(curl) {

  `_renv_curl_valid`[[curl]] <- `_renv_curl_valid`[[curl]] %??% {
    renv_curl_validate_impl(curl)
  }

}

renv_curl_validate_impl <- function(curl) {

  # make sure we can run this copy of curl
  # note that 'system2()' will give an error if curl isn't runnable at all
  output <- suppressWarnings(
    tryCatch(
      system2(
        command = curl,
        args = "--version",
        stdout = TRUE,
        stderr = TRUE
      ),
      error = identity
    )
  )

  if (!inherits(output, "error")) {
    status <- attr(output, "status") %??% 0L
    if (status == 0L)
      return(curl)
  }

  message <- if (inherits(output, "error"))
    conditionMessage(output)
  else
    output

  fmt <- "Error executing '%s --version': is your copy of curl functional?"
  footer <- sprintf(fmt, curl)
  all <- c("", header(paste(curl, "--version"), prefix = "$"), message, "", footer)

  envir <- renv_dynamic_envir()
  defer(
    message(paste(all, collapse = "\n")),
    envir = envir
  )

  return(curl)

}

renv_curl_exe_missing <- function(curl) {

  if (!once())
    return(invisible(curl))

  parts <- c(
    "curl does not appear to be installed; downloads will fail.",
    "See <https://rstudio.github.io/renv/articles/renv.html#downloads> for more information."
  )

  msg <- paste(parts, collapse = "\n")
  warning(msg, call. = FALSE)

  invisible(curl)

}
