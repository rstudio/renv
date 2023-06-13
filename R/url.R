
renv_url_parse <- function(url) {

  pattern <- paste0(
    "^",
    "([^:]+://)?",        # protocol
    "([^/?#]+)",          # domain
    "(?:(/[^?#]*))?",     # path
    "(?:[?]([^#]+))?",    # parameters
    "(?:#(.*))?",         # fragment
    ""
  )

  matches <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
  if (length(matches) != 6L)
    stopf("couldn't parse url '%s'", url)

  matches <- as.list(matches)
  names(matches) <- c("url", "protocol", "domain", "path", "parameters", "fragment")

  # parse parameters into named list
  matches$parameters <- renv_properties_read(
    text = chartr("&", "\n", matches$parameters),
    delimiter = "=",
    dequote = FALSE,
    trim = FALSE
  )

  # return parsed URL
  matches

}

