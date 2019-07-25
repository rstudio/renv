
renv_lockfile_encoder_repos <- function() {
  list(encode = renv_repos_encode, decode = renv_repos_decode)
}

# special encode / decode definitions for certain fields
# which need special treatment
renv_lockfile_serializer_list <- function() {

  list(

    R = list(
      Repositories = renv_lockfile_encoder_repos()
    ),

    Bioconductor = list(
      Repositories = renv_lockfile_encoder_repos()
    )

  )

}

renv_lockfile_serializer <- function(section, key) {

  delim <- gregexpr("/", section, fixed = TRUE)[[1]]

  labels <- section
  if (!identical(c(delim), -1L))
    labels <- substring(section, 1, c(delim - 1, nchar(section)))

  entries <- renv_lockfile_serializer_list()
  for (label in rev(labels)) {
    entry <- entries[[label]]
    if (!is.null(entry) && !is.null(entry[[key]]))
      return(entry[[key]])
  }

  list(
    encode = renv_lockfile_serializer_encode_default,
    decode = renv_lockfile_serializer_decode_default
  )

}

renv_lockfile_serializer_encode_default <- function(value) {
  toString(value)
}

renv_lockfile_serializer_decode_default <- function(value) {

  if (value %in% c("TRUE", "FALSE"))
    return(as.logical(value))

  strsplit(value, "\\s*,\\s*")[[1]]

}
