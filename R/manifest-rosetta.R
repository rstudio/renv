
renv_manifest_serializer_list <- function() {

  list(

    R = list(

      Version = list(
        encode = format,
        decode = numeric_version
      ),

      Libraries = list(
        encode = toString,
        decode = fromString
      ),

      Overlay = list(
        encode = as.character,
        decode = as.logical
      ),

      Repositories = list(
        encode = renv_repos_encode,
        decode = renv_repos_decode
      )

    )

  )

}

renv_manifest_serializer <- function(section, key) {

  dots <- gregexpr(".", section, fixed = TRUE)[[1]]

  labels <- section
  if (!identical(c(dots), -1L))
    labels <- substring(section, 1, c(dots - 1, nchar(section)))

  entries <- renv_manifest_serializer_list()
  for (label in rev(labels)) {
    entry <- entries[[label]]
    if (!is.null(entry) && !is.null(entry[[key]]))
      return(entry[[key]])
  }

  list(
    encode = renv_manifest_serializer_encode_default,
    decode = renv_manifest_serializer_decode_default
  )

}

renv_manifest_serializer_encode_default <- function(value) {
  toString(value)
}

renv_manifest_serializer_decode_default <- function(value) {
  parsed <- catch(parse(text = value)[[1]])
  if (inherits(parsed, "error"))
    return(value)

  if (is.atomic(parsed))
    return(parsed)

  value
}
