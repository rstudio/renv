
renv_manifest_read <- function(file) {

  contents <- read(file)
  splat <- strsplit(contents, "\\n+(?=\\[)", perl = TRUE)[[1]]

  idx <- regexpr("\n", splat, fixed = TRUE)
  section <- substring(splat, 1, idx - 1)
  body <- substring(splat, idx + 1)
  names(body) <- substring(section, 2, nchar(section) - 1)

  fields <- enumerate(body, renv_manifest_read_fields)

  data <- list()
  enumerate(fields, function(section, entries) {
    splat <- strsplit(section, ".", fixed = TRUE)[[1]]

    for (i in seq_len(length(splat) - 1)) {
      k <- splat[1:i]
      if (is.null(data[[k]]))
        data[[k]] <<- list()
    }

    data[[splat]] <<- entries
  })

  data

}

renv_manifest_read_fields <- function(section, fields) {
  parts <- strsplit(fields, "\\n(?!\\s)", perl = TRUE)[[1]]

  idx <- regexpr("=", parts, fixed = TRUE)
  keys <- substring(parts, 1, idx - 1)
  vals <- trimws(substring(parts, idx + 1))
  names(vals) <- keys

  enumerate(vals, renv_manifest_read_field, section = section)
}

renv_manifest_read_field <- function(key, value, section) {
  serializer <- renv_manifest_serializer(section, key)
  serializer$decode(value)
}
