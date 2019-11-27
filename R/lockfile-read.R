
renv_lockfile_read_finish_impl <- function(key, val) {

  # convert repository records to named vectors
  # (be careful to handle NAs, NULLs)
  if (identical(key, "Repositories") && is.null(names(val))) {

    getter <- function(name) function(record) record[[name]] %||% "" %NA% ""
    keys <- map_chr(val, getter("Name"))
    vals <- map_chr(val, getter("URL"))

    result <- case(
      empty(keys)       ~ list(),
      any(nzchar(keys)) ~ named(vals, keys),
      TRUE              ~ vals
    )

    return(as.list(result))

  }

  # recurse for lists
  if (is.list(val))
    return(enumerate(val, renv_lockfile_read_finish_impl))

  # return other values as-is
  val

}

renv_lockfile_read_finish <- function(data) {

  # port old packages format
  packages <- data$R$Packages %||% data$R$Package
  if (!is.null(packages)) {
    renv_records(data) <- packages
    data$R$Package <- data$R$Packages <- NULL
  }

  # convert repository fields
  data <- enumerate(data, renv_lockfile_read_finish_impl)

  # set class
  class(data) <- "renv_lockfile"
  data

}

renv_lockfile_read <- function(file = NULL, text = NULL) {

  contents <- if (is.null(file))
    unlist(strsplit(text, "\n", fixed = TRUE))
  else
    readLines(file, warn = FALSE, encoding = "UTF-8")

  # try reading as JSON
  json <- catch(renv_json_read(text = contents))
  if (!inherits(json, "error"))
    return(renv_lockfile_read_finish(json))

  # fall back to internal format
  contents <- grep("^\\s*[#]", contents, value = TRUE, invert = TRUE)
  contents <- contents[nzchar(contents)]
  contents <- paste(contents, collapse = "\n")

  splat <- strsplit(contents, "\\n+(?=\\[)", perl = TRUE)[[1]]

  idx <- regexpr("(?:\\n|$)", splat)
  section <- substring(splat, 1, idx - 1)
  body <- substring(splat, idx + 1)
  names(body) <- substring(section, 2, nchar(section) - 1)

  fields <- enumerate(body, renv_lockfile_read_fields)

  data <- list()
  enumerate(fields, function(section, entries) {
    splat <- strsplit(section, "/", fixed = TRUE)[[1]]

    for (i in seq_len(length(splat) - 1)) {
      k <- splat[1:i]
      if (is.null(data[[k]]))
        data[[k]] <<- list()
    }

    data[[splat]] <<- entries
  })

  renv_lockfile_read_finish(data)

}

renv_lockfile_read_fields <- function(section, fields) {
  parts <- strsplit(fields, "\\n(?!\\s)", perl = TRUE)[[1]]

  idx <- regexpr("=", parts, fixed = TRUE)
  keys <- substring(parts, 1, idx - 1)
  vals <- trimws(substring(parts, idx + 1))
  names(vals) <- keys

  enumerate(vals, renv_lockfile_read_field, section = section)
}

renv_lockfile_read_field <- function(key, value, section) {
  serializer <- renv_lockfile_serializer(section, key)
  serializer$decode(value)
}
