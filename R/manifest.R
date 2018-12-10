
renv_manifest_load <- function(project = NULL) {

  project <- renv_active_project(project)
  path <- file.path(project, "renv/manifest")
  if (file.exists(path))
    return(renv_manifest_read(path))

  renv_diagnose(project)

}

renv_manifest_read <- function(file) {

  contents <- read(file)
  parts <- strsplit(contents, "\n{2,}")[[1]]

  config <- renv_config_read(text = parts[[1]])
  parts <- tail(parts, n = -1L)

  library <- lapply(parts, function(part) {
    pieces <- strsplit(part, "\n", fixed = TRUE)[[1]]
    idx <- regexpr(":", pieces, fixed = TRUE)
    as.list(named(
      trimws(substring(pieces, idx + 1)),
      trimws(substring(pieces, 1, idx - 1))
    ))
  })
  names(library) <- map_chr(library, `[[`, "Package")

  list(config = config, library = library)

}

renv_manifest_write <- function(manifest, file) {

  header <- renv_manifest_serialize_config(manifest)
  body <- renv_manifest_serialize_library(manifest)
  contents <- paste(header, body, sep = "\n\n")
  write_lines(contents, con = file)

}

renv_manifest_serialize_config <- function(manifest) {
  renv_config_write(manifest$config, path = NULL, comment = FALSE)
}

renv_manifest_serialize_library <- function(manifest) {
  parts <- map_chr(manifest$library, function(entry) {
    paste(names(entry), entry, sep = ": ", collapse = "\n")
  })
  paste(parts, collapse = "\n\n")
}

renv_manifest_diff <- function(old, new) {

  old <- old$library; new <- new$library
  packages <- named(union(names(old), names(new)))
  actions <- lapply(packages, function(package) {

    before <- old[[package]]; after <- new[[package]]

    case(
      is.null(before) ~ "install",
      is.null(after)  ~ "remove",

      before$Version < after$Version ~ "upgrade",
      before$Version > after$Version ~ "downgrade",
      before$Source != after$Source  ~ "crossgrade"
    )

  })

  Filter(Negate(is.null), actions)

}
