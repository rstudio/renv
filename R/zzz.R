
.onLoad <- function(libname, pkgname) {

  renv_platform_init()
  renv_methods_init()
  renv_patch_init()
  renv_envvars_init()
  renv_libpaths_init()
  renv_filebacked_init()

  addTaskCallback(renv_repos_init_callback)
  addTaskCallback(renv_snapshot_auto_callback)

}

.onAttach <- function(libname, pkgname) {
  renv_rstudio_fixup()
  renv_exports_attach()
}

renv_zzz_run <- function() {

  # only run when devtools::document() is called
  ok <- FALSE
  document <- parse(text = "devtools::document")[[1]]
  for (call in sys.calls()) {
    if (identical(call[[1]], document)) {
      ok <- TRUE
      break
    }
  }

  if (!ok)
    return(FALSE)

  renv_zzz_bootstrap()
  renv_zzz_docs()

  TRUE

}

renv_zzz_bootstrap <- function() {

  source <- "templates/template-activate.R"
  target <- "inst/resources/activate.R"

  # read the necessary bootstrap scripts
  scripts <- c("R/bootstrap.R", "R/json-read.R")
  contents <- map(scripts, readLines)
  bootstrap <- unlist(contents)

  # format nicely for insertion
  bootstrap <- paste(" ", bootstrap)
  bootstrap <- paste(bootstrap, collapse = "\n")

  # replace template with bootstrap code
  template <- renv_file_read(source)
  replaced <- renv_template_replace(template, list(BOOTSTRAP = bootstrap))

  # write to resources
  printf("* Generating 'inst/resources/activate.R' ... ")
  writeLines(replaced, con = target)
  writef("Done!")

}

renv_zzz_docs <- function() {

  reg.finalizer(globalenv(), function(object) {

    printf("* Copying vignettes to 'inst/doc' ... ")

    ensure_directory("inst/doc")

    files <- list.files("vignettes")
    src <- file.path("vignettes", files)
    tgt <- file.path("inst/doc", files)
    file.copy(src, tgt, overwrite = TRUE)

    writef("Done!")

  }, onexit = TRUE)
}

if (identical(.packageName, "renv"))
  renv_zzz_run()
