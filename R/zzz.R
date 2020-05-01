
.onLoad <- function(libname, pkgname) {

  renv_patch_init()
  renv_paths_init()
  renv_libpaths_init()
  renv_filebacked_init()
  renv_platform_init()
  renv_envvars_init()

  addTaskCallback(renv_repos_init_callback)

}

.onAttach <- function(libname, pkgname) {
  renv_rstudio_fixup()
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

  TRUE

}

renv_zzz_bootstrap <- function() {

  source <- "templates/template-activate.R"
  target <- "inst/resources/activate.R"

  bootstrap <- readLines("R/bootstrap.R")
  bootstrap <- paste(" ", bootstrap)
  bootstrap <- paste(bootstrap, collapse = "\n")

  template <- renv_file_read(source)
  replaced <- renv_template_replace(template, list(BOOTSTRAP = bootstrap))

  printf("* Generating 'inst/resources/activate.R' ... ")
  writeLines(replaced, con = target)
  writef("Done!")

}

renv_zzz_run()
