
renv <- new.env(parent = new.env())

renv$imports <- ..imports..

renv$initialize <- function() {

  attr(renv, "name") <- "embedded:renv"
  attr(parent.env(renv), "name") <- "imports:renv"

  for (package in names(renv$imports)) {
    namespace <- asNamespace(package)
    functions <- renv$imports[[package]]
    list2env(mget(functions, envir = namespace), envir = parent.env(renv))
  }

  script <- system.file("vendor/renv.R", package = .packageName)
  sys.source(script, envir = renv)

}
