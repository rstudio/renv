
renv <- new.env(parent = new.env())

renv$initialize <- function(libname, pkgname) {

  # set up renv + imports environments
  attr(renv, "name") <- "embedded:renv"
  attr(parent.env(renv), "name") <- "imports:renv"

  # get imports
  imports <- ..imports..

  # load the imports required by renv
  for (package in names(imports)) {
    namespace <- asNamespace(package)
    functions <- imports[[package]]
    list2env(mget(functions, envir = namespace), envir = parent.env(renv))
  }

  # source renv into the aforementioned environment
  script <- system.file("vendor/renv.R", package = .packageName)
  sys.source(script, envir = renv)

  # initialize metadata
  renv$the$metadata <- ..metadata..

  # run our load / attach hooks so internal state is initialized
  renv$.onLoad(libname, pkgname)

  # remove our initialize method when we're done
  rm(list = "initialize", envir = renv)

}
