
options(renv.verbose = TRUE)

# poor man's load_all()
envir <- attach(NULL, name = "tools:renv")
assign(".packageName", "renv", envir = envir)
files <- list.files("R", full.names = TRUE)
for (file in files)
  sys.source(file, envir = envir)
invisible(envir$.onLoad("", "renv"))
invisible(envir$.onAttach("", "renv"))

# copy package sources to temporary directory
dir <- tempfile("renv-")
renv_file_copy(getwd(), dir)
owd <- setwd(dir)

# track which versions have been seen
versions <- new.env(parent = emptyenv())

# get the set of tags
tags <- system("git tag --list", intern = TRUE)

repeat {

  # get current version of package
  descpath <- file.path(getwd(), "DESCRIPTION")
  desc <- renv_description_read(descpath)
  version <- desc$Version

  # bail if we've already tagged this
  if (version %in% tags)
    break

  # attempt to tag this commit
  vwritef("* Tagging version '%s'", version)
  system(paste("git tag", version))

  # go to the previous commit
  args <- c("checkout", shQuote("HEAD~1"))
  status <- system2("git", args, stdout = FALSE, stderr = FALSE)
  if (status != 0)
    break

}

vwritef("* Updating tags")
system("git push --tags")
setwd(owd)
