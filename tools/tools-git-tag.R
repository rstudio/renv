
options(renv.verbose = TRUE)

# iterate through old commits of renv, and update tags for each commit
devtools::load_all()

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
  status <- system("git checkout HEAD~1 &> /dev/null")
  if (status != 0)
    break

}

vwritef("* Updating tags")
system("git push --tags")
setwd(owd)
