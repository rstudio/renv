# renv

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Overview

The renv package creates **r**eproducible **env**ironments that increase the
reproducibility of your code, so it return the same results wherever and 
whenever it is run. renv uses two tools to make this possible:

* A __lockfile__, `renv.lock`, records the precise details of installed packages
  so that anyone can reinstall exactly the same versions as you're using.
  (This is simple for CRAN packages, but requires extra details for packages 
  installed from GitHub.)
  
* A __project library__, `renv/library`, is directory of installed R packages.
  Usually, packages are installed in __system library__. This is convenient 
  but means every project must use the same version of every package. A project
  library means that the dependencies of a project can evolve independently.

If you're familiar with [packrat][packrat], the goal is for renv to be a robust, 
stable replacement, with fewer surprises and better default behaviors.

## Workflow

renv has three main function:

1.  `renv::snapshot()` creates a lockfile describing all the versions of all
    packages used by the current project. This allows you to share the exact
    state of dependencies with other people, and rollback to known good versions
    if later updated break your code.

1.  `renv::init()` creates a project local environment, including a project 
    library, and default `.gitignore`, `.Rbuildignore` and `.Rprofile` files
    designed for optimal results. 

1. `renv::restore()` recreates the project library from the lockfile. Use this
    if 

See the [Introduction vignette](https://rstudio.github.io/renv/articles/introduction.html)
for more details.

[packrat]: https://rstudio.github.io/packrat/
