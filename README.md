
<!-- README.md is generated from README.Rmd. Please edit that file -->

# renv <img src="man/figures/logo.svg" align="right" height="115"/>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/renv)](https://CRAN.R-project.org/package=renv)
[![R-CMD-check](https://github.com/rstudio/renv/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/renv/actions)

<!-- badges: end -->

## Overview

The renv package[^1] helps you create **r**eproducible **env**ironments
for your R projects. Use renv to make your R projects more isolated,
portable and reproducible.

- **Isolated**: Installing a new or updated package for one project
  won’t break your other projects, and vice versa. That’s because renv
  gives each project its own private library.
- **Portable**: Easily transport your projects from one computer to
  another, even across different platforms. renv makes it easy to
  install the packages your project depends on.
- **Reproducible**: renv records the exact package versions you depend
  on, and ensures those exact versions are the ones that get installed
  wherever you go.

## Installation

Install the latest version of renv from CRAN with:

``` r
install.packages("renv")
```

## Workflow

<img src="vignettes/renv.png" alt="A diagram showing the most important verbs and nouns of renv. Projects start with init(), which creates a project library using packages from the system library. snapshot() updates the lockfile using the packages installed in the project library, where restore() installs packages into the project library using the metadata from the lockfile, and status() compares the lockfile to the project library. You install and update packages from CRAN and GitHub using install() and update(), but because you'll need to do this for multiple projects, renv uses cache to make this fast." width="408" style="display: block; margin: auto;" />

Use `renv::init()` to initialize renv with a new or existing project.
This will set up your project with a private library, containing all the
packages you’re currently using. The packages (and all the metadata
needed to reinstall them) are also recorded into a *lockfile*,
`renv.lock`.

As you work in your project, you will install and upgrade packages.
After you’ve confirmed your code works as expected, call
`renv::snapshot()` to record their versions in the lockfile.

Later, if you need to share your code with someone else or run your code
on new machine, your collaborator (or you) can call `renv::restore()` to
reinstall the specific package versions recorded in the lockfile.

## Learning more

If this is your first time using renv, we strongly recommend reading the
[Introduction to
renv](https://rstudio.github.io/renv/articles/renv.html) vignette.

If you have a question about renv, please first check the
[FAQ](https://rstudio.github.io/renv/articles/faq.html) to see whether
your question has already been addressed. If it hasn’t, please feel free
to ask on the [RStudio Community forums](https://community.rstudio.com).

If you believe you’ve found a bug in renv, please file a bug (and, if
possible, a [reproducible example](https://reprex.tidyverse.org)) at
<https://github.com/rstudio/renv/issues>.

[^1]: Pronounced “R” “env”
