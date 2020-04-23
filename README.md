
# renv<a href="https://rstudio.github.io/renv"><img src="man/figures/logo.svg" align="right" height="115" /></a>

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN status](https://www.r-pkg.org/badges/version/renv)](https://CRAN.R-project.org/package=renv)
[![Travis build status](https://travis-ci.org/rstudio/renv.svg?branch=master)](https://travis-ci.org/rstudio/renv)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rstudio/renv?branch=master&svg=true)](https://ci.appveyor.com/project/rstudio/renv)
[![Codecov test coverage](https://codecov.io/gh/rstudio/renv/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/renv?branch=master)
<!-- badges: end -->

## Overview

The `renv` package helps you create **r**eproducible **env**ironments for
your R projects. Use `renv` to make your R projects more:

**Isolated**: Installing a new or updated package for one project won't break
your other projects, and vice versa. That's because `renv` gives each project
its own private package library.

**Portable**: Easily transport your projects from one computer to another, even
across different platforms. `renv` makes it easy to install the packages your
project depends on.

**Reproducible**: `renv` records the exact package versions you depend on, and
ensures those exact versions are the ones that get installed wherever you go.


## Installation

Install the latest development version of `renv` with:

```r
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")
```


## Workflow

Use `renv::init()` to initialize `renv` with a new or existing project. This
will set up your project with a private library, and also make sure to install
all of the packages you're using into that library. The packages used in your
project will be recorded into a *lockfile*, called `renv.lock`.

As you work in your project, you may need to install or upgrade different
packages. As these packages are installed, `renv` will automatically write
`renv.lock` for you. The `renv.lock` lockfile records the state of your project's
private library, and can be used to restore the state of that library as required.

Later, if you need to port your project to a new machine, you can call
`renv::restore()` to reinstall all of the packages as declared in the lockfile.


## Learning More

You can browse the package documentation online at
<https://rstudio.github.io/renv/>.

If this is your first time using `renv`, we strongly recommend reading the
[Introduction to renv](https://rstudio.github.io/renv/articles/renv.html)
vignette.

If you have a question about `renv`, please first check the
[FAQ](https://rstudio.github.io/renv/articles/faq.html) to see whether your
question has already been addressed. If it hasn't, please feel free to either
ask on the [RStudio Community forums](https://community.rstudio.com).

If you believe you've found a bug in `renv`, please file a bug (and, if
possible, a [reproducible example](https://reprex.tidyverse.org)) at
<https://github.com/rstudio/renv/issues>.
