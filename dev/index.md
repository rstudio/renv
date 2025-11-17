# renv

## Overview

The renv package[¹](#fn1) helps you create **r**eproducible
**env**ironments for your R projects. Use renv to make your R projects
more isolated, portable and reproducible.

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

Alternatively, install the development version from
[r-universe](https://rstudio.r-universe.dev/renv) with:

``` r
install.packages("renv", repos = "https://rstudio.r-universe.dev")
```

## Workflow

![A diagram showing the most important verbs and nouns of renv. Projects
start with init(), which creates a project library using packages from
the system library. snapshot() updates the lockfile using the packages
installed in the project library, where restore() installs packages into
the project library using the metadata from the lockfile, and status()
compares the lockfile to the project library. You install and update
packages from CRAN and GitHub using install() and update(), but because
you'll need to do this for multiple projects, renv uses cache to make
this fast.](articles/renv.png)

Use
[`renv::init()`](https://rstudio.github.io/renv/dev/reference/init.md)
to initialize renv in a new or existing project. This will set up a
**project library**, containing all the packages you’re currently using.
The packages (and all the metadata needed to reinstall them) are
recorded into a **lockfile**, `renv.lock`, and a `.Rprofile` ensures
that the library is used every time you open that project.

As you continue to work on your project, you will install and upgrade
packages, either using
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
and [`update.packages()`](https://rdrr.io/r/utils/update.packages.html)
or
[`renv::install()`](https://rstudio.github.io/renv/dev/reference/install.md)
and
[`renv::update()`](https://rstudio.github.io/renv/dev/reference/update.md).
After you’ve confirmed your code works as expected, use
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
to record the packages and their sources in the lockfile.

Later, if you need to share your code with someone else or run your code
on new machine, your collaborator (or you) can call
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
to reinstall the specific package versions recorded in the lockfile.

## Learning more

If this is your first time using renv, we strongly recommend starting
with the [Introduction to
renv](https://rstudio.github.io/renv/articles/renv.html) vignette: this
will help you understand the most important verbs and nouns of renv.

If you have a question about renv, please first check the
[FAQ](https://rstudio.github.io/renv/articles/faq.html) to see whether
your question has already been addressed. If it hasn’t, please feel free
to ask on the [Posit Forum](https://forum.posit.co).

If you believe you’ve found a bug in renv, please file a bug (and, if
possible, a [reproducible example](https://reprex.tidyverse.org)) at
<https://github.com/rstudio/renv/issues>.

------------------------------------------------------------------------

1.  Pronounced “R” “env”
