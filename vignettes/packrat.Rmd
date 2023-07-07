---
title: "packrat vs. renv"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{packrat vs. renv}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r setup}
library(renv)
```

Our first attempt at solving the problem of reproducible environments was [packrat](https://rstudio.github.io/packrat/).
While we learned a lot from packrat, it ultimately proved challenging to use in ways that were difficult to fix.
This led us to create renv, a system with fewer surprises and better defaults.
If you're an existing packrat user, you can call `renv::migrate()` to switch renv, then read on to learn about the main differences:

1.  The renv lockfile `renv.lock` is formatted as [JSON](https://www.json.org/).
    This should make the lockfile easier to use and consume with other tools.

2.  renv no longer attempts to explicitly download and track R package source tarballs within your project.
    This was a frustrating default that operated under the assumption that you might later want to be able to restore a project's private library without access to a CRAN repository.
    In practice, this is almost never the case, and the time spent downloading + storing the package sources seemed to outweigh the potential reproducibility benefits.

3.  Packrat tried to maintain the distinction between so-called *stale* packages; that is, R packages which were installed by Packrat but were not recorded in the lockfile for some reason.
    This distinction was (1) overall not useful, and (2) confusing.
    renv no longer makes this distinction: `snapshot()` saves the state of your project library to `renv.lock`, `restore()` loads the state of your project library from `renv.lock`, and that's all.

4.  In renv, the global package cache is enabled by default.
    This should reduce overall disk-space usage as packages can effectively be shared across each project using renv.

5.  renv's dependency discovery machinery is more configurable.
    The function `renv::dependencies()` is exported, and users can create `.renvignore` files to instruct renv to ignore specific files and folders in their projects.
    (See `?renv::dependencies` for more information.)
