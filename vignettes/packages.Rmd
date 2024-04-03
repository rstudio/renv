---
title: "Package development"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Package development}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
```


## Development

Often, R packages will have other R packages as dependencies. For this, one must
declare their R package dependencies within the package `DESCRIPTION` file. If
you want to prepare your environment for package development, you can use:

```
renv::install()
```

to install the packages as declared in the package's `DESCRIPTION` file. This
action is roughly analogous to `remotes::install_deps()`.

If you're developing a package that you intend to release to CRAN, then you
likely want to build and test your package against the latest versions of your
dependencies as available on CRAN. For this, you should consider using:

```
renv::update()
```

to ensure your package dependencies are up-to-date, as appropriate.


## Isolation

Normally, a package under development should be tested against the
latest-available versions of its dependencies on CRAN. However, in some cases,
you may need to ensure your package is compatible with other packages also
currently under development.

In these cases, the renv project library can be useful -- you can install the
development version(s) of your dependencies into the project library, without
worrying about clobbering any packages already installed in your system library.

In these cases, you can declare your development dependencies using the
`Remotes` field of the `DESCRIPTION` file; e.g.

```
Remotes:
  r-lib/ggplot2
```

and `renv::install()` will parse that remotes declaration and retrieve the
requested package. See the remotes vignette,
[Dependency resolution for R package development][dependencies],
for more details.


## Library Paths

For package projects using renv, a library path outside of the project
directory will be used instead. As an example, on macOS, this might look like:

```
> .libPaths()
[1] "/Users/kevin/Library/Caches/org.R-project.R/R/renv/library/example-552f6e80/R-4.3/aarch64-apple-darwin20"
[2] "/Users/kevin/Library/Caches/org.R-project.R/R/renv/sandbox/R-4.3/aarch64-apple-darwin20/ac5c2659"
```

This is done to avoid issues with `R CMD build`, which can become very slow if
your project contains a large number of files -- as can happen if the project
library is located in `renv/library` (the typical location). Note that even
though the library is located outside of the project, the library path
generated will still be unique to that project, and so the project is still
effectively isolated in the same way as other renv projects normally are.

If you want to customize the location where `renv` places project libraries in
this scenario, you can use the `RENV_PATHS_LIBRARY_ROOT` environment variable.
For example:

```
RENV_PATHS_LIBRARY_ROOT = ~/.renv/library
```

If you'd still prefer to keep your project library within the project directory,
you can set:

```
RENV_PATHS_LIBRARY = renv/library
```

within an appropriate `.Renviron` start-up profile -- but please be aware of
the caveats to doing this, as the performance of `R CMD build` will be affected.


## Testing

While developing your package, you may want to use a continuous integration
service (such as [Travis CI](https://www.travis-ci.com)) to build and test
your package remotely. You can use renv to help facilitate this testing --
see the [Continuous Integration](ci.html) vignette for more information. In
particular, clever use of the renv cache can help save time that might
normally be spent on package installation. See
<https://github.com/rstudio/renv/blob/main/.github/workflows/R-CMD-check.yaml>
for an example of how renv uses itself for package management in its own
CI tests.


## Submitting to CRAN

Note that packages submitted to CRAN should be designed to work with the other R
packages currently available on CRAN. For that reason, when preparing your package
for submission, you'll need to ensure your source package tarball does not
include any `renv` infrastructure. `renv` makes this easy by automatically
including

```
^renv$
^renv\.lock$
```

in your package's `.Rbuildignore` file. This instructs `R CMD build` to not
include these files and folders in the generated package tarball. Through this,
even if `renv` is used during package development, it's still easy to build and
publish your package to CRAN as you would when developing packages without `renv`.


[dependencies]: https://remotes.r-lib.org/articles/dependencies.html
