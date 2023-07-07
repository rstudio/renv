---
title: "Package sources"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Package sources}
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


# Package sources

renv uses an installed package's `DESCRIPTION` file to infer its source. For
example, packages installed from the CRAN repositories typically have the field:

```
Repository: CRAN
```

set, and renv takes this as a signal that the package was retrieved from CRAN.

## Inferring package sources

The following fields are checked, in order, when inferring a package's source:

1. The `RemoteType` field; typically written for packages installed by the
  devtools, remotes and pak packages,

1. The `Repository` field; for example, packages retrieved from CRAN will
   typically have the `Repository: CRAN` field,
   
1. The `biocViews` field; typically present for packages installed from the
   Bioconductor repositories,

As a fallback, if renv is unable to determine a package's source from the
`DESCRIPTION` file directly, but a package of the same name is available in the
active R repositories (as specified in `getOption("repos")`), then the package
will be treated as though it was installed from an R package repository.

If all of the above methods fail, renv will finally check for a package
available from the _cellar_. The package cellar is typically used as an escape
hatch, for packages which do not have a well-defined remote source, or for
packages which might not be remotely accessible from your machine.


## Unknown sources

If renv is unable to infer a package's source, it will inform you during
`renv::snapshot()` -- for example, if we attempted to snapshot a package
called `skeleton` with no known source:

```
> renv::snapshot()
The following package(s) were installed from an unknown source:

        skeleton

renv may be unable to restore these packages in the future.
Consider reinstalling these packages from a known source (e.g. CRAN).

Do you want to proceed? [y/N]:
```

While you can still create a lockfile with such packages, `restore()` will
likely fail unless you can ensure this package is installed through some
other mechanism.


## Custom R package repositories

Custom and local R package repositories are supported as well. The only
requirement is that these repositories are set as part of the `repos` R
option, and that these repositories are named. For example, you might use:

```
repos <- c(CRAN = "https://cloud.r-project.org", WORK = "https://work.example.org")
options(repos = repos)
```

to tell renv to work with both the official CRAN package repository, as well
as a package repository you have hosted and set up in your work environment.

## Bioconductor


renv has been designed to work together as seamlessly as possible with
the [Bioconductor](https://bioconductor.org/) project. This vignette outlines
some of the extra steps that may be required when using renv with packages
obtained from Bioconductor.


## Initializing a project

To initialize renv in a project using Bioconductor, you can pass the
`bioconductor` argument to `renv::init()`:

```{r, eval = FALSE}
# use the latest-available Bioconductor release
renv::init(bioconductor = TRUE)

# use a specific version of Bioconductor
renv::init(bioconductor = "3.14")
```

This will tell renv to activate the appropriate Bioconductor repositories,
and to use those repositories when attempting to restore packages.

### Bioconductor releases

Bioconductor prepares different versions of its package repositories, for use
with different versions of R. The version of Bioconductor used within a
particular renv project is stored both as a project setting, and also within
the project lockfile. This allows you to "lock" a particular project to a
particular Bioconductor release, even as new Bioconductor releases are made
available for newer versions of R.

To set the version of Bioconductor used in a project, you can use:

```{r, eval = FALSE}
renv::settings$bioconductor.version("3.14")
```

If you later choose to upgrade R, you may need to upgrade the version
of Bioconductor used as well.

If you want to override the Bioconductor repositories used by renv, you can
also explicitly set the following option:

```{r, eval = FALSE}
options(renv.bioconductor.repos = c(...))
```


## The package cellar

In some cases, your project may depend on R packages which are not available
from any external source, or that external source may not be accessible from the
machine calling `renv::restore()`. To help accommodate these scenarios, renv
allows you to prepare a package "cellar", to be used as an ad-hoc repository of
packages during restore. This allows you to provide package tarballs that can be
used to restore packages which cannot be retrieved from any other source.

The environment variable `RENV_PATHS_CELLAR` can be used to customize the
package cellar location.  It should point to a directory containing package
binaries and sources, with a structure of the form:

- `${RENV_PATHS_CELLAR}/<package>_<version>.tar.gz`; or
- `${RENV_PATHS_CELLAR}/<package>/<package>_<version>.tar.gz`

Alternatively, you can also use a project-local cellar by placing your
packages within a folder located at `<project>/renv/cellar`. Note that this
folder does not exist by default; you must create it to opt-in.

- `<project>/renv/cellar/<package>_<version>.tar.gz`; or
- `<project>/renv/cellar/<package>/<package>_<version>.tar.gz`

As an example, if your project depended on a package `skeleton 1.0.0`, you could
place a tarball for this package in one of the following locations:

- `${RENV_PATHS_CELLAR}/skeleton_1.0.0.tar.gz`
- `${RENV_PATHS_CELLAR}/skeleton/skeleton_1.0.0.tar.gz`
- `<project>/renv/cellar/skeleton_1.0.0.tar.gz`; or
- `<project>/renv/cellar/skeleton/skeleton_1.0.0.tar.gz`

Once this is done, renv will consult these directories during future attempts
to restore your packages.

You can install a package from the cellar like any other package, i.e. `renv::install("skeleton")`.

During restore, if a compatible package is located within the cellar, that copy
of the package will be preferred even if that package might otherwise be
accessible from its associated remote source. For example, if `skeleton 1.0.0`
was also available on CRAN, `renv::restore()` would still use the tarball
available in the cellar rather than the version available from CRAN.

If you want to see what paths renv is using for the cellar, you can use:

```{r eval=FALSE}
renv:::renv_paths_cellar()
```

See `?paths` for more details.

### Explicit sources

You can also provide explicit source paths in the lockfile if desired. This is
most useful if you are building an renv lockfile "by hand", or need to tweak
an existing lockfile to point at a separate package for installation. For
example, you could have a package record in `renv.lock` of the form:

```
{
  "Package": "skeleton",
  "Version": "1.0.1",
  "Source": "/mnt/r/pkg/skeleton_1.0.1.tar.gz"
}
```

Packages should have the following extensions, depending on whether the archive
contains a binary copy of the package or the package sources:

| **Platform** | **Binary** | **Sources** |
| ------------ | ---------- | ----------- |
| Windows      | `.zip`     | `.tar.gz`   |
| macOS        | `.tgz`     | `.tar.gz`   |
| Linux        | `.tar.gz`  | `.tar.gz`   |

Note that on Linux, both binaries and sources should have the `.tar.gz`
extension, but R and renv will handle this as appropriate during installation.


## ABI compatibility

ABI compatibility issues can arise if different packages were built against
different versions of a shared dependency. For example, one package may have
been built against Rcpp 1.0.6, and another package might have been built against
Rcpp 1.0.7. However, because only one version of the Rcpp package can be loaded
at a time within an R session, mixing of these two packages might cause issues
either on load or at runtime depending on the version of Rcpp available.

It's worth emphasizing that this is not Rcpp's fault; a package built against
Rcpp 1.0.7 would reasonably expect newer APIs made available by that version of
the package would be available at runtime, and that contract would be violated
if an older version of Rcpp were installed in the project library. The challenge
for renv is that this build-time dependency is not clearly communicated to
renv; in general, it is not possible to know what packages (and their
versions) a particular package was built against.


## Build-time dependencies

R packages might occasionally (and unintentionally) take a build-time
dependency on another R package -- for example, a package with the code:

```
`%>%` <- magrittr::`%>%`
```

would take the version of `%>%` that was available from the version of magrittr
that was available at _build_ time, not the one available at _run_ time. This
could be problematic if, for example, an update to the magrittr package
changed in a way that made old definitions of `%>%` incompatible with newer
internal functions.

In general, it is a mistake for packages to take a build-time dependency on
exported objects from another package; rather, such objects should be imported
at runtime (using e.g. `importFrom()` in the package `NAMESPACE` file).
