# Retrieve packages

Retrieve (download) one or more packages from external sources. Using
`renv::retrieve()` can be useful in CI / CD workflows, where you might
want to download all packages listed in a lockfile before later invoking
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md).
Packages will be downloaded to an internal path within `renv`'s local
state directories – see
[paths](https://rstudio.github.io/renv/dev/reference/paths.md) for more
details.

## Usage

``` r
retrieve(packages = NULL, ..., lockfile = NULL, destdir = NULL, project = NULL)
```

## Arguments

- packages:

  Either `NULL` (the default) to install all packages required by the
  project, or a character vector of packages to install. renv supports a
  subset of the remotes syntax used for package installation, e.g:

  - `pkg`: install latest version of `pkg` from CRAN.

  - `pkg@version`: install specified version of `pkg` from CRAN.

  - `username/repo`: install package from GitHub

  - `bioc::pkg`: install `pkg` from Bioconductor.

  See <https://remotes.r-lib.org/articles/dependencies.html> and the
  examples below for more details.

  renv deviates from the remotes spec in one important way:
  subdirectories are separated from the main repository specification
  with a `:`, not `/`. So to install from the `subdir` subdirectory of
  GitHub package `username/repo` you'd use `"username/repo:subdir`.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- lockfile:

  The path to an `renv` lockfile. When set, `renv` will retrieve the
  packages as defined within that lockfile. If `packages` is also
  non-`NULL`, then only those packages will be retrieved.

- destdir:

  The directory where packages should be downloaded. When `NULL` (the
  default), the default internal storage locations (normally used by
  e.g.
  [`install()`](https://rstudio.github.io/renv/dev/reference/install.md)
  or
  [`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md))
  will be used.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A named vector, mapping package names to the paths where those packages
were downloaded.

## Details

If `destdir` is `NULL` and the requested package is already available
within the `renv` cache, `renv` will return the path to that package
directory in the cache.

## Examples

``` r
if (FALSE) { # \dontrun{

# retrieve package + versions as defined in the lockfile
# normally used as a pre-flight step to renv::restore()
renv::retrieve()

# download one or more packages locally
renv::retrieve("rlang", destdir = ".")

} # }
```
