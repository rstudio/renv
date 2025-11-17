# Checkout a repository

`renv::checkout()` can be used to retrieve the latest-available packages
from a (set of) package repositories.

## Usage

``` r
checkout(
  repos = NULL,
  ...,
  packages = NULL,
  date = NULL,
  clean = FALSE,
  actions = "restore",
  restart = NULL,
  project = NULL
)
```

## Arguments

- repos:

  The R package repositories to use.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- packages:

  The packages to be installed. When `NULL` (the default), all packages
  currently used in the project will be installed, as determined by
  [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md).
  The recursive dependencies of these packages will be included as well.

- date:

  The snapshot date to use. When set, the associated snapshot as
  available from the Posit's public [Package
  Manager](https://packagemanager.rstudio.com/) instance will be used.
  Ignored if `repos` is non-`NULL`.

- clean:

  Boolean; remove packages not recorded in the lockfile from the target
  library? Use `clean = TRUE` if you'd like the library state to exactly
  reflect the lockfile contents after
  [`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md).

- actions:

  The action(s) to perform with the requested repositories. This can
  either be `"snapshot"`, in which `renv` will generate a lockfile based
  on the latest versions of the packages available from `repos`, or
  `"restore"` if you'd like to install those packages. You can use
  `c("snapshot", "restore")` if you'd like to generate a lockfile and
  install those packages in a single call.

- restart:

  Should the R session be restarted after the new packages have been
  checked out? When `NULL` (the default), the session is restarted if
  the `"restore"` action was taken.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Details

`renv::checkout()` is most useful with services like the Posit's
[Package Manager](https://packagemanager.rstudio.com/), as it can be
used to switch between different repository snapshots within an renv
project. In this way, you can upgrade (or downgrade) all of the packages
used in a particular renv project to the package versions provided by a
particular snapshot.

Note that calling `renv::checkout()` will also install the version of
`renv` available as of the requested snapshot date, which might be older
or lack features available in the currently-installed version of `renv`.
In addition, the project's `renv/activate.R` script will be re-generated
after checkout. If this is undesired, you can re-install a newer version
of `renv` after checkout from your regular R package repository.

## Caveats

If your library contains packages installed from other remote sources
(e.g. GitHub), but a version of a package of the same name is provided
by the repositories being checked out, then please be aware that the
package will be replaced with the version provided by the requested
repositories. This could be a concern if your project uses R packages
from GitHub whose name matches that of an existing CRAN package, but is
otherwise unrelated to the package on CRAN.

## Examples

``` r
if (FALSE) { # \dontrun{

# check out packages from PPM using the date '2023-01-02'
renv::checkout(date = "2023-01-02")

# alternatively, supply the full repository path
renv::checkout(repos = c(PPM = "https://packagemanager.rstudio.com/cran/2023-01-02"))

# only check out some subset of packages (and their recursive dependencies)
renv::checkout(packages = "dplyr", date = "2023-01-02")

# generate a lockfile based on a snapshot date
renv::checkout(date = "2023-01-02", actions = "snapshot")

} # }
```
