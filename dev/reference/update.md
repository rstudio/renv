# Update packages

Update packages which are currently out-of-date. Currently supports
CRAN, Bioconductor, other CRAN-like repositories, GitHub, GitLab, Git,
and BitBucket.

Updates will only be checked from the same source – for example, if a
package was installed from GitHub, but a newer version is available on
CRAN, that updated version will not be seen.

## Usage

``` r
update(
  packages = NULL,
  ...,
  exclude = NULL,
  library = NULL,
  type = NULL,
  rebuild = FALSE,
  check = FALSE,
  prompt = interactive(),
  lock = FALSE,
  all = FALSE,
  project = NULL
)
```

## Arguments

- packages:

  A character vector of R packages to update. When `NULL` (the default),
  all packages (apart from any listed in the `ignored.packages` project
  setting) will be updated.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- exclude:

  A set of packages to explicitly exclude from updating. Use
  `renv::update(exclude = <...>)` to update all packages except for a
  specific set of excluded packages.

- library:

  The R library to be used. When `NULL`, the active project library will
  be used instead.

- type:

  The type of package to install ("source" or "binary"). Defaults to the
  value of `getOption("pkgType")`.

- rebuild:

  Force packages to be rebuilt, thereby bypassing any installed versions
  of the package available in the cache? This can either be a boolean
  (indicating that all installed packages should be rebuilt), or a
  vector of package names indicating which packages should be rebuilt.

- check:

  Boolean; check for package updates without actually installing
  available updates? This is useful when you'd like to determine what
  updates are available, without actually installing those updates.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

- lock:

  Boolean; update the `renv.lock` lockfile after the successful
  installation of the requested packages?

- all:

  Boolean; should `renv` check all library paths for out-of-date
  packages? When `FALSE` (the default), only the project library will be
  checked for out-of-date packages.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A named list of package records which were installed by renv.

## Examples

``` r
if (FALSE) { # \dontrun{

# update the 'dplyr' package
renv::update("dplyr")

} # }
```
