# R System Requirements

Compute the system requirements (system libraries; operating system
packages) required by a set of R packages.

## Usage

``` r
sysreqs(
  packages = NULL,
  ...,
  source = NULL,
  recursive = TRUE,
  local = FALSE,
  check = NULL,
  report = TRUE,
  distro = NULL,
  collapse = FALSE,
  project = NULL
)
```

## Arguments

- packages:

  A vector of R package names. When `NULL` (the default), the packages
  recorded in the project lockfile are used, together with the project's
  package dependencies as reported via
  [`dependencies()`](https://rstudio.github.io/renv/reference/dependencies.md).

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- source:

  The sources to consult when resolving package records for system
  requirement lookup. For each package, the sources are tried in order,
  and the first source able to provide a record for that package is
  used:

  - `"lockfile"`: use the record in the project lockfile,

  - `"library"`: use the `DESCRIPTION` of the installed package,

  - `"crandb"`: query <https://crandb.r-pkg.org> for the package.

  The default consults all three, in the order listed above. Note that
  lockfiles produced by older versions of `renv` may not include the
  `SystemRequirements` field in their records; such records are used
  only to infer the package version. When the package version is known,
  an installed copy of the package is only used if its version matches,
  and crandb is queried for that specific version. Otherwise, crandb is
  queried for the latest version available from the active package
  repositories. crandb only has records of CRAN releases; when it has no
  record of the requested version, the latest CRAN release is used as a
  last resort.

- recursive:

  Boolean; should the system requirements of the recursive dependencies
  of `packages` be included as well? Only the dependencies required to
  install a package (`Depends`, `Imports`, and `LinkingTo`) are
  considered. Note that the dependencies of a package are determined
  from the version of that package being used; see **Package versions**
  for more details.

- local:

  Boolean; superseded by `source`. `local = TRUE` is equivalent to
  `source = "library"`; that is, only locally-installed copies of
  packages are used when resolving system requirements.

- check:

  Boolean; should `renv` also check whether the requires system packages
  appear to be installed on the current system? Ignored when `distro` is
  supplied.

- report:

  Boolean; should `renv` also report the commands which could be used to
  install all of the requisite package dependencies?

- distro:

  The name of the Linux distribution for which system requirements
  should be checked – typical values are "ubuntu", "debian", and
  "redhat". These should match the distribution names used by the R
  system requirements database. A version suffix can be included; for
  example, "ubuntu:24.04".

- collapse:

  Boolean; when reporting which packages need to be installed, should
  the report be collapsed into a single installation command? When
  `FALSE` (the default), a separate installation line is printed for
  each required system package.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Details

This function relies on the database of package system requirements
maintained by Posit at
<https://github.com/rstudio/r-system-requirements>, as well as the
"meta-CRAN" service at <https://crandb.r-pkg.org>. This service
primarily exists to map the (free-form) `SystemRequirements` field used
by R packages to the system packages made available by a particular
operating system.

As an example, the `curl` R package depends on the `libcurl` system
library, and declares this with a `SystemRequirements` field of the
form:

- libcurl (\>= 7.62): libcurl-devel (rpm) or libcurl4-openssl-dev (deb)

This dependency can be satisfied with the following command line
invocations on different systems:

- Debian: `sudo apt install libcurl4-openssl-dev`

- Redhat: `sudo dnf install libcurl-devel`

and so `sysreqs("curl")` would help provide the name of the package
whose installation would satisfy the `libcurl` dependency.

## Package versions

System requirements belong to a specific *version* of a package, not to
the package in general. Each version of a package declares its own
`SystemRequirements`, and its own R package dependencies, and both can
change from one release to the next. For example, `ragg 1.3.0` declares:

- freetype2, libpng, libtiff, libjpeg

whereas `ragg 1.5.2` declares:

- freetype2, libpng, libtiff, libjpeg, libwebp, libwebpmux

This means that the system packages reported by `sysreqs()` are only
accurate for the package versions that were used to compute them. The
same call can give different results in different projects, or in the
same project after its packages have been updated.

For each package, including those found as recursive dependencies,
`sysreqs()` uses the first of the following versions which is available:

1.  The version recorded in the project lockfile,

2.  The version installed in the active library paths,

3.  The latest version available from the active package repositories.

These correspond to the `"lockfile"`, `"library"`, and `"crandb"`
sources; see the `source` argument for more details. Each package is
resolved independently, so the versions used need not come from the same
source.

If you are using `sysreqs()` to prepare a system for
[`restore()`](https://rstudio.github.io/renv/reference/restore.md), make
sure the lockfile is up-to-date, so that `sysreqs()` reports on the same
package versions that
[`restore()`](https://rstudio.github.io/renv/reference/restore.md) will
later install.

## Examples

``` r

if (FALSE) { # \dontrun{

# report the required system packages for this system
sysreqs()

# report the required system packages for a specific OS
sysreqs(distro = "ubuntu:24.04")

# report the system packages required by a package, using
# the latest version available from the package repositories
sysreqs("ragg", source = "crandb", distro = "ubuntu:24.04")

} # }
```
