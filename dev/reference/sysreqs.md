# R System Requirements

Compute the system requirements (system libraries; operating system
packages) required by a set of R packages.

## Usage

``` r
sysreqs(
  packages = NULL,
  ...,
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

  A vector of R package names. When `NULL` (the default), the project's
  package dependencies as reported via
  [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
  are used.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- local:

  Boolean; should `renv` rely on locally-installed copies of packages
  when resolving system requirements? When `FALSE`, `renv` will use
  <https://crandb.r-pkg.org> to resolve the system requirements for
  these packages.

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

## Examples

``` r
if (FALSE) { # \dontrun{

# report the required system packages for this system
sysreqs()

# report the required system packages for a specific OS
sysreqs(platform = "ubuntu")

} # }
```
