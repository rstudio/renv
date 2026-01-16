# Request Vulnerability Information for a Package

This function acts as an interface to Posit Package Manager's
vulnerability API, making it possible to ascertain if the provided
packages have any known vulnerabilities.

## Usage

``` r
vulns(
  packages = NULL,
  lockfile = NULL,
  ...,
  repos = NULL,
  verbose = FALSE,
  project = NULL
)
```

## Arguments

- packages:

  A vector of package specifications, of the form `<package>==<version>`
  or `<package>@<version>`.

- lockfile:

  The path to an `renv` lockfile. When specified, `packages` is ignored,
  and vulnerabilities are queried based on the packages defined in the
  lockfile.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- repos:

  The Package Manager repository to be queried.

- verbose:

  Boolean; when `TRUE`, verbose information from the `curl` web request
  will be printed to the console.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

An R list of vulnerability information. Only packages which have known
vulnerabilities will be included in the resulting data object.

## Details

This function requires the curl package to be installed.
