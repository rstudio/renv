# Rebuild the packages in your project library

Rebuild and reinstall packages in your library. This can be useful as a
diagnostic tool – for example, if you find that one or more of your
packages fail to load, and you want to ensure that you are starting from
a clean slate.

## Usage

``` r
rebuild(
  packages = NULL,
  recursive = TRUE,
  ...,
  type = NULL,
  prompt = interactive(),
  library = NULL,
  project = NULL
)
```

## Arguments

- packages:

  The package(s) to be rebuilt. When `NULL`, all packages in the library
  will be reinstalled.

- recursive:

  Boolean; should dependencies of packages be rebuilt recursively?
  Defaults to `TRUE`.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- type:

  The type of package to install ("source" or "binary"). Defaults to the
  value of `getOption("pkgType")`.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

- library:

  The R library to be used. When `NULL`, the active project library will
  be used instead.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A named list of package records which were installed by renv.

## Examples

``` r
if (FALSE) { # \dontrun{

# rebuild the 'dplyr' package + all of its dependencies
renv::rebuild("dplyr", recursive = TRUE)

# rebuild only 'dplyr'
renv::rebuild("dplyr", recursive = FALSE)

} # }
```
