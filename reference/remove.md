# Remove packages

Remove (uninstall) R packages.

## Usage

``` r
remove(packages, ..., library = NULL, project = NULL)
```

## Arguments

- packages:

  A character vector of R packages to remove.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- library:

  The library from which packages should be removed. When `NULL`, the
  active library (that is, the first entry reported in
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)) is used
  instead.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

A vector of package records, describing the packages (if any) which were
successfully removed.

## Examples

``` r
if (FALSE) { # \dontrun{

# disable automatic snapshots
auto.snapshot <- getOption("renv.config.auto.snapshot")
options(renv.config.auto.snapshot = FALSE)

# initialize a new project (with an empty R library)
renv::init(bare = TRUE)

# install digest 0.6.19
renv::install("digest@0.6.19")

# save library state to lockfile
renv::snapshot()

# remove digest from library
renv::remove("digest")

# check library status
renv::status()

# restore lockfile, thereby reinstalling digest 0.6.19
renv::restore()

# restore automatic snapshots
options(renv.config.auto.snapshot = auto.snapshot)

} # }
```
