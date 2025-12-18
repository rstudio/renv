# Programmatically Create and Modify a Lockfile

**NOTE: `lockfile()` is now internal, please use the
[lockfiles](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
API.**

## Usage

``` r
lockfile(file = NULL, project = NULL)
```

## Arguments

- file:

  The path to an existing lockfile. When no lockfile is provided, a new
  one will be created based on the current project context. If you want
  to create a blank lockfile, use `file = NA` instead.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Details

This function provides an API for creating and modifying `renv`
lockfiles. This can be useful when you'd like to programmatically
generate or modify a lockfile – for example, because you want to update
or change a package record in an existing lockfile.

## See also

[`lockfiles`](https://rstudio.github.io/renv/dev/reference/lockfiles.md),
for a description of the structure of an `renv` lockfile.

## Examples

``` r
if (FALSE) { # \dontrun{

lock <- lockfile("renv.lock")

# set the repositories for a lockfile
lock$repos(CRAN = "https://cran.r-project.org")

# depend on digest 0.6.22
lock$add(digest = "digest@0.6.22")

# write to file
lock$write("renv.lock")

} # }
```
