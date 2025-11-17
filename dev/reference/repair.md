# Repair a project

Use `repair()` to recover from some common issues that can occur with a
project. Currently, two operations are performed:

## Usage

``` r
repair(library = NULL, lockfile = NULL, project = NULL)
```

## Arguments

- library:

  The R library to be used. When `NULL`, the active project library will
  be used instead.

- lockfile:

  The path to a lockfile (if any). When available, renv will use the
  lockfile when attempting to infer the remote associated with the
  inaccessible version of each missing package. When `NULL` (the
  default), the project lockfile will be used.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Details

1.  Packages with broken symlinks into the cache will be re-installed.

2.  Packages that were installed from sources, but appear to be from an
    remote source (e.g. GitHub), will have their `DESCRIPTION` files
    updated to record that remote source explicitly.
