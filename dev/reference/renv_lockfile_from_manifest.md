# Generate `renv.lock` from an RStudio Connect `manifest.json`

Use `renv_lockfile_from_manifest()` to convert a `manifest.json` file
from an RStudio Connect content bundle into an `renv.lock` lockfile.

This function can be useful when you need to recreate the package
environment of a piece of content that is deployed to RStudio Connect.
The content bundle contains a `manifest.json` file that is used to
recreate the package environment. This function will let you convert
that manifest file to an `renv.lock` file. Run
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
after you've converted the file to restore the package environment.

## Usage

``` r
renv_lockfile_from_manifest(
  manifest = "manifest.json",
  lockfile = NA,
  project = NULL
)
```

## Arguments

- manifest:

  The path to a `manifest.json` file.

- lockfile:

  The path to the lockfile to be generated and / or updated. When `NA`
  (the default), the generated lockfile is returned as an R object;
  otherwise, the lockfile will be written to the path specified by
  `lockfile`.

## Value

An renv lockfile.
