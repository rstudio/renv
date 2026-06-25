# Create a lockfile

Create an `renv` lockfile from a variety of sources.

## Usage

``` r
lockfile(from = NULL, to = NULL, project = NULL)
```

## Arguments

- from:

  The source from which the lockfile should be created. This can be:

  - `NULL` (the default), in which case the lockfile is created from the
    state of the active project's library;

  - The path to a Posit Connect `manifest.json` file, or a manifest that
    has already been read in as an R list, in which case the manifest is
    converted into a lockfile.

  The set of supported sources may be expanded in future releases of
  `renv`.

- to:

  The path to a lockfile to be written. When `NULL` (the default), the
  lockfile is returned as an R object and not written to disk;
  otherwise, the lockfile is written to `to` and returned invisibly.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

An `renv` lockfile, as an R object of class `renv_lockfile`. When `to`
is supplied, the lockfile is returned invisibly.

## Details

`lockfile()` provides a generic entry point for producing an `renv`
lockfile. The kind of lockfile produced depends on the value supplied as
`from`; for example, given the path to a Posit Connect `manifest.json`
file, `lockfile()` will convert that manifest into a lockfile.

By default the lockfile is returned as an R object. Supply `to` to also
write it to disk, after which
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
can be used to restore the associated project library.

## See also

[`lockfiles`](https://rstudio.github.io/renv/dev/reference/lockfiles.md),
for a description of the structure of an `renv` lockfile.

## Examples

``` r

if (FALSE) { # \dontrun{

# create a lockfile from a Posit Connect 'manifest.json' file
lock <- lockfile(from = "manifest.json")

# convert a 'manifest.json' file and write 'renv.lock' in a single call
lockfile(from = "manifest.json", to = "renv.lock")

} # }
```
