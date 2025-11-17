# Modify a Lockfile

Modify a project's lockfile, either interactively or non-interactively.

## Usage

``` r
modify(project = NULL, changes = NULL)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- changes:

  A list of changes to be merged into the lockfile. When `NULL` (the
  default), the lockfile is instead opened for interactive editing.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Details

After edit, if the lockfile edited is associated with the active
project, any state-related changes (e.g. to R repositories) will be
updated in the current session.

## Examples

``` r
if (FALSE) { # \dontrun{

# modify an existing lockfile
if (interactive())
  renv::modify()

} # }
```
