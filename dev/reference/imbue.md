# Imbue an renv Installation

Imbue an renv installation into a project, thereby making the requested
version of renv available within.

## Usage

``` r
imbue(project = NULL, version = NULL, quiet = FALSE)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- version:

  The version of renv to install. If `NULL`, the version of renv
  currently installed will be used. The requested version of renv will
  be retrieved from the renv public GitHub repository, at
  <https://github.com/rstudio/renv>.

- quiet:

  Boolean; avoid printing output during install of renv?

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Details

Normally, this function does not need to be called directly by the user;
it will be invoked as required by
[`init()`](https://rstudio.github.io/renv/dev/reference/init.md) and
[`activate()`](https://rstudio.github.io/renv/dev/reference/activate.md).
