# Retrieve the active project

Retrieve the path to the active project (if any).

## Usage

``` r
project(default = NULL)
```

## Arguments

- default:

  The value to return when no project is currently active. Defaults to
  `NULL`.

## Value

The active project directory, as a length-one character vector.

## Examples

``` r
if (FALSE) { # \dontrun{

# get the currently-active renv project
renv::project()

} # }
```
