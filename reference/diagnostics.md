# Print a diagnostics report

Print a diagnostics report, summarizing the state of a project using
renv. This report can occasionally be useful when diagnosing issues with
renv.

## Usage

``` r
diagnostics(project = NULL)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

This function is normally called for its side effects.
