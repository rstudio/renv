# Run a script

Run an R script, in the context of a project using renv. The script will
be run within an R sub-process.

## Usage

``` r
run(script, ..., job = NULL, name = NULL, args = NULL, project = NULL)
```

## Arguments

- script:

  The path to an R script.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- job:

  Run the requested script as an RStudio job? Requires a recent version
  of both RStudio and the rstudioapi packages. When `NULL`, the script
  will be run as a job if possible, and as a regular R process launched
  by [`system2()`](https://rdrr.io/r/base/system2.html) if not.

- name:

  The name to associate with the job, for scripts run as a job.

- args:

  description A character vector of command line arguments to be passed
  to the launched job. These parameters can be accessed via
  `commandArgs(trailingOnly = FALSE)`.

- project:

  The path to the renv project. This project will be loaded before the
  requested script is executed. When `NULL` (the default), renv will
  automatically determine the project root for the associated script if
  possible.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.
