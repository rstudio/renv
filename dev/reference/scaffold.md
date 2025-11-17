# Generate project infrastructure

Create the renv project infrastructure. This will:

- Create a project library, `renv/library`.

- Install renv into the project library.

- Update the project `.Rprofile` to call `source("renv/activate.R")` so
  that renv is automatically loaded for new R sessions launched in this
  project.

- Create `renv/.gitignore`, which tells git to ignore the project
  library.

- Create `.Rbuildignore`, if the project is also a package. This tells
  `R CMD build` to ignore the renv infrastructure,

- Write a (bare)
  [lockfile](https://rstudio.github.io/renv/dev/reference/lockfile-api.md),
  `renv.lock`.

## Usage

``` r
scaffold(
  project = NULL,
  version = NULL,
  repos = getOption("repos"),
  settings = NULL
)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- version:

  The version of renv to associate with this project. By default, the
  version of renv currently installed is used.

- repos:

  The R repositories to associate with this project.

- settings:

  A list of renv settings, to be applied to the project after creation.
  These should map setting names to the desired values. See
  [settings](https://rstudio.github.io/renv/dev/reference/settings.md)
  for more details.

## Examples

``` r
if (FALSE) { # \dontrun{
# create scaffolding with 'devtools' ignored
renv::scaffold(settings = list(ignored.packages = "devtools"))
} # }
```
