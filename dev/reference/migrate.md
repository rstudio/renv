# Migrate a project from packrat to renv

Migrate a project's infrastructure from packrat to renv.

## Usage

``` r
migrate(
  project = NULL,
  packrat = c("lockfile", "sources", "library", "options", "cache")
)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- packrat:

  Components of the Packrat project to migrate. See the default argument
  list for components of the Packrat project that can be migrated.
  Select a subset of those components for migration as appropriate.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Migration

When migrating Packrat projects to renv, the set of components migrated
can be customized using the `packrat` argument. The set of components
that can be migrated are as follows:

|            |                                                                                                                                                                                                         |
|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Name**   | **Description**                                                                                                                                                                                         |
| `lockfile` | Migrate the Packrat lockfile (`packrat/packrat.lock`) to the renv lockfile (`renv.lock`).                                                                                                               |
| `sources`  | Migrate package sources from the `packrat/src` folder to the renv sources folder. Currently, only CRAN packages are migrated to renv – packages retrieved from other sources (e.g. GitHub) are ignored. |
| `library`  | Migrate installed packages from the Packrat library to the renv project library.                                                                                                                        |
| `options`  | Migrate compatible Packrat options to the renv project.                                                                                                                                                 |
| `cache`    | Migrate packages from the Packrat cache to the renv cache.                                                                                                                                              |

## Examples

``` r
if (FALSE) { # \dontrun{

# migrate Packrat project infrastructure to renv
renv::migrate()

} # }
```
