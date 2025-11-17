# Isolate a project

Copy packages from the renv cache directly into the project library, so
that the project can continue to function independently of the renv
cache.

## Usage

``` r
isolate(project = NULL)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Details

After calling `isolate()`, renv will still be able to use the cache on
future
[`install()`](https://rstudio.github.io/renv/dev/reference/install.md)s
and
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)s.
If you'd prefer that renv copy packages from the cache, rather than use
symlinks, you can set the renv configuration option:

    options(renv.config.cache.symlinks = FALSE)

to force renv to copy packages from the cache, as opposed to symlinking
them. If you'd like to disable the cache altogether for a project, you
can use:

    settings$use.cache(FALSE)

to explicitly disable the cache for the project.

## Examples

``` r
if (FALSE) { # \dontrun{

# isolate a project
renv::isolate()

} # }
```
