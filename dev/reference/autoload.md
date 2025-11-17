# Auto-load the active project

Automatically load the renv project associated with a particular
directory. renv will search parent directories for the renv project
root; if found, that project will be loaded via
[`load()`](https://rstudio.github.io/renv/dev/reference/load.md).

## Usage

``` r
autoload()
```

## Details

To enable the renv auto-loader, you can place:

    renv::autoload()

into your site-wide or user `.Rprofile` to ensure that renv projects are
automatically loaded for any newly-launched R sessions, even if those R
sessions are launched within the sub-directory of an renv project.

If you'd like to launch R within the sub-directory of an renv project
without auto-loading renv, you can set the environment variable:

    RENV_AUTOLOAD_ENABLED = FALSE

before starting R.

Note that `renv::autoload()` is only compatible with projects using
`renv 0.15.3` or newer, as it relies on features within the
`renv/activate.R` script that are only generated with newer versions of
renv.
