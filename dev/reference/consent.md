# Consent to usage of renv

Provide consent to renv, allowing it to write and update certain files
on your filesystem.

## Usage

``` r
consent(provided = FALSE)
```

## Arguments

- provided:

  The default provided response. If you need to provide consent from a
  non-interactive R session, you can invoke
  `renv::consent(provided = TRUE)` explicitly.

## Value

`TRUE` if consent is provided, or an R error otherwise.

## Details

As part of its normal operation, renv will write and update some files
in your project directory, as well as an application-specific cache
directory. These paths are documented within
[paths](https://rstudio.github.io/renv/dev/reference/paths.md).

In accordance with the [CRAN Repository
Policy](https://cran.r-project.org/web/packages/policies.html), renv
must first obtain consent from you, the user, before these actions can
be taken. Please call `renv::consent()` first to provide this consent.

You can also set the R option:

    options(renv.consent = TRUE)

to implicitly provide consent for e.g. non-interactive R sessions.
