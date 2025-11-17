# Refresh the local cache of available packages

Query the active R package repositories for available packages, and
update the in-memory cache of those packages.

## Usage

``` r
refresh()
```

## Value

A list of package databases, invisibly – one for each repository
currently active in the R session. Note that this function is normally
called for its side effects.

## Details

Note that R also maintains its own on-disk cache of available packages,
which is used by
[`available.packages()`](https://rdrr.io/r/utils/available.packages.html).
Calling `refresh()` will force an update of both types of caches. renv
prefers using an in-memory cache as on occasion the temporary directory
can be slow to access (e.g. when it is a mounted network filesystem).

## Examples

``` r
if (FALSE) { # \dontrun{

# check available packages
db <- available.packages()

# wait some time (suppose packages are uploaded / changed in this time)
Sys.sleep(5)

# refresh the local available packages database
# (the old locally cached db will be removed)
db <- renv::refresh()

} # }
```
