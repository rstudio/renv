# Resolve a Remote

Given a remote specification, resolve it into an renv package record
that can be used for download and installation (e.g. with
[install](https://rstudio.github.io/renv/dev/reference/install.md)).

## Usage

``` r
remote(spec)
```

## Arguments

- spec:

  A remote specification. This should be a string, conforming to the
  Remotes specification as defined in
  <https://remotes.r-lib.org/articles/dependencies.html>.
