# Purge packages from the cache

Purge packages from the cache. This can be useful if a package which had
previously been installed in the cache has become corrupted or unusable,
and needs to be reinstalled.

## Usage

``` r
purge(package, ..., version = NULL, hash = NULL, prompt = interactive())
```

## Arguments

- package:

  A single package to be removed from the cache.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- version:

  The package version to be removed. When `NULL`, all versions of the
  requested package will be removed.

- hash:

  The specific hashes to be removed. When `NULL`, all hashes associated
  with a particular package's version will be removed.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

## Value

The set of packages removed from the renv global cache, as a character
vector of file paths.

## Details

`purge()` is an inherently destructive option. It removes packages from
the cache, and so any project which had symlinked that package into its
own project library would find that package now unavailable. These
projects would hence need to reinstall any purged packages. Take heed of
this in case you're looking to purge the cache of a package which is
difficult to install, or if the original sources for that package are no
longer available!

## Examples

``` r
if (FALSE) { # \dontrun{

# remove all versions of 'digest' from the cache
renv::purge("digest")

# remove only a particular version of 'digest' from the cache
renv::purge("digest", version = "0.6.19")

} # }
```
