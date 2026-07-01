# Re-hash packages in the renv cache

Re-hash packages in the renv cache, ensuring that any previously-cached
packages are copied to a new cache location appropriate for this version
of renv. This can be useful if the cache scheme has changed in a new
version of renv, but you'd like to preserve your previously-cached
packages.

## Usage

``` r
rehash(prompt = interactive(), ...)
```

## Arguments

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

## Details

When re-hashing, packages are relocated to the cache location
appropriate for the active version of renv: they are copied when
migrating from a previous cache version, and moved when re-hashing
within the active cache. Because a package's previous cache location is
not retained, project libraries that still link to that old location
will be left with broken links after a re-hash. Use
[`repair()`](https://rstudio.github.io/renv/dev/reference/repair.md)
(or, equivalently,
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md))
within those projects to reinstall and re-link the affected packages.
