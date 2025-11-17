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

Any packages which are re-hashed will retain links to the location of
the newly-hashed package, ensuring that prior installations of renv can
still function as expected.
