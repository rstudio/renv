# Update package records in a lockfile

Use `record()` to record a new entry within an existing renv lockfile.

## Usage

``` r
record(records, lockfile = NULL, project = NULL, enrich = TRUE)
```

## Arguments

- records:

  A list of named records, mapping package names to a definition of
  their source. See **Records** for more details.

- lockfile:

  Path to a lockfile. When `NULL` (the default), the `renv.lock` located
  in the root of the current project will be used.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- enrich:

  Should resolved records be enriched with the DESCRIPTION fields that
  [`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  would write (`Depends`, `Imports`, `Suggests`, `LinkingTo`, `License`,
  etc.)? Defaults to `TRUE`. When `TRUE`, `record()` consults the active
  package repositories (and crandb, when enabled, for CRAN packages) to
  fill in the additional fields, erroring if the remote source is
  unreachable. The `Hash` field is not computed for enriched records.
  Additional DESCRIPTION-only fields (`Title`, `Description`, `Author`,
  `Maintainer`) are only included when the source can supply them –
  typically when crandb is reachable for CRAN packages, or when the
  source is a Git host (GitHub, GitLab, Bitbucket). Set to `FALSE` to
  keep the minimal record (`Package`, `Version`, `Source`, `Repository`)
  produced by remote resolution.

## Details

This function can be useful when you need to change one or more of the
package records within an renv lockfile – for example, because a
recorded package cannot be restored in a particular environment, and you
know of a suitable alternative.

## Records

Records can be provided either using the **remotes** short-hand syntax,
or by using an R list of entries to record within the lockfile. See
[`?lockfiles`](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
for more information on the structure of a package record.

## Examples

``` r

if (FALSE) { # \dontrun{

# use digest 0.6.22 from package repositories -- different ways
# of specifying the remote. use whichever is most natural
renv::record("digest@0.6.22")
renv::record(list(digest = "0.6.22"))
renv::record(list(digest = "digest@0.6.22"))

# alternatively, provide a full record as a list
digest_record <- list(
  Package = "digest",
  Version = "0.6.22",
  Source  = "Repository",
  Repository = "CRAN"
)

renv::record(list(digest = digest_record))

} # }
```
