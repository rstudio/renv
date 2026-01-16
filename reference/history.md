# View and revert to a historical lockfile

`history()` uses your version control system to show prior versions of
the lockfile and `revert()` allows you to restore one of them.

These functions are currently only implemented for projects that use
git.

## Usage

``` r
history(project = NULL)

revert(commit = "HEAD", ..., project = NULL)
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- commit:

  The commit associated with a prior version of the lockfile.

- ...:

  Optional arguments; currently unused.

## Value

`history()` returns a `data.frame` summarizing the commits in which
`renv.lock` has been changed. `revert()` is usually called for its
side-effect but also invisibly returns the `commit` used.

## Examples

``` r
if (FALSE) { # \dontrun{

# get history of previous versions of renv.lock in VCS
db <- renv::history()

# choose an older commit
commit <- db$commit[5]

# revert to that version of the lockfile
renv::revert(commit = commit)

} # }
```
