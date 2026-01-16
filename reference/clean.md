# Clean a project

Clean up a project and its associated R libraries.

## Usage

``` r
clean(project = NULL, ..., actions = NULL, prompt = interactive())
```

## Arguments

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- actions:

  The set of clean actions to take. See the documentation in **Actions**
  for a list of available actions, and the default actions taken when no
  actions are supplied.

- prompt:

  Boolean; prompt the user before taking any action? For backwards
  compatibility, `confirm` is accepted as an alias for `prompt`.

## Value

The project directory, invisibly. Note that this function is normally
called for its side effects.

## Actions

The following clean actions are available:

- `package.locks`:

  During package installation, R will create package locks in the
  library path, typically named `00LOCK-<package>`. On occasion, if
  package installation fails or R is terminated while installing a
  package, these locks can be left behind and will inhibit future
  attempts to reinstall that package. Use this action to remove such
  left-over package locks.

- `library.tempdirs`:

  During package installation, R may create temporary directories with
  names of the form `file\w{12}`, and on occasion those files can be
  left behind even after they are no longer in use. Use this action to
  remove such left-over directories.

- `system.library`:

  In general, it is recommended that only packages distributed with R
  are installed into the default library (the library path referred to
  by `.Library`). Use this action to remove any user-installed packages
  that have been installed to the system library.

  Because this action is destructive, it is by default never run – it
  must be explicitly requested by the user.

- `unused.packages`:

  Remove packages that are installed in the project library, but no
  longer appear to be used in the project sources.

  Because this action is destructive, it is by default only run in
  interactive sessions when prompting is enabled.

## Examples

``` r
if (FALSE) { # \dontrun{

# clean the current project
renv::clean()

} # }
```
