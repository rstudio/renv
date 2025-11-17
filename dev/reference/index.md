# Package index

## Core Workflow

These functions are the bread and butter of renv usage, and make up the
bulk of day-to-day renv usage.

- [`init()`](https://rstudio.github.io/renv/dev/reference/init.md) : Use
  renv in a project
- [`status()`](https://rstudio.github.io/renv/dev/reference/status.md) :
  Report inconsistencies between lockfile, library, and dependencies
- [`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
  : Record current state of the project library in the lockfile
- [`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
  : Restore project library from a lockfile

## Package Management

- [`install()`](https://rstudio.github.io/renv/dev/reference/install.md)
  : Install packages
- [`update()`](https://rstudio.github.io/renv/dev/reference/update.md) :
  Update packages
- [`remove()`](https://rstudio.github.io/renv/dev/reference/remove.md) :
  Remove packages

## Configuration

- [`config`](https://rstudio.github.io/renv/dev/reference/config.md) :
  User-level settings
- [`settings`](https://rstudio.github.io/renv/dev/reference/settings.md)
  : Project settings

## Project Management

- [`activate()`](https://rstudio.github.io/renv/dev/reference/activate.md)
  [`deactivate()`](https://rstudio.github.io/renv/dev/reference/activate.md)
  : Activate or deactivate a project
- [`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
  : Find R package dependencies in a project
- [`load()`](https://rstudio.github.io/renv/dev/reference/load.md) :
  Load a project
- [`migrate()`](https://rstudio.github.io/renv/dev/reference/migrate.md)
  : Migrate a project from packrat to renv
- [`paths`](https://rstudio.github.io/renv/dev/reference/paths.md) :
  Path for storing global state
- [`project()`](https://rstudio.github.io/renv/dev/reference/project.md)
  : Retrieve the active project
- [`upgrade()`](https://rstudio.github.io/renv/dev/reference/upgrade.md)
  : Upgrade renv

## Library Management

- [`rebuild()`](https://rstudio.github.io/renv/dev/reference/rebuild.md)
  : Rebuild the packages in your project library
- [`repair()`](https://rstudio.github.io/renv/dev/reference/repair.md) :
  Repair a project
- [`clean()`](https://rstudio.github.io/renv/dev/reference/clean.md) :
  Clean a project

## Lockfile Management

- [`lockfile_create()`](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
  [`lockfile_read()`](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
  [`lockfile_write()`](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
  [`lockfile_modify()`](https://rstudio.github.io/renv/dev/reference/lockfiles.md)
  : Lockfiles
- [`lockfile_validate()`](https://rstudio.github.io/renv/dev/reference/lockfile_validate.md)
  : Validate an renv lockfile against a schema
- [`record()`](https://rstudio.github.io/renv/dev/reference/record.md) :
  Update package records in a lockfile
- [`remote()`](https://rstudio.github.io/renv/dev/reference/remote.md) :
  Resolve a Remote
- [`modify()`](https://rstudio.github.io/renv/dev/reference/modify.md) :
  Modify a Lockfile
- [`history()`](https://rstudio.github.io/renv/dev/reference/history.md)
  [`revert()`](https://rstudio.github.io/renv/dev/reference/history.md)
  : View and revert to a historical lockfile
- [`scaffold()`](https://rstudio.github.io/renv/dev/reference/scaffold.md)
  : Generate project infrastructure

## Package Cache

- [`isolate()`](https://rstudio.github.io/renv/dev/reference/isolate.md)
  : Isolate a project
- [`rehash()`](https://rstudio.github.io/renv/dev/reference/rehash.md) :
  Re-hash packages in the renv cache
- [`purge()`](https://rstudio.github.io/renv/dev/reference/purge.md) :
  Purge packages from the cache

## Python Integration

- [`use_python()`](https://rstudio.github.io/renv/dev/reference/use_python.md)
  : Use python

## Miscellaneous

- [`autoload()`](https://rstudio.github.io/renv/dev/reference/autoload.md)
  : Auto-load the active project

- [`checkout()`](https://rstudio.github.io/renv/dev/reference/checkout.md)
  : Checkout a repository

- [`consent()`](https://rstudio.github.io/renv/dev/reference/consent.md)
  : Consent to usage of renv

- [`diagnostics()`](https://rstudio.github.io/renv/dev/reference/diagnostics.md)
  : Print a diagnostics report

- [`refresh()`](https://rstudio.github.io/renv/dev/reference/refresh.md)
  : Refresh the local cache of available packages

- [`run()`](https://rstudio.github.io/renv/dev/reference/run.md) : Run a
  script

- [`sysreqs()`](https://rstudio.github.io/renv/dev/reference/sysreqs.md)
  : R System Requirements

- [`embed()`](https://rstudio.github.io/renv/dev/reference/embed.md)
  [`use()`](https://rstudio.github.io/renv/dev/reference/embed.md) :

  Capture and re-use dependencies within a `.R`, `.Rmd` or `.qmd`
