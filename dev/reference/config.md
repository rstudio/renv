# User-level settings

Configure different behaviors of renv.

## Usage

``` r
config
```

## Details

For a given configuration option:

1.  If an R option of the form `renv.config.<name>` is available, then
    that option's value will be used;

2.  If an environment variable of the form `RENV_CONFIG_<NAME>` is
    available, then that option's value will be used;

3.  Otherwise, the default for that particular configuration value is
    used.

Any periods (`.`)s in the option name are transformed into underscores
(`_`) in the environment variable name, and vice versa. For example, the
configuration option `auto.snapshot` could be configured as:

- `options(renv.config.auto.snapshot = <...>)`

- `Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = <...>)`

Note that if both the R option and the environment variable are defined,
the R option will be used instead. Environment variables can be more
useful when you want a particular configuration to be automatically
inherited by child processes; if that behavior is not desired, then the
R option may be preferred.

If you want to set and persist these options across multiple projects,
it is recommended that you set them in a a startup `.Renviron` file;
e.g. in your own `~/.Renviron`, or in the R installation's
`etc/Rprofile.site` file. See
[Startup](https://rdrr.io/r/base/Startup.html) for more details.

Configuration options can also be set within the project `.Rprofile`,
but be aware the options should be set before
`source("renv/activate.R")` is called.

## Configuration

The following renv configuration options are available:

### renv.config.activate.prompt

Automatically prompt the user to activate the current project, if it
does not appear to already be activated? This is mainly useful to help
ensure that calls to
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
and
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
use the project library. See
[`?renv::activate`](https://rstudio.github.io/renv/dev/reference/activate.md)
for more details. Defaults to `TRUE`.

### renv.config.autoloader.enabled

Enable the renv auto-loader? When `FALSE`, renv will not automatically
load a project containing an renv autoloader within its `.Rprofile`. In
addition, renv will not write out the project auto-loader in calls to
[`renv::init()`](https://rstudio.github.io/renv/dev/reference/init.md).
Defaults to `TRUE`.

### renv.config.auto.snapshot

Automatically snapshot changes to the project library when the project
dependencies change? Defaults to `FALSE`.

### renv.config.bioconductor.init

The default value to be used for the `bioconductor` parameter in calls
to
[`renv::init()`](https://rstudio.github.io/renv/dev/reference/init.md).
This can be used if you'd like to automatically enable or disable
Bioconductor by default for newly-initialized `renv` projects. See the
documentation in
[`?renv::init`](https://rstudio.github.io/renv/dev/reference/init.md)
for more details. Defaults to `NULL`.

### renv.config.bitbucket.host

The default Bitbucket host to be used during package retrieval. Defaults
to `"api.bitbucket.org/2.0"`.

### renv.config.copy.method

The method to use when attempting to copy directories. See **Copy
Methods** for more information. Defaults to `"auto"`.

### renv.config.connect.timeout

The amount of time to spend (in seconds) when attempting to download a
file. Only applicable when the `curl` downloader is used. Defaults to
`20L`.

### renv.config.connect.retry

The number of times to attempt re-downloading a file, when transient
download errors occur. Only applicable when the `curl` downloader is
used. Defaults to `3L`.

### renv.config.cache.enabled

Enable the global renv package cache? When active, renv will install
packages into a global cache, and link or copy packages from the cache
into your R library as appropriate. This can greatly save on disk space
and install time when R packages are shared across multiple projects in
the same environment. Defaults to `TRUE`.

### renv.config.cache.symlinks

Symlink packages from the global renv package cache into your project
library? When `TRUE`, renv will use symlinks (or, on Windows, junction
points) to reference packages installed in the cache. Set this to
`FALSE` if you'd prefer to copy packages from the cache into your
project library. Enabled by default, except on Windows where this
feature is only enabled if the project library and global package cache
are on the same volume. Defaults to `NULL`.

### renv.config.dependency.errors

Many renv APIs require the enumeration of your project's R package
dependencies. This option controls how errors that occur during this
enumeration are handled. By default, errors are reported but are
non-fatal. Set this to `"fatal"` to force errors to be fatal, and
`"ignored"` to ignore errors altogether. See
[`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
for more details. Defaults to `"reported"`.

### renv.config.dependencies.limit

By default, renv reports if it discovers more than this many files when
looking for dependencies, as that may indicate you are running
[`dependencies()`](https://rstudio.github.io/renv/dev/reference/dependencies.md)
in the wrong place. Set to `Inf` to disable this warning. Defaults to
`1000L`.

### renv.config.exported.functions

When [`library(renv)`](https://rstudio.github.io/renv/) is called,
should its exports be placed on the search path? Set this to `FALSE` to
avoid issues that can arise with, for example,
[`renv::load()`](https://rstudio.github.io/renv/dev/reference/load.md)
masking [`base::load()`](https://rdrr.io/r/base/load.html). In general,
we recommend referencing renv functions from its namespace explicitly;
e.g. prefer
[`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md)
over
[`snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md).
By default, all exported renv functions are attached and placed on the
search path, for backwards compatibility with existing scripts using
renv. Defaults to `"*"`.

### renv.config.external.libraries

A character vector of external libraries, to be used in tandem with your
projects. Be careful when using external libraries: it's possible that
things can break within a project if the version(s) of packages used in
your project library happen to be incompatible with packages in your
external libraries; for example, if your project required `xyz 1.0` but
`xyz 1.1` was present and loaded from an external library. Can also be
an R function that provides the paths to external libraries. Library
paths will be expanded via
[`.expand_R_libs_env_var()`](https://rdrr.io/r/base/base-internal.html)
as necessary. Defaults to `NULL`.

### renv.config.filebacked.cache

Enable the renv file-backed cache? When enabled, renv will cache the
contents of files that are read (e.g. DESCRIPTION files) in memory,
thereby avoiding re-reading the file contents from the filesystem if the
file has not changed. renv uses the file `mtime` to determine if the
file has changed; consider disabling this if `mtime` is unreliable on
your system. Defaults to `TRUE`.

### renv.config.github.host

The default GitHub host to be used during package retrieval. Defaults to
`"api.github.com"`.

### renv.config.gitlab.host

The default GitLab host to be used during package retrieval. Defaults to
`"gitlab.com"`.

### renv.config.hydrate.libpaths

A character vector of library paths, to be used by
[`hydrate()`](https://rstudio.github.io/renv/dev/reference/hydrate.md)
when attempting to hydrate projects. When empty, the default set of
library paths (as documented in
[`?renv::hydrate`](https://rstudio.github.io/renv/dev/reference/hydrate.md))
are used instead. See
[`hydrate()`](https://rstudio.github.io/renv/dev/reference/hydrate.md)
for more details. Defaults to `NULL`.

### renv.config.install.build

Should downloaded package archives be built (via `R CMD build`) before
installation? When TRUE, package vignettes will also be built as part of
package installation. Because building packages before installation may
require packages within 'Suggests' to be available, this option is not
enabled by default. Defaults to `FALSE`.

### renv.config.install.remotes

Should renv read a package's `Remotes:` field when determining how
package dependencies should be installed? Defaults to `TRUE`.

### renv.config.install.shortcuts

Allow for a set of 'shortcuts' when installing packages with renv? When
enabled, if renv discovers that a package to be installed is already
available within the user or site libraries, then it will install a
local copy of that package. Defaults to `TRUE`.

### renv.config.install.staged

DEPRECATED: Please use `renv.config.install.transactional` instead.
Defaults to `TRUE`.

### renv.config.install.transactional

Perform a transactional install of packages during install and restore?
When enabled, renv will first install packages into a temporary library,
and later copy or move those packages back into the project library only
if all packages were successfully downloaded and installed. This can be
useful if you'd like to avoid mutating your project library if
installation of one or more packages fails. Defaults to `TRUE`.

### renv.config.install.verbose

Be verbose when installing R packages from sources? When `TRUE`, renv
will stream any output generated during package build + installation to
the console. Defaults to `FALSE`.

### renv.config.locking.enabled

Use interprocess locks when invoking methods which might mutate the
project library? Enable this to allow multiple processes to use the same
renv project, while minimizing risks relating to concurrent access to
the project library. Disable this if you encounter locking issues. Locks
are stored as files within the project at `renv/lock`; if you need to
manually remove a stale lock you can do so via
`unlink("renv/lock", recursive = TRUE)`. Defaults to `FALSE`.

### renv.config.mran.enabled

DEPRECATED: MRAN is no longer maintained by Microsoft. Defaults to
`FALSE`.

### renv.config.pak.enabled

Use the [pak](https://pak.r-lib.org/) package to install packages?
Defaults to `FALSE`.

### renv.config.ppm.enabled

Boolean; enable [Posit Package
Manager](https://packagemanager.posit.co/) integration in renv projects?
When `TRUE`, renv will attempt to transform repository URLs used by PPM
into binary URLs as appropriate for the current Linux platform. Set this
to `FALSE` if you'd like to continue using source-only PPM URLs, or if
you find that renv is improperly transforming your repository URLs. You
can still set and use PPM repositories with this option disabled; it
only controls whether renv tries to transform source repository URLs
into binary URLs on your behalf. Defaults to `TRUE`.

### renv.config.ppm.default

Boolean; should new projects use the [Posit Public Package
Manager](https://packagemanager.posit.co/) instance by default? When
`TRUE` (the default), projects initialized with
[`renv::init()`](https://rstudio.github.io/renv/dev/reference/init.md)
will use the P3M instance if the `repos` R option has not already been
set by some other means (for example, in a startup `.Rprofile`).
Defaults to `TRUE`.

### renv.config.ppm.url

The default PPM URL to be used for new renv projects. Defaults to the
CRAN mirror maintained by Posit at <https://packagemanager.posit.co/>.
This option can be changed if you'd like renv to use an alternate
package manager instance. Defaults to
`"https://packagemanager.posit.co/cran/latest"`.

### renv.config.repos.override

Override the R package repositories used during
[`restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)?
Primarily useful for deployment / continuous integration, where you
might want to enforce the usage of some set of repositories over what is
defined in `renv.lock` or otherwise set by the R session. Defaults to
`NULL`.

### renv.config.rspm.enabled

DEPRECATED: Please use `renv.config.ppm.enabled` instead. Defaults to
`TRUE`.

### renv.config.sandbox.enabled

Enable sandboxing for renv projects? When active, renv will attempt to
sandbox the system library, preventing non-system packages installed in
the system library from becoming available in renv projects. (That is,
only packages with priority `"base"` or `"recommended"`, as reported by
[`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html),
are made available.) Sandboxing is done by linking or copying system
packages into a separate library path, and then instructing R to use
that library path as the system library path. In some environments, this
action can take a large amount of time – in such a case, you may want to
disable the renv sandbox. Defaults to `TRUE`.

### renv.config.shims.enabled

Should renv shims be installed on package load? When enabled, renv will
install its own shims over the functions
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html),
[`update.packages()`](https://rdrr.io/r/utils/update.packages.html) and
[`remove.packages()`](https://rdrr.io/r/utils/remove.packages.html),
delegating these functions to
[`install()`](https://rstudio.github.io/renv/dev/reference/install.md),
[`update()`](https://rstudio.github.io/renv/dev/reference/update.md) and
[`remove()`](https://rstudio.github.io/renv/dev/reference/remove.md) as
appropriate. Defaults to `TRUE`.

### renv.config.snapshot.inference

For packages which were installed from local sources, should renv try to
infer the package's remote from its DESCRIPTION file? When `TRUE`, renv
will check and prompt you to update the package's DESCRIPTION file if
the remote source can be ascertained. Currently, this is only
implemented for packages hosted on GitHub. Note that this check is only
performed in interactive R sessions. Defaults to `TRUE`.

### renv.config.snapshot.validate

Validate R package dependencies when calling snapshot? When `TRUE`, renv
will attempt to diagnose potential issues in the project library before
creating `renv.lock` – for example, if a package installed in the
project library depends on a package which is not currently installed.
Defaults to `TRUE`.

### renv.config.startup.quiet

Be quiet during startup? When set, renv will not display the typical
`Project <path> loaded. [renv <version>]` banner on startup. Defaults to
`NULL`.

### renv.config.synchronized.check

Check that the project library is synchronized with the lockfile on
load? Defaults to `TRUE`.

### renv.config.sysreqs.check

Check whether the requisite system packages are installed during package
installation and restore? This feature uses the R System Requirements
database maintained at
<https://github.com/rstudio/r-system-requirements>. Defaults to `TRUE`.

### renv.config.updates.check

Check for package updates when the session is initialized? This can be
useful if you'd like to ensure that your project lockfile remains
up-to-date with packages as they are released on CRAN. Defaults to
`FALSE`.

### renv.config.updates.parallel

Check for package updates in parallel? This can be useful when a large
number of packages installed from non-CRAN remotes are installed, as
these packages can then be checked for updates in parallel. Defaults to
`2L`.

### renv.config.user.environ

Load the user R environ (typically located at `~/.Renviron`) when renv
is loaded? Defaults to `TRUE`.

### renv.config.user.library

Include the system library on the library paths for your projects? Note
that this risks breaking project encapsulation and is not recommended
for projects which you intend to share or collaborate on with other
users. See also the caveats for the `renv.config.external.libraries`
option. Defaults to `FALSE`.

### renv.config.user.profile

Load the user R profile (typically located at `~/.Rprofile`) when renv
is loaded? This is disabled by default, as running arbitrary code from
the the user `~/.Rprofile` could risk breaking project encapsulation. If
your goal is to set environment variables that are visible within all
renv projects, then placing those in `~/.Renviron` is often a better
choice. Defaults to `FALSE`.

## Copy methods

If you find that renv is unable to copy some directories in your
environment, you may want to try setting the `copy.method` option. By
default, renv will try to choose a system tool that is likely to succeed
in copying files on your system – `robocopy` on Windows, and `cp` on
Unix. renv will also instruct these tools to preserve timestamps and
attributes when copying files. However, you can select a different
method as appropriate.

The following methods are supported:

|            |                                                                               |
|------------|-------------------------------------------------------------------------------|
| `auto`     | Use `robocopy` on Windows, and `cp` on Unix-alikes.                           |
| `R`        | Use R's built-in [`file.copy()`](https://rdrr.io/r/base/files.html) function. |
| `cp`       | Use `cp` to copy files.                                                       |
| `robocopy` | Use `robocopy` to copy files. (Only available on Windows.)                    |
| `rsync`    | Use `rsync` to copy files.                                                    |

You can also provide a custom copy method if required; e.g.

    options(renv.config.copy.method = function(src, dst) {
      # copy a file from 'src' to 'dst'
    })

Note that renv will always first attempt to copy a directory first to a
temporary path within the target folder, and then rename that temporary
path to the final target destination. This helps avoid issues where a
failed attempt to copy a directory could leave a half-copied directory
behind in the final location.

## Project-local settings

For settings that should persist alongside a particular project, the
various settings available in
[settings](https://rstudio.github.io/renv/dev/reference/settings.md) can
be used.

## Examples

``` r
# disable automatic snapshots
options(renv.config.auto.snapshot = FALSE)

# disable with environment variable
Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = FALSE)
```
