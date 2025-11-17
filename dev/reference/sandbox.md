# The default library sandbox

An R installation can have up to three types of library paths available
to the user:

- The *user library*, where R packages downloaded and installed by the
  current user are installed. This library path is only visible to that
  specific user.

- The *site library*, where R packages maintained by administrators of a
  system are installed. This library path, if it exists, is visible to
  all users on the system.

- The *default library*, where R packages distributed with R itself are
  installed. This library path is visible to all users on the system.

Normally, only so-called "base" and "recommended" packages should be
installed in the default library. (You can get a list of these packages
with `installed.packages(priority = c("base", "recommended"))`).
However, it is possible for users and administrators to install packages
into the default library, if the filesystem permissions permit them to
do so. (This, for example, is the default behavior on macOS.)

Because the site and default libraries are visible to all users, having
those accessible in renv projects can potentially break isolation – that
is, if a package were updated in the default library, that update would
be visible to all R projects on the system.

To help defend against this, renv uses something called the "sandbox" to
isolate renv projects from non-"base" packages that are installed into
the default library. When an renv project is loaded, renv will:

- Create a new, empty library path (called the "sandbox"),

- Link only the "base" and "recommended" packages from the default
  library into the sandbox,

- Mark the sandbox as read-only, so that users are unable to install
  packages into this library,

- Instruct the R session to use the "sandbox" as the default library.

This process is mostly transparent to the user. However, because the
sandbox is read-only, if you later need to remove the sandbox, you'll
need to reset file permissions manually; for example, with
`renv::sandbox$unlock()`.

If you'd prefer to keep the sandbox unlocked, you can also set:

    RENV_SANDBOX_LOCKING_ENABLED = FALSE

in an appropriate startup `.Renviron` or `Renviron.site` file.

The sandbox can also be disabled entirely with:

    RENV_CONFIG_SANDBOX_ENABLED = FALSE

The sandbox library path can also be configured using the
`RENV_PATHS_SANDBOX` environment variable: see
[paths](https://rstudio.github.io/renv/dev/reference/paths.md) for more
details.

## Usage

``` r
sandbox
```
