# Use python

Associate a version of Python with your project.

## Usage

``` r
use_python(
  python = NULL,
  ...,
  type = c("auto", "virtualenv", "conda", "system"),
  name = NULL,
  project = NULL
)
```

## Arguments

- python:

  The path to the version of Python to be used with this project. See
  **Finding Python** for more details.

- ...:

  Optional arguments; currently unused.

- type:

  The type of Python environment to use. When `"auto"` (the default),
  virtual environments will be used.

- name:

  The name or path that should be used for the associated Python
  environment. If `NULL` and `python` points to a Python executable
  living within a pre-existing virtual environment, that environment
  will be used. Otherwise, a project-local environment will be created
  instead, using a name generated from the associated version of Python.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Value

`TRUE`, indicating that the requested version of Python has been
successfully activated. Note that this function is normally called for
its side effects.

## Details

When Python integration is active, renv will:

- Save metadata about the requested version of Python in `renv.lock` –
  in particular, the Python version, and the Python type ("virtualenv",
  "conda", "system"),

- Capture the set of installed Python packages during
  [`renv::snapshot()`](https://rstudio.github.io/renv/dev/reference/snapshot.md),

- Re-install the set of recorded Python packages during
  [`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md).

In addition, when the project is loaded, the following actions will be
taken:

- The `RENV_PYTHON` environment variable will be set, indicating the
  version of Python currently active for this sessions,

- The `RETICULATE_PYTHON` environment variable will be set, so that the
  reticulate package can automatically use the requested copy of Python
  as appropriate,

- The requested version of Python will be placed on the `PATH`, so that
  attempts to invoke Python will resolve to the expected version of
  Python.

You can override the version of Python used in a particular project by
setting the `RENV_PYTHON` environment variable; e.g. as part of the
project's `.Renviron` file. This can be useful if you find that renv is
unable to automatically discover a compatible version of Python to be
used in the project.

## Finding Python

In interactive sessions, when `python = NULL`, renv will prompt for an
appropriate version of Python. renv will search a pre-defined set of
locations when attempting to find Python installations on the system:

- `getOption("renv.python.root")`,

- `/opt/python`,

- `/opt/local/python`,

- `~/opt/python`,

- `/usr/local/opt` (for macOS Homebrew-installed copies of Python),

- `/opt/homebrew/opt` (for M1 macOS Homebrew-installed copies of
  Python),

- `~/.pyenv/versions`,

- Python instances available on the `PATH`.

In non-interactive sessions, renv will first check the
`RETICULATE_PYTHON` environment variable; if that is unset, renv will
look for Python on the `PATH`. It is recommended that the version of
Python to be used is explicitly supplied for non-interactive usages of
`use_python()`.

## Warning

We strongly recommend using Python virtual environments, for a few
reasons:

1.  If something goes wrong with a local virtual environment, you can
    safely delete that virtual environment, and then re-initialize it
    later, without worry that doing so might impact other software on
    your system.

2.  If you choose to use a "system" installation of Python, then any
    packages you install or upgrade will be visible to any other
    application that wants to use that same Python installation. Using a
    virtual environment ensures that any changes made are isolated to
    that environment only.

3.  Choosing to use Anaconda will likely invite extra frustration in the
    future, as you may be required to upgrade and manage your Anaconda
    installation as new versions of Anaconda are released. In addition,
    Anaconda installations tend to work poorly with software not
    specifically installed as part of that same Anaconda installation.

In other words, we recommend selecting "system" or "conda" only if you
are an expert Python user who is already accustomed to managing Python /
Anaconda installations on your own.

## Examples

``` r
if (FALSE) { # \dontrun{

# use python with a project
renv::use_python()

# use python with a project; create the environment
# within the project directory in the '.venv' folder
renv::use_python(name = ".venv")

# use python with a pre-existing virtual environment located elsewhere
renv::use_python(name = "~/.virtualenvs/env")

# use virtualenv python with a project
renv::use_python(type = "virtualenv")

# use conda python with a project
renv::use_python(type = "conda")

} # }
```
