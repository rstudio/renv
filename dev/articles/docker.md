# Using renv with Docker

While renv can help capture the state of your R library at some point in
time, there are still other aspects of the system that can influence the
runtime behavior of your R application. In particular, the same R code
can produce different results depending on:

- The operating system in use,
- The compiler flags used when R and packages are built,
- The LAPACK / BLAS system(s) in use,
- The versions of system libraries installed and in use,

and so on. [Docker](https://www.docker.com/) is a tool that can help
solve this problem through the use of **containers**. Very roughly
speaking, one can think of a container as a small, self-contained system
within which different applications can be run. Using Docker, one can
declaratively state how a container should be built, and then use that
system to run applications. For more details, please see
<https://environments.rstudio.com/docker>.

Using Docker and renv together, one can then ensure that both the
underlying system, alongside the required R packages, are fixed and
constant for a particular application.

This vignette will assume you are already familiar with Docker; if you
are not yet familiar with Docker, the [Docker
Documentation](https://docs.docker.com/) provides a thorough
introduction. To learn more about using Docker to manage R environments,
visit
[environments.rstudio.com](https://environments.rstudio.com/docker.html).

We focus here on the most common case: you already have an existing renv
project and want to build a Docker image from it. We assume that your
project already contains `renv.lock`, `.Rprofile`, `renv/activate.R`,
and `renv/settings.json`.

## Containerizing an existing renv project

For an existing renv project, a good default is to copy the renv
metadata first, restore packages, and only then copy the rest of the
repository:

``` dockerfile
FROM <parent-image>

WORKDIR /project
RUN mkdir -p renv

COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN R -s -e "renv::restore()"

COPY . .
```

This is a good starting point for most projects. The image restore step
uses the same project metadata that you already commit to version
control, so the container can recreate the project library before the
rest of the source tree is copied.

If you need to customize the library path, set `RENV_PATHS_LIBRARY`
before calling
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md):

``` dockerfile
ENV RENV_PATHS_LIBRARY=/project/renv/library
RUN R -s -e "renv::restore()"
```

## Caching package installs

If you rebuild the same image repeatedly, caching can make
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
much faster. There are three common approaches.

### Basic Docker layer cache

The Dockerfile above already uses Docker’s normal layer cache. Because
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
happens before `COPY . .`, changes to application code do not invalidate
the restore layer. Docker only needs to run
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
again when the copied renv files change.

### Cache mounts

If you are using BuildKit, you can also mount a cache directory into the
build. This allows
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
to reuse previously cached packages even when the restore layer itself
needs to be rebuilt.

``` dockerfile
# syntax=docker/dockerfile:1
FROM <parent-image>

WORKDIR /project
RUN mkdir -p renv

COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN --mount=type=cache,target=/root/.cache/R/renv/cache \
    R -s -e "renv::restore()"

COPY . .
```

The line `RUN --mount=type=cache,target=/root/.cache/R/renv/cache` tells
BuildKit to make a persistent build cache available at renv’s cache path
for that one `RUN` instruction, so
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
can reuse previously downloaded packages; see Docker’s [`RUN --mount`
documentation](https://docs.docker.com/reference/dockerfile/#run---mount)
and [cache mount guide](https://docs.docker.com/build/cache/optimize/).

Setting `RENV_CONFIG_CACHE_SYMLINKS=FALSE` is important here because the
cache mount is not part of the final image. With symlinks enabled, renv
could leave the project library pointing at packages in the mounted
cache, and those symlinks would be broken once the build step finishes.

This cache only helps on the specific machine or builder that created
it. It is useful for repeated local builds, but it will not usually
carry over to a different machine or a fresh CI runner.

### Bind-mounted host caches

If the host machine already has a populated renv cache, you can
bind-mount that cache into the build and let
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
reuse it. This is especially useful when the host cache is managed
outside Docker.

The Dockerfile can mount a host-provided cache context into the default
renv cache path for a root-based Linux container:

``` dockerfile
# syntax=docker/dockerfile:1
FROM <parent-image>

WORKDIR /project
RUN mkdir -p renv

COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN --mount=type=bind,from=renv-cache,source=.,target=/root/.cache/R/renv/cache,rw \
    R -s -e "renv::restore()"

COPY . .
```

The line
`RUN --mount=type=bind,from=renv-cache,source=.,target=/root/.cache/R/renv/cache,rw`
tells BuildKit to mount the named build context `renv-cache` at renv’s
cache path for that one `RUN` instruction, with temporary write access;
see Docker’s [`RUN --mount`
documentation](https://docs.docker.com/reference/dockerfile/#run---mount)
and [named contexts
documentation](https://docs.docker.com/build/building/context/#named-contexts).

You can then provide that cache directory at build time with
`docker buildx build`:

``` sh
docker buildx build \
  --build-context renv-cache=.cache/renv \
  -t <image-name> .
```

This approach is most useful when `.cache/renv` has already been
populated on the host, for example by running
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
outside Docker. Bind mounts are read-only by default, so the example
uses `rw` to avoid write failures if
[`renv::restore()`](https://rstudio.github.io/renv/dev/reference/restore.md)
needs to update the cache during the build. Even with `rw`, writes to
the bind mount are only available for the duration of that `RUN`
instruction and are discarded afterwards, so the host-provided cache
context is not modified. This helps keep repeated builds reproducible,
including when multiple builds run sequentially or in parallel.
`RENV_CONFIG_CACHE_SYMLINKS=FALSE` is needed here for the same reason as
in the cache-mount example: the mounted cache is available during the
build step, but it is not carried into the final image. It is often the
preferred approach on ephemeral hosts such as GitHub Actions runners,
because the host-side cache directory can be restored with the CI
platform’s native cache support before the build starts. GitHub Actions
and Azure DevOps both provide native cache features that work well for
this: [GitHub Actions
cache](https://docs.github.com/actions/concepts/workflows-and-actions/dependency-caching)
and [Azure DevOps Cache
task](https://learn.microsoft.com/azure/devops/pipelines/release/caching?view=azure-devops).

If you want to use a different cache location inside the container,
customize the mount target to match your configured renv cache path.

## Handling the renv autoloader

When `R` is launched within a project folder, the renv auto-loader (if
present) will attempt to download and install renv into the project
library if it’s not available. Depending on how your Docker container is
configured, this could fail. For example:

``` sh
Error installing renv:
======================
ERROR: unable to create '/usr/local/pipe/renv/library/master/R-4.0/x86_64-pc-linux-gnu/renv'
Warning messages:
1: In system2(r, args, stdout = TRUE, stderr = TRUE) :
  running command ''/usr/lib/R/bin/R' --vanilla CMD INSTALL -l 'renv/library/master/R-4.0/x86_64-pc-linux-gnu' '/tmp/RtmpwM7ooh/renv_0.12.2.tar.gz' 2>&1' had status 1
2: Failed to find an renv installation: the project will not be loaded.
Use `renv::activate()` to re-initialize the project.
```

Bootstrapping renv into the project library might be unnecessary for
you. If that is the case, then you can avoid this behavior by launching
R with the `--vanilla` flag set; for example:

``` sh
R --vanilla -s -e 'renv::restore()'
```
