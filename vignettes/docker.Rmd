---
title: "Using renv with Docker"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Using renv with Docker}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

While renv can help capture the state of your R library at some point in time, there are still other aspects of the system that can influence the run-time behavior of your R application.
In particular, the same R code can produce different results depending on:

-   The operating system in use,
-   The compiler flags used when R and packages are built,
-   The LAPACK / BLAS system(s) in use,
-   The versions of system libraries installed and in use,

And so on.
[Docker](https://www.docker.com/) is a tool that helps solve this problem through the use of **containers**.
Very roughly speaking, one can think of a container as a small, self-contained system within which different applications can be run.
Using Docker, one can declaratively state how a container should be built (what operating system it should use, and what system software should be installed within), and use that system to run applications.
(For more details, please see <https://environments.rstudio.com/docker>.)

Using Docker and renv together, one can then ensure that both the underlying system, alongside the required R packages, are fixed and constant for a particular application.

The main challenges in using Docker with renv are:

-   Ensuring that the renv cache is visible to Docker containers, and

-   Ensuring that required R package dependencies are available at run-time.

This vignette will assume you are already familiar with Docker; if you are not yet familiar with Docker, the [Docker Documentation](https://docs.docker.com/) provides a thorough introduction.
To learn more about using Docker to manage R environments, visit [environments.rstudio.com](https://environments.rstudio.com/docker.html).

We'll discuss two strategies for using renv with Docker:

1.  Using renv to install packages when the Docker image is generated;
2.  Using renv to install packages when Docker containers are run.

We'll also explore the pros and cons of each strategy.

## Creating Docker images with renv

With Docker, [Dockerfiles](https://docs.docker.com/engine/reference/builder/) are used to define new images.
Dockerfiles can be used to declaratively specify how a Docker image should be created.
A Docker image captures the state of a machine at some point in time -- e.g., a Linux operating system after downloading and installing R `r getRversion()[1, 1:2]`.
Docker containers can be created using that image as a base, allowing different independent applications to run using the same pre-defined machine state.

First, you'll need to get renv installed on your Docker image.
The easiest way to accomplish this is with the `remotes` package.
For example, you could install the latest release of `renv` from CRAN:

```
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
```

Alternatively, if you needed to use the development version of `renv`, you could use:

```         
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv')"
```

Next, if you'd like the `renv.lock` lockfile to be used to install R packages when the Docker image is built, you'll need to copy it to the container:

```         
WORKDIR /project
COPY renv.lock renv.lock
```

Next, you need to tell renv which library paths to use for package installation.
You can either set the `RENV_PATHS_LIBRARY` environment variable to a writable path within your Docker container, or copy the renv auto-loader tools into the container so that a project-local library can be automatically provisioned and used when R is launched.

```         
# approach one
ENV RENV_PATHS_LIBRARY renv/library

# approach two
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
```

Finally, you can run `renv::restore()` to restore packages as defined in the lockfile:

```         
RUN R -e "renv::restore()"
```

With this, renv will download and install the requisite packages as appropriate when the image is created.
Any new containers created from this image will hence have those R packages installed and visible at run-time.

## Speeding up package installations

The aforementioned approach is useful if you have multiple applications with
identical package requirements. In this case, a single image containing this
identical package library could serve as the parent image for several 
containerized applications.

However, `renv::restore()` is slow -- it needs to download and install packages, 
which can take some time. Thus, some care is required to efficiently make use
of the `renv` cache for projects that require: 

1. Building an image multiple times (e.g., to debug the production application
   as source code is updated), or

2. Calling `renv::restore()` each time the container is run.

The former process can be sped up using multi-stage builds, the latter by dynamically provisioning R Libraries, as described below.

### Multi-stage builds

For projects that require repeatedly building an image, 
[multi-stage builds](https://docs.docker.com/build/building/multi-stage/) 
can be used to speed up the build process. With multi-stage builds, multiple 
FROM statements are used in the Dockerfile and files can be copied across 
build stages. 

This approach can be leveraged to generate more efficient builds
by dedicating a first stage build to package synchronization and a second stage
build to copying files and executing code that may need to be updated often 
across builds (e.g., code that needs to be debugged in the container).

To implement a two stage build, the following code could be used as part of a
Dockerfile.

```
# STAGE 1: renv-related code
FROM <parent_image> AS base

WORKDIR /project

# using approach 2 above
RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf

# change default location of cache to project folder
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

# restore 
RUN R -e "renv::restore()"
```

The above code uses `FROM <parent_image> AS <name>` to name the first stage of 
the build `base`. Here, `<parent_image>` should be replaced with an appropriate 
image name. 

Subsequently, the code uses approach 2 (described above) to copy the auto-loader
to the project directory in the image. It additionally creates the `renv/.cache`
directory that is to be used as the `renv` cache.

The second stage of the build is defined by adding the following code to the 
same Dockerfile, below the previous code chunk.

```
FROM <parent_image>

WORKDIR /project
COPY --from=base /project .

# add commands that need to be debugged below
```

Here, `<parent_image>` could be the same as the parent image of `base`, but does 
not have to be (see 
[documentation](https://docs.docker.com/build/building/multi-stage/) for more 
details).

The key line is the `COPY` command, which specifies that the contents of 
`/project` directory from the `base` image are copied into the `/project` 
directory of this image. 

Any commands that will change frequently across builds could be included below
the `COPY` command. If only this code associated with the second stage 
build is updated then `renv::restore()` will not be called again at build time.
Instead, the layers associated with the `base` image will be loaded from 
Docker's cache, thereby saving significant time in build process.

In fact, `renv::restore()` will only be called when the `base` image needs to be 
rebuilt (e.g., when changes are made to `renv.lock`). Docker's cache system is
generally good at understanding the dependencies of images. However, if you find
that the `base` image is not updating as expected, it is possible to manually 
enforce a clean build by including the `--no-cache` option in the call to 
`docker build`.

### Dynamically Provisioning R Libraries with renv

However, on occasion, one will have multiple applications built from a single base image, but each application will have its own independent R package requirements.
In this case, rather than including the package dependencies in the image itself, it would be preferable for each container to provision its own library at run-time, based on that application's `renv.lock` lockfile.

In effect, this is as simple as ensuring that `renv::restore()` happens at container run-time, rather than image build time.
However, on its own, `renv::restore()` is slow -- it needs to download and install packages, which could take prohibitively long if an application needs to be run repeatedly.

The renv package cache can be used to help ameliorate this issue.
When the cache is enabled, whenever renv attempts to install or restore an R package, it first checks to see whether that package is already available within the renv cache.
If it is, that instance of the package is linked into the project library.
Otherwise, the package is first installed into the renv cache, and then that newly-installed copy is linked for use in the project.

In effect, if the renv cache is available, you should only need to pay the cost of package installation once -- after that, the newly-installed package will be available for re-use across different projects.
At the same time, each project's library will remain independent and isolated from one another, so installing a package within one container won't affect another container.

However, by default, each Docker container will have its own independent filesystem.
Ideally, we'd like for *all* containers launched from a particular image to have access to the same renv cache.
To accomplish this, we'll have to tell each container to use an renv cache located on a shared mount.

In sum, if we'd like to allow for run-time provisioning of R package dependencies, we will need to ensure the renv cache is located on a shared volume, which is visible to any containers launched.
We will accomplish this by:

1.  Setting the `RENV_PATHS_CACHE` environment variable, to tell the instance of renv running in each container where the global cache lives;

2.  Telling Docker to mount some filesystem location from the host filesystem, at some location (`RENV_PATHS_CACHE_HOST`), to a container-specific location (`RENV_PATHS_CACHE_CONTAINER`).

For example, if you had a container running a Shiny application:

```         
# the location of the renv cache on the host machine
RENV_PATHS_CACHE_HOST=/opt/local/renv/cache

# where the cache should be mounted in the container
RENV_PATHS_CACHE_CONTAINER=/renv/cache

# run the container with the host cache mounted in the container
docker run --rm \
    -e "RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}" \
    -v "${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}" \
    -p 14618:14618 \
    R -s -e 'renv::restore(); shiny::runApp(host = "0.0.0.0", port = 14618)'
```

With this, any calls to renv APIs within the created docker container will have access to the mounted cache.
The first time you run a container, renv will likely need to populate the cache, and so some time will be spent downloading and installing the required packages.
Subsequent runs will be much faster, as renv will be able to reuse the global package cache.

The primary downside with this approach compared to the image-based approach is that it requires you to modify how containers are created, and requires a bit of extra orchestration in how containers are launched.
However, once the renv cache is active, newly-created containers will launch very quickly, and a single image can then be used as a base for a myriad of different containers and applications, each with their own independent package dependencies.

## Handling the renv autoloader

When `R` is launched within a project folder, the renv auto-loader (if present) will attempt to download and install renv into the project library.
Depending on how your Docker container is configured, this could fail.
For example:

```         
Error installing renv:
======================
ERROR: unable to create '/usr/local/pipe/renv/library/master/R-4.0/x86_64-pc-linux-gnu/renv'
Warning messages:
1: In system2(r, args, stdout = TRUE, stderr = TRUE) :
  running command ''/usr/lib/R/bin/R' --vanilla CMD INSTALL -l 'renv/library/master/R-4.0/x86_64-pc-linux-gnu' '/tmp/RtmpwM7ooh/renv_0.12.2.tar.gz' 2>&1' had status 1
2: Failed to find an renv installation: the project will not be loaded.
Use `renv::activate()` to re-initialize the project.
```

Bootstrapping renv into the project library might be unnecessary for you.
If that is the case, then you can avoid this behavior by launching R with the `--vanilla` flag set; for example:

```         
R --vanilla -s -e 'renv::restore()'
```
