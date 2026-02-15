
renv_graph_create <- function(remotes, records = NULL) {
  graph <- renv_graph_init(remotes, records = records)
  renv_graph_sort(graph)
}

renv_graph_init <- function(remotes, records = NULL) {

  # create an environment to track resolved descriptions (avoids cycles/dupes)
  envir <- new.env(parent = emptyenv())

  # when the user passes dependencies = TRUE/FALSE, install() sets
  # the$install_dependency_fields; apply extended fields (e.g. Suggests)
  # only to the top-level remotes, not to transitive dependencies.
  # when NULL, read from project settings so that e.g. Suggests is
  # respected when configured in settings$package.dependency.fields
  fields <- the$install_dependency_fields %||% {
    project <- renv_project_resolve()
    renv_description_dependency_fields(NULL, project = project)
  }

  # phase 1: resolve top-level remotes with extended dependency fields;
  # BFS ensures explicit user requests take priority over implicit lookups
  queue <- list()
  for (remote in remotes) {
    deps <- renv_graph_resolve(remote, envir, records = records, fields = fields)
    queue <- c(queue, as.list(deps))
  }

  # if any resolved description indicates Bioconductor is needed (via Source
  # or biocViews), activate Bioconductor repos for the rest of resolution
  resolved <- as.list(envir)
  bioc <- any(vapply(resolved, function(desc) {
    source <- renv_record_source(desc, normalize = TRUE)
    biocviews <- desc[["biocViews"]] %||% ""
    identical(source, "bioconductor") || nzchar(biocviews)
  }, logical(1L)))

  if (bioc) {
    project <- renv_restore_state(key = "project") %||% renv_project_resolve()
    renv_scope_bioconductor(project = project)
  }

  # phase 2: resolve transitive dependencies with default fields
  while (length(queue) > 0L) {
    remote <- queue[[1L]]
    queue <- queue[-1L]
    deps <- renv_graph_resolve(remote, envir, records = records)
    queue <- c(queue, as.list(deps))
  }

  as.list(envir)

}

renv_graph_resolve <- function(remote, envir, records = NULL, fields = NULL) {

  # resolve the record; use pre-resolved record if available
  record <- if (is.character(remote) && !is.null(records[[remote]]))
    records[[remote]]
  else
    renv_remotes_resolve(remote)

  # resolve lazy records (functions that produce the actual record)
  if (is.function(record))
    record <- record()

  # skip if already resolved; because we use BFS, top-level remotes
  # are always resolved before transitive dependencies, so the first
  # resolution for a given package name wins
  package <- record[["Package"]]
  if (exists(package, envir = envir, inherits = FALSE))
    return(character())

  # reserve the slot to prevent re-processing via dependency cycles
  assign(package, NULL, envir = envir)

  # fetch DESCRIPTION-level metadata for this record;
  # renv_graph_description_repository includes cellar via
  # renv_available_packages_latest(cellar = TRUE)
  desc <- catch(renv_graph_description(record))
  if (inherits(desc, "error")) {
    reason <- conditionMessage(desc)
    desc <- list()
    attr(desc, "resolution_failed") <- TRUE
    attr(desc, "resolution_error") <- reason
  }

  # merge record fields (Source, Remote* fields) into description
  for (field in names(record))
    if (is.null(desc[[field]]))
      desc[[field]] <- record[[field]]

  # supplement with installed DESCRIPTION for non-standard dependency fields
  # (e.g. Config/Needs/protein); PACKAGES metadata only has standard fields
  if (!is.null(fields)) {
    installed <- catch(renv_description_read(package = package))
    if (!inherits(installed, "error")) {
      for (field in fields)
        if (is.null(desc[[field]]) && !is.null(installed[[field]]))
          desc[[field]] <- installed[[field]]
    }
  }

  # store the resolved description
  assign(package, desc, envir = envir)

  # return dependencies for the caller to enqueue
  renv_graph_deps(desc, fields = fields)

}

renv_graph_description <- function(record) {

  source <- renv_record_source(record, normalize = TRUE)

  case(
    source == "repository"   ~ renv_graph_description_repository(record),
    source == "bioconductor" ~ renv_graph_description_bioconductor(record),
    source == "github"       ~ renv_graph_description_github(record),
    source == "gitlab"       ~ renv_graph_description_gitlab(record),
    source == "bitbucket"    ~ renv_graph_description_bitbucket(record),
    source == "git"          ~ renv_graph_description_github(record),
    source == "local"        ~ renv_graph_description_local(record),
    source == "url"          ~ as.list(record),
    TRUE                     ~ renv_graph_description_repository(record)
  )

}

renv_graph_description_repository <- function(record) {

  package <- record$Package
  version <- record$Version

  # try available packages entry (returns full fields including Imports, Depends);
  # we need these fields for dependency graph resolution
  entry <- catch(renv_available_packages_entry(package, filter = version))
  if (!inherits(entry, "error"))
    return(as.list(entry))

  # try cellar via renv_available_packages_latest (includes cellar = TRUE);
  # cellar packages won't be found by renv_available_packages_entry.
  # NOTE: renv_available_packages_latest only returns limited fields
  # (Package, Version, etc.) — no Depends/Imports/LinkingTo
  latest <- catch(renv_available_packages_latest(package))
  if (!inherits(latest, "error")) {
    if (is.null(version) || identical(latest$Version, version))
      return(as.list(latest))
  }

  # if the package exists in configured repos at a different version, use
  # the full-field entry with overridden version; renv_available_packages_entry
  # without filter returns complete dependency fields unlike latest
  if (!is.null(version) && !inherits(latest, "error")) {
    full <- catch(renv_available_packages_entry(package))
    if (!inherits(full, "error")) {
      desc <- as.list(full)
      desc$Version <- version
      return(desc)
    }
  }

  # fall back to crandb for archived versions not in any configured repo
  if (!is.null(version)) {
    crandb <- catch(renv_graph_description_crandb(package, version))
    if (!inherits(crandb, "error"))
      return(as.list(crandb))
  }

  # if we found any entry at all (e.g. version filter excluded it),
  # use the latest available description
  if (!inherits(latest, "error"))
    return(as.list(latest))

  # all lookups failed; signal an error so the caller can warn
  stopf("package '%s' is not available", package)

}

renv_graph_description_bioconductor <- function(record) {

  # activate Bioconductor repositories in this context
  version <- record[["git_branch"]]
  if (!is.null(version)) {
    parts <- strsplit(version, "_", fixed = TRUE)[[1L]]
    ok <- length(parts) == 3L && tolower(parts[[1L]]) == "release"
    if (!ok) version <- NULL else version <- paste(parts[2:3], collapse = ".")
  }

  project <- renv_restore_state(key = "project") %||% renv_project_resolve()
  renv_scope_bioconductor(project = project, version = version)

  renv_graph_description_repository(record)

}

renv_graph_description_crandb <- function(package, version) {

  url <- sprintf("https://crandb.r-pkg.org/%s/%s", package, version)
  destfile <- renv_scope_tempfile("renv-crandb-")
  download(url, destfile = destfile, quiet = TRUE)
  json <- renv_json_read(file = destfile)

  # convert to DESCRIPTION-like list
  desc <- list(
    Package = json[["Package"]] %||% package,
    Version = json[["Version"]] %||% version
  )

  # convert structured dependency fields to DESCRIPTION-format strings
  fields <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  for (field in fields) {
    value <- json[[field]]
    if (!is.null(value))
      desc[[field]] <- renv_graph_description_crandb_convert(value)
  }

  desc

}

renv_graph_description_crandb_convert <- function(value) {

  parts <- enum_chr(value, function(name, version) {
    sprintf("%s (%s)", name, version)
  })

  fixed <- gsub(" (*)", "", parts, fixed = TRUE)
  paste(fixed, collapse = ", ")

}

renv_graph_description_github <- function(record) {

  host   <- record$RemoteHost %||% config$github.host()
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  subdir <- record$RemoteSubdir
  sha    <- record$RemoteSha

  origin <- fsub("api.github.com", "github.com", renv_retrieve_origin(host))
  url <- paste(c(origin, user, repo), collapse = "/")

  desc <- renv_remotes_resolve_github_description(url, host, user, repo, subdir, sha)
  as.list(desc)

}

renv_graph_description_gitlab <- function(record) {

  host   <- record$RemoteHost %||% config$gitlab.host()
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  subdir <- record$RemoteSubdir
  ref    <- record$RemoteRef

  parts <- c(subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  # scope authentication
  renv_scope_auth(repo)

  # retrieve DESCRIPTION file
  fmt <- "%s/api/v4/projects/%s/repository/files/%s/raw?ref=%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id, descpath, ref)

  destfile <- renv_scope_tempfile("renv-description-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  desc <- renv_dcf_read(destfile)
  as.list(desc)

}

renv_graph_description_bitbucket <- function(record) {

  host   <- record$RemoteHost %||% config$bitbucket.host()
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  ref    <- record$RemoteRef

  # scope authentication
  renv_scope_auth(repo)

  # get DESCRIPTION file
  fmt <- "%s/repositories/%s/%s/src/%s/DESCRIPTION"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_scope_tempfile("renv-description-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  desc <- renv_dcf_read(destfile)
  as.list(desc)

}

renv_graph_description_local <- function(record) {

  # try reading DESCRIPTION from the local path;
  # renv_description_read handles directories, archives, and files
  path <- record$Path %||% record$RemoteUrl
  if (!is.null(path) && file.exists(path)) {
    desc <- catch(renv_description_read(path))
    if (!inherits(desc, "error"))
      return(as.list(desc))
  }

  as.list(record)

}

renv_graph_deps <- function(desc, fields = NULL) {

  fields <- fields %||% c("Depends", "Imports", "LinkingTo")

  deps <- character()
  for (field in fields) {
    value <- desc[[field]]
    if (is.null(value) || is.na(value))
      next
    parsed <- renv_description_parse_field(value)
    if (!is.null(parsed))
      deps <- c(deps, parsed$Package)
  }

  # exclude base packages (R, utils, methods, etc.)
  deps <- setdiff(unique(deps), renv_packages_base())
  deps

}

renv_graph_sort <- function(descriptions) {

  packages <- names(descriptions)
  n <- length(packages)
  if (n == 0L)
    return(descriptions)

  # build adjacency: for each package, its deps that are within the set
  adj <- lapply(descriptions, function(desc) {
    deps <- renv_graph_deps(desc)
    intersect(deps, packages)
  })
  names(adj) <- packages

  # compute in-degree (number of deps within the set)
  indegree <- integer(n)
  names(indegree) <- packages
  for (pkg in packages)
    indegree[pkg] <- length(adj[[pkg]])

  # build reverse adjacency: for each package, who depends on it
  revadj <- vector("list", n)
  names(revadj) <- packages
  for (pkg in packages)
    revadj[pkg] <- list(character())

  for (pkg in packages)
    for (dep in adj[[pkg]])
      revadj[[dep]] <- c(revadj[[dep]], pkg)

  # Kahn's algorithm
  queue <- packages[indegree == 0L]
  result <- character()

  while (length(queue) > 0L) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    result <- c(result, current)

    for (dependent in revadj[[current]]) {
      indegree[[dependent]] <- indegree[[dependent]] - 1L
      if (indegree[[dependent]] == 0L)
        queue <- c(queue, dependent)
    }
  }

  # handle cycles: append any remaining packages
  remaining <- setdiff(packages, result)
  if (length(remaining) > 0L) {
    warningf("dependency cycle detected among: %s", paste(remaining, collapse = ", "))
    result <- c(result, remaining)
  }

  descriptions[result]

}

renv_graph_waves <- function(descriptions) {

  packages <- names(descriptions)
  n <- length(packages)
  if (n == 0L)
    return(list())

  # build adjacency: for each package, its deps within the install set
  adj <- lapply(descriptions, function(desc) {
    deps <- renv_graph_deps(desc)
    intersect(deps, packages)
  })
  names(adj) <- packages

  # compute in-degree
  indegree <- integer(n)
  names(indegree) <- packages
  for (pkg in packages)
    indegree[pkg] <- length(adj[[pkg]])

  # build reverse adjacency
  revadj <- vector("list", n)
  names(revadj) <- packages
  for (pkg in packages)
    revadj[pkg] <- list(character())

  for (pkg in packages)
    for (dep in adj[[pkg]])
      revadj[[dep]] <- c(revadj[[dep]], pkg)

  # iterative peeling: each iteration collects all packages with in-degree 0
  waves <- list()
  remaining <- packages

  while (length(remaining) > 0L) {

    # find all packages with no unresolved dependencies
    ready <- remaining[indegree[remaining] == 0L]

    # handle cycles: if nothing is ready, dump remaining into final wave
    if (length(ready) == 0L) {
      warningf("dependency cycle detected among: %s", paste(remaining, collapse = ", "))
      waves <- c(waves, list(remaining))
      break
    }

    waves <- c(waves, list(ready))
    remaining <- setdiff(remaining, ready)

    # decrement in-degree for dependents
    for (pkg in ready)
      for (dependent in revadj[[pkg]])
        indegree[[dependent]] <- indegree[[dependent]] - 1L

  }

  waves

}

renv_graph_urls <- function(descriptions) {

  result <- vector("list", length(descriptions))
  names(result) <- names(descriptions)

  for (i in seq_along(descriptions)) {

    desc <- descriptions[[i]]
    package <- desc$Package
    source <- renv_record_source(desc, normalize = TRUE)

    entry <- tryCatch(
      switch(source,
        repository   = renv_graph_url_repository(desc),
        bioconductor = renv_graph_url_bioconductor(desc),
        github       = renv_graph_url_github(desc),
        gitlab       = renv_graph_url_gitlab(desc),
        url          = renv_graph_url_url(desc),
        local        = renv_graph_url_local(desc),
        cellar       = renv_graph_url_cellar(desc),
        NULL
      ),
      error = function(e) NULL
    )

    result[[package]] <- entry

  }

  result

}

renv_graph_url_bioconductor <- function(desc) {

  project <- renv_restore_state(key = "project") %||% renv_project_resolve()
  renv_scope_bioconductor(project = project)
  renv_graph_url_repository(desc)

}

renv_graph_url_repository <- function(desc) {

  package <- desc$Package
  version <- desc$Version

  # use renv_available_packages_latest to select the best record;
  # this handles binary vs source selection (including P3M),
  # NeedsCompilation checks, and toolchain availability
  record <- catch(renv_available_packages_latest(package))
  if (inherits(record, "error"))
    return(NULL)

  # if the latest version matches, use it directly
  if (is.null(version) || identical(record$Version, version))
    return(renv_graph_url_repository_record(desc, record))

  # the latest version doesn't match; try a version-filtered lookup
  # using the same type that was selected for the latest
  type <- attr(record, "type", exact = TRUE) %||% "source"
  entry <- catch(renv_available_packages_entry(
    package = package,
    type    = type,
    filter  = version,
    prefer  = desc[["Repository"]]
  ))

  if (!inherits(entry, "error")) {
    tagged <- renv_available_packages_record(entry, type)
    return(renv_graph_url_repository_record(desc, tagged))
  }

  # the requested version is not in current available packages;
  # try the CRAN-style archive (source only)
  root <- renv_retrieve_repos_archive_root(
    attr(record, "url", exact = TRUE),
    as.list(desc)
  )

  if (!is.null(root)) {
    repourl <- attr(record, "url", exact = TRUE)
    reponame <- attr(record, "name", exact = TRUE)
    name <- renv_retrieve_repos_archive_name(as.list(desc), "source")
    url <- file.path(root, name)
    destfile <- renv_retrieve_path(as.list(desc), type = "source")
    return(list(
      url      = url,
      destfile = destfile,
      type     = "repository",
      pkgtype  = "source",
      repourl  = repourl,
      reponame = reponame
    ))
  }

  # couldn't resolve; fall back to sequential retrieval
  NULL

}

renv_graph_url_repository_record <- function(desc, record) {

  type <- attr(record, "type", exact = TRUE) %||% "source"
  repo <- attr(record, "url", exact = TRUE)
  reponame <- attr(record, "name", exact = TRUE)
  name <- renv_retrieve_repos_archive_name(record, type)

  url <- file.path(repo, name)
  destfile <- renv_retrieve_path(as.list(desc), type = type)

  # carry repository metadata so install can tag the record
  # for renv_package_augment (RemoteRepos, RemoteReposName)
  list(
    url      = url,
    destfile = destfile,
    type     = "repository",
    pkgtype  = type,
    repourl  = repo,
    reponame = reponame
  )

}

renv_graph_url_github <- function(desc) {

  host   <- desc$RemoteHost %||% config$github.host()
  origin <- renv_retrieve_origin(host)
  user   <- desc$RemoteUsername
  repo   <- desc$RemoteRepo
  ref    <- desc$RemoteSha %||% desc$RemoteRef

  if (is.null(ref) || is.null(user) || is.null(repo))
    return(NULL)

  url <- sprintf("%s/repos/%s/%s/tarball/%s", origin, user, repo, ref)
  destfile <- renv_retrieve_path(as.list(desc))

  list(url = url, destfile = destfile, type = "github")

}

renv_graph_url_gitlab <- function(desc) {

  host   <- desc$RemoteHost %||% config$gitlab.host()
  origin <- renv_retrieve_origin(host)
  user   <- desc$RemoteUsername
  repo   <- desc$RemoteRepo
  sha    <- desc$RemoteSha %||% desc$RemoteRef

  if (is.null(user) || is.null(repo))
    return(NULL)

  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf("%s/api/v4/projects/%s/repository/archive.tar.gz", origin, id)
  if (!is.null(sha))
    url <- paste(url, paste("sha", sha, sep = "="), sep = "?")

  destfile <- renv_retrieve_path(as.list(desc))

  list(url = url, destfile = destfile, type = "gitlab")

}

renv_graph_url_url <- function(desc) {

  url <- desc$RemoteUrl
  if (is.null(url)) {
    # fall back to RemotePkgRef parsing (see renv_retrieve_url_resolve)
    pkgref <- desc$RemotePkgRef
    if (!is.null(pkgref)) {
      remote <- renv_remotes_parse(pkgref)
      if (identical(remote$type, "url"))
        url <- remote$url
    }
  }

  if (is.null(url))
    return(NULL)

  hash <- md5(url)
  ext <- fileext(url, default = ".tar.gz")
  destfile <- renv_paths_source("url", paste0(hash, ext))

  list(url = url, destfile = destfile, type = "url")

}

renv_graph_url_local <- function(desc) {

  # check Path and Source fields for a local file path
  for (field in c("Path", "Source")) {
    path <- desc[[field]]
    if (is.null(path) || !nzchar(path))
      next
    if (!grepl("[/\\\\]|[.](?:zip|tgz|gz)$", path))
      next
    if (!file.exists(path))
      next

    path <- renv_path_normalize(path, mustWork = TRUE)
    destfile <- renv_retrieve_path(as.list(desc))

    # use a file URI so the parallel downloaders can handle it uniformly
    url <- paste0("file://", path)
    return(list(url = url, destfile = destfile, type = "local"))
  }

  NULL

}

renv_graph_url_cellar <- function(desc) {

  # cellar records from renv_available_packages_latest carry the
  # file URI in their "url" attribute (set by renv_record_tag)
  url <- attr(desc, "url", exact = TRUE)
  type <- attr(desc, "type", exact = TRUE) %||% "source"

  if (is.null(url))
    return(NULL)

  # the url points to the cellar directory; construct the tarball name
  name <- renv_retrieve_repos_archive_name(as.list(desc), type)
  href <- file.path(url, name)

  destfile <- renv_retrieve_path(as.list(desc), type = type)

  list(
    url       = href,
    destfile  = destfile,
    type      = "cellar",
    cellarurl = url
  )

}

renv_graph_download <- function(records) {

  packages <- names(records)

  # set up restore state; recursive = FALSE because we already
  # resolved the full dependency graph up front
  renv_scope_restore(
    project   = renv_project_resolve(),
    library   = renv_libpaths_active(),
    records   = records,
    packages  = packages,
    recursive = FALSE
  )

  # prepare retrieve environment (repos, PPM, user agent)
  # this setup is normally done inside renv_retrieve_impl;
  # we replicate it here so forked processes inherit these options
  repos <- renv_repos_normalize()
  if (renv_ppm_enabled())
    repos <- renv_ppm_transform(repos)

  agent <- renv_http_useragent()
  if (!grepl("renv", agent))
    agent <- paste(sprintf("renv (%s)", renv_metadata_version()), agent, sep = "; ")

  renv_scope_options(repos = repos, HTTPUserAgent = agent)

  before <- Sys.time()

  # resolve download URLs for all packages
  url_info <- renv_graph_urls(records)

  # split into packages we can batch-download vs those needing sequential retrieval
  downloadable <- Filter(Negate(is.null), url_info)
  fallback_pkgs <- setdiff(packages, names(downloadable))

  # batch download for packages with resolved URLs
  if (length(downloadable)) {
    urls      <- vapply(downloadable, `[[`, character(1L), "url")
    destfiles <- vapply(downloadable, `[[`, character(1L), "destfile")
    types     <- vapply(downloadable, `[[`, character(1L), "type")
    ok <- renv_download_parallel(urls, destfiles, types)

    # build records for successful downloads
    for (pkg in names(downloadable)) {
      if (isTRUE(ok[[pkg]])) {
        record <- as.list(records[[pkg]])
        record$Path <- downloadable[[pkg]]$destfile
        records[[pkg]] <- record
      } else {
        # failed parallel download; try sequential fallback
        fallback_pkgs <- c(fallback_pkgs, pkg)
      }
    }
  }

  # sequential fallback for packages that can't be batch-downloaded
  for (pkg in fallback_pkgs) {
    status <- catch({
      renv_retrieve_impl_one(pkg)
      renv_restore_state()$install$data()[[pkg]]
    })
    if (!inherits(status, "error") && !is.null(status))
      records[[pkg]] <- status
  }

  after <- Sys.time()

  names(records) <- packages
  records <- Filter(Negate(is.null), records)

  count <- length(records)
  if (count) {
    elapsed <- difftime(after, before, units = "secs")
    writef("Successfully downloaded %s in %s.", nplural("package", count), renv_difftime_format(elapsed))
    writef("")
  }

  records

}

renv_graph_install <- function(descriptions, jobs = 1L) {

  packages <- names(descriptions)
  if (length(packages) == 0L)
    return(invisible(list()))

  project <- renv_project_resolve()
  library <- renv_libpaths_active()

  # set up restore state if not already provided by the caller
  state <- the$restore_state %||% renv_scope_restore(
    project   = project,
    library   = library,
    records   = descriptions,
    packages  = packages,
    recursive = FALSE
  )

  # prepare retrieve environment (repos, PPM, user agent)
  repos <- renv_repos_normalize()
  if (renv_ppm_enabled())
    repos <- renv_ppm_transform(repos)

  agent <- renv_http_useragent()
  if (!grepl("renv", agent))
    agent <- paste(sprintf("renv (%s)", renv_metadata_version()), agent, sep = "; ")

  renv_scope_options(repos = repos, HTTPUserAgent = agent)

  # prepare install environment (inherited by subprocesses)
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "NULL", R_LIBS_SITE = "NULL")
  renv_scope_rtools()

  tar <- Sys.getenv("R_INSTALL_TAR", unset = renv_tar_exe(default = "internal"))
  renv_scope_envvars(R_INSTALL_TAR = tar)

  if (renv_platform_macos())
    renv_xcode_check()

  renv_scope_install()

  # set up staged install if configured
  staged <- renv_config_install_staged()
  installdir <- library

  if (staged) {
    templib <- renv_install_staged_library_path()
    defer(unlink(templib, recursive = TRUE))
    renv_scope_libpaths(c(templib, renv_libpaths_all()))
    installdir <- templib
  }

  # determine cache settings
  linkable <- renv_cache_linkable(project = project, library = library)
  linker <- if (linkable) renv_file_link else renv_file_copy

  # compute waves
  waves <- renv_graph_waves(descriptions)

  # staging directory for unpacked packages; scoped to this function
  # so temp files from renv_package_unpack survive until install completes
  staging <- renv_scope_tempfile("renv-graph-staging-")
  ensure_directory(staging)

  # track results, failures, and error messages
  all_records <- list()
  failed <- character()
  errors <- stack()
  verbose <- config$install.verbose()

  writef(header("Installing packages"))

  before <- Sys.time()

  for (wave in waves) {

    # remove packages that already failed in previous waves
    wave <- setdiff(wave, failed)

    if (length(wave) == 0L)
      next

    # filter out packages that are already correctly installed;
    # renv_restore_find returns a non-empty path when the installed
    # version matches the record (and the package wasn't explicitly
    # requested for reinstall via state$packages)
    wave <- Filter(function(pkg) {
      path <- renv_restore_find(pkg, descriptions[[pkg]])
      !nzchar(path)
    }, wave)

    if (length(wave) == 0L)
      next

    # sort for deterministic output ordering
    wave <- sort(wave)

    # check cache first: packages with a valid cache entry can be
    # installed directly, skipping the download entirely
    cache_hits <- character()
    for (pkg in wave) {
      desc <- descriptions[[pkg]]
      cacheable <-
        renv_cache_config_enabled(project = project) &&
        renv_record_cacheable(desc) &&
        !renv_restore_rebuild_required(desc)

      if (cacheable) {
        path <- renv_cache_find(desc)
        if (renv_cache_package_validate(path)) {
          renv_install_package_cache(desc, path, linker)
          all_records[[pkg]] <- desc
          cache_hits <- c(cache_hits, pkg)
        }
      }
    }
    wave <- setdiff(wave, cache_hits)

    if (length(wave) == 0L)
      next

    # resolve download URLs for this wave
    wave_descs <- descriptions[wave]
    url_info <- renv_graph_urls(wave_descs)

    downloadable <- Filter(Negate(is.null), url_info)
    fallback_pkgs <- setdiff(wave, names(downloadable))

    # batch download packages with resolved URLs
    dl_before <- Sys.time()

    if (length(downloadable)) {
      urls      <- vapply(downloadable, `[[`, character(1L), "url")
      destfiles <- vapply(downloadable, `[[`, character(1L), "destfile")
      types     <- vapply(downloadable, `[[`, character(1L), "type")

      # ensure parent directories exist
      for (df in destfiles)
        ensure_parent_directory(df)

      dl_ok <- renv_download_parallel(urls, destfiles, types)

      for (pkg in names(downloadable)) {
        if (!isTRUE(dl_ok[[pkg]])) {
          # move to sequential fallback
          fallback_pkgs <- c(fallback_pkgs, pkg)
        }
      }
    }

    # sequential fallback for unsupported sources or failed parallel downloads
    for (pkg in fallback_pkgs) {
      status <- catch({
        renv_scope_options(renv.download.headers = NULL)
        invisible(capture.output(renv_retrieve_impl_one(pkg)))
      })
      if (inherits(status, "error")) {
        # mark the destfile location so we can detect failure below
        downloadable[[pkg]] <- NULL
      } else {
        # retrieve_impl_one stores the record with $Path
        rec <- renv_restore_state()$install$data()[[pkg]]
        if (!is.null(rec))
          downloadable[[pkg]] <- list(
            url      = "",
            destfile = rec$Path,
            type     = renv_record_source(rec, normalize = TRUE)
          )
      }
    }

    dl_after <- Sys.time()

    # report per-package download results and collect records
    records <- list()
    for (package in wave) {

      info <- downloadable[[package]]
      has_file <- !is.null(info) && file.exists(info$destfile)

      if (!has_file) {
        writef("- Failed to download '%s'.", package)
        errors$push(list(package = package, message = "failed to download"))
        failed <- c(failed, package)
        next
      }

      desc <- descriptions[[package]]
      record <- as.list(desc)
      record$Path <- info$destfile

      # tag repository records so renv_package_augment can write
      # RemoteRepos / RemoteReposName into the installed DESCRIPTION
      if (!is.null(info$repourl))
        record <- renv_record_tag(record, info$pkgtype, info$repourl, info$reponame)

      # tag cellar records with their file URI
      if (!is.null(info$cellarurl))
        record <- renv_record_tag(record, info$type, info$cellarurl, "__renv_cellar__")

      msg <- sprintf("- Downloading %s %s ... ", record$Package, record$Version)
      printf(format(msg, width = the$install_step_width))
      elapsed <- difftime(dl_after, dl_before, units = "secs")
      renv_report_ok("downloaded", elapsed = elapsed, verbose = verbose)
      records[[package]] <- record

    }

    # partition into binaries and source packages
    source_queue <- list()

    for (package in names(records)) {

      record <- records[[package]]

      # unpack archives and pre-build if needed; unpacked files are
      # moved into 'staging' so they outlive the unpack function's frame
      path <- catch(renv_graph_install_unpack(record, staging))
      if (inherits(path, "error")) {
        msg <- conditionMessage(path)
        writef("- Failed to prepare '%s': %s", package, msg)
        errors$push(list(package = package, message = msg))
        failed <- c(failed, package)
        next
      }

      # prepare the package for installation
      prepared <- catch(renv_graph_install_prepare(record, path, installdir))
      if (inherits(prepared, "error")) {
        msg <- conditionMessage(prepared)
        writef("- Failed to prepare '%s': %s", package, msg)
        errors$push(list(package = package, message = msg))
        failed <- c(failed, package)
        next
      }

      if (identical(prepared$method, "copy")) {
        # binary: install inline (fast copy)
        renv_install_step_start("Installing", record, verbose = verbose)
        copy_before <- Sys.time()
        result <- catch(renv_graph_install_copy(prepared))
        copy_after <- Sys.time()
        if (inherits(result, "error")) {
          msg <- conditionMessage(result)
          writef("FAILED")
          errors$push(list(package = package, message = msg))
          failed <- c(failed, package)
          next
        }
        copy_elapsed <- difftime(copy_after, copy_before, units = "auto")
        renv_install_step_ok("installed binary", elapsed = copy_elapsed, verbose = verbose)
        renv_graph_install_finalize(record, prepared, installdir, project, linkable)
        all_records[[package]] <- record
      } else {
        # source: queue for concurrent install
        source_queue[[package]] <- list(record = record, prepared = prepared)
      }

    }

    # install source packages concurrently via pipe(), up to `jobs` at a time.
    # we launch a batch of workers (all start concurrently as separate processes),
    # then collect results sequentially (readLines blocks per connection, but
    # all processes in the batch run in parallel)
    if (length(source_queue) > 0L) {

      source_names <- names(source_queue)
      pos <- 1L

      while (pos <= length(source_names)) {

        # determine this batch
        batch <- source_names[pos:min(pos + jobs - 1L, length(source_names))]
        pos <- pos + length(batch)

        # back up existing installations before launching installs
        backups <- list()
        for (pkg in batch) {
          installpath <- file.path(installdir, pkg)
          backups[[pkg]] <- renv_file_backup(installpath)
          if (renv_file_broken(installpath))
            renv_file_remove(installpath)
        }

        # launch all workers in this batch (processes start concurrently)
        workers <- list()
        for (pkg in batch)
          workers[[pkg]] <- renv_graph_install_launch(source_queue[[pkg]]$prepared)

        # collect results sequentially; print progress just before each
        # blocking read so start/ok pairs stay on the same line
        for (pkg in batch) {
          entry <- source_queue[[pkg]]
          renv_install_step_start("Installing", entry$record, verbose = verbose)

          worker <- workers[[pkg]]
          result <- renv_graph_install_collect(worker)

          installpath <- file.path(installdir, pkg)
          if (result$success && file.exists(installpath)) {
            renv_install_step_ok("built from source", elapsed = result$elapsed, verbose = verbose)
            renv_graph_install_finalize(entry$record, entry$prepared, installdir, project, linkable)
            all_records[[pkg]] <- entry$record
          } else {
            # remove partial installation so backup can restore
            installpath <- file.path(installdir, pkg)
            unlink(installpath, recursive = TRUE)
            writef("FAILED")
            if (verbose) writeLines(result$output)
            errors$push(list(package = pkg, message = "install failed", output = result$output))
            failed <- c(failed, pkg)
          }

          # restore backup on failure, clean up on success
          backups[[pkg]]()
        }

      }

    }

  }

  after <- Sys.time()

  # migrate staged packages into real library;
  # if transactional and there were failures, skip migration entirely
  # (the staging directory is cleaned up by defer, leaving the real
  # library unchanged for a clean rollback)
  transactional <- config$install.transactional()
  if (staged && length(all_records) > 0L && !(transactional && length(failed) > 0L)) {
    sources <- file.path(templib, names(all_records))
    sources <- sources[file.exists(sources)]
    targets <- file.path(library, basename(sources))
    names(targets) <- sources
    enumerate(targets, renv_file_move, overwrite = TRUE)

    # clear filebacked cache entries
    descpaths <- file.path(targets, "DESCRIPTION")
    renv_filebacked_clear("renv_description_read", descpaths)
    renv_filebacked_clear("renv_hash_description", descpaths)
  }

  n <- length(all_records)
  if (n > 0L) {
    elapsed <- difftime(after, before, units = "secs")
    writef("Successfully installed %s in %s.", nplural("package", n), renv_difftime_format(elapsed))
  }

  # report errors
  renv_graph_install_errors(errors$data(), failed, descriptions)

  invisible(all_records)

}

renv_graph_install_errors <- function(errors, failed, descriptions) {

  if (length(errors) == 0L && length(failed) == 0L)
    return(invisible())

  # build summary lines for packages that failed directly
  messages <- map_chr(errors, function(item) {
    sprintf("[%s]: %s", item$package, item$message)
  })

  # find packages skipped due to failed dependencies (not in errors themselves)
  error_packages <- map_chr(errors, `[[`, "package")
  skipped <- setdiff(failed, error_packages)
  if (length(skipped) > 0L) {
    skipped_messages <- map_chr(skipped, function(pkg) {
      deps <- renv_graph_deps(descriptions[[pkg]])
      deps_failed <- intersect(deps, failed)
      sprintf("[%s]: dependency failed (%s)", pkg, paste(deps_failed, collapse = ", "))
    })
    messages <- c(messages, skipped_messages)
  }

  writef("")
  bulletin(
    "The following package(s) were not installed successfully:",
    messages,
    "You may need to manually download and install these packages."
  )

  # show full R CMD INSTALL output for failed source installs
  for (item in errors) {
    output <- item$output
    if (is.null(output) || length(output) == 0L)
      next
    writef("Install output for '%s':", item$package)
    writeLines(output)
    writef("")
  }

  invisible()

}

renv_graph_install_unpack <- function(record, staging) {

  package <- record$Package
  path <- record$Path

  # check if it's an archive
  info <- renv_file_info(path)
  isarchive <- identical(info$isdir, FALSE)

  subdir <- record$RemoteSubdir %||% ""
  if (isarchive) {
    # renv_package_unpack creates a temp dir scoped to this function's
    # caller (parent.frame()); move the result into the staging directory
    # so it persists after this function returns
    unpacked <- renv_package_unpack(package, path, subdir = subdir)
    dest <- file.path(staging, package)
    renv_file_move(unpacked, dest)
    path <- dest
  } else if (nzchar(subdir)) {
    path <- paste(c(path, subdir), collapse = "/")
  }

  # check whether we should pre-build
  path <- renv_install_package_impl_prebuild(record, path, quiet = TRUE)

  # normalize
  renv_path_normalize(path, mustWork = TRUE)

}

renv_graph_install_prepare <- function(record, path, library) {

  package <- record$Package

  # determine package type
  isdir <- renv_file_type(path, symlinks = FALSE) == "directory"
  isbin <- renv_package_type(path, quiet = TRUE) == "binary"
  copyable <- isdir && isbin

  if (copyable) {

    list(
      method  = "copy",
      path    = path,
      library = library,
      package = package
    )

  } else {

    # construct R CMD INSTALL command
    args <- c(
      "--vanilla",
      "CMD", "INSTALL", "--preclean", "--no-multiarch", "--with-keep.source",
      r_cmd_install_option(package, "configure.args", TRUE),
      r_cmd_install_option(package, "configure.vars", TRUE),
      r_cmd_install_option(package, c("install.opts", "INSTALL_opts"), FALSE),
      "-l", renv_shell_path(library),
      renv_shell_path(path)
    )

    cmd <- paste(renv_shell_path(R()), paste(args, collapse = " "))

    list(
      method  = "install",
      cmd     = cmd,
      path    = path,
      library = library,
      package = package
    )

  }

}

renv_graph_install_launch <- function(prepared) {

  con <- pipe(paste(prepared$cmd, "2>&1"), open = "r")

  list(
    connection = con,
    package    = prepared$package,
    start      = Sys.time()
  )

}

renv_graph_install_collect <- function(worker) {

  con <- worker$connection

  # readLines blocks until the process exits (EOF on pipe)
  lines <- tryCatch(
    readLines(con, warn = FALSE),
    error = function(e) character()
  )

  # close() on a pipe connection returns the process exit status invisibly;
  # R emits a warning on non-zero exit, which we suppress
  status <- tryCatch(
    suppressWarnings(close(con)),
    error = function(e) 1L
  )

  exit_code <- status %||% 0L

  elapsed <- difftime(Sys.time(), worker$start, units = "auto")

  list(
    success = identical(as.integer(exit_code), 0L),
    output  = lines,
    elapsed = elapsed
  )

}

renv_graph_install_copy <- function(prepared) {

  package <- prepared$package
  library <- prepared$library
  installpath <- file.path(library, package)

  # back up existing installation
  callback <- renv_file_backup(installpath)
  defer(callback())

  # remove broken symlinks
  if (renv_file_broken(installpath))
    renv_file_remove(installpath)

  renv_file_copy(prepared$path, installpath, overwrite = TRUE)
  invisible(installpath)

}

renv_graph_install_finalize <- function(record, prepared, library, project, linkable) {

  package <- record$Package
  installpath <- file.path(library, package)

  # test binary loading
  isbin <- renv_package_type(prepared$path, quiet = TRUE) == "binary"
  if (isbin) {
    tryCatch(
      renv_install_test(package),
      error = function(err) {
        unlink(installpath, recursive = TRUE)
        stop(err)
      }
    )
  }

  # augment package metadata
  renv_package_augment(installpath, record)

  # synchronize with cache
  if (renv_cache_config_enabled(project = project))
    renv_cache_synchronize(record, linkable = linkable)

}
