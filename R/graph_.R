
renv_graph_create <- function(remotes, records = list(), project = NULL, scope = parent.frame()) {
  graph <- renv_graph_init(remotes, records = records, project = project, scope = scope)
  renv_graph_sort(graph)
}

renv_graph_init <- function(remotes, records = list(), project = NULL, scope = parent.frame()) {

  # create an environment to track resolved descriptions (avoids cycles/dupes)
  project <- project %||% renv_project_resolve()
  envir <- new.env(parent = emptyenv())

  # when the user passes dependencies = TRUE/FALSE, install() sets
  # the$install_dependency_fields; apply extended fields (e.g. Suggests)
  # only to the top-level remotes, not to transitive dependencies.
  # when NULL, read from project settings so that e.g. Suggests is
  # respected when configured in settings$package.dependency.fields
  fields <- the$install_dependency_fields %||% {
    renv_description_dependency_fields(NULL, project = project)
  }

  # pre-seed with project-level Remotes; these act as fallback records
  # so that packages specified via the project DESCRIPTION Remotes field
  # are resolved from the correct source (e.g. GitHub) even when the
  # caller doesn't explicitly include them in 'records'
  if (!is.null(project) && config$install.remotes()) {
    projrecords <- renv_project_remotes(project)
    for (name in names(projrecords))
      if (is.null(records[[name]]))
        records[[name]] <- projrecords[[name]]
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
  bioc <- map_lgl(resolved, function(desc) {
    source <- renv_record_source(desc, normalize = TRUE)
    biocviews <- desc[["biocViews"]] %||% ""
    identical(source, "bioconductor") || nzchar(biocviews)
  })

  if (any(bioc)) {
    project <- renv_restore_state(key = "project") %||% renv_project_resolve()
    renv_scope_bioconductor(project = project, scope = scope)
  }

  # phase 2: resolve transitive dependencies with default fields
  idx <- 1L
  while (idx <= length(queue)) {
    remote <- queue[[idx]]
    idx <- idx + 1L
    deps <- renv_graph_resolve(remote, envir, records = records)
    queue <- c(queue, as.list(deps))
  }

  as.list(envir)

}

renv_graph_resolve <- function(remote, envir, records = NULL, fields = NULL, override = FALSE) {

  # resolve the record; use pre-resolved record if available
  record <- if (is.character(remote) && !is.null(records[[remote]]))
    records[[remote]]
  else
    renv_remotes_resolve(remote)

  # resolve lazy records (functions that produce the actual record)
  if (is.function(record))
    record <- record()

  # skip base packages (utils, methods, etc.) -- they can't be installed
  package <- record$Package
  if (package %in% renv_packages_base())
    return(character())

  # skip if already resolved; because we use BFS, top-level remotes
  # are always resolved before transitive dependencies, so the first
  # resolution for a given package name wins.
  #
  # when override is TRUE (called from Remotes processing), allow
  # replacing default repository records with the Remotes-specified
  # source -- e.g. a GitHub remote should win over a CRAN record
  if (exists(package, envir = envir, inherits = FALSE)) {

    if (!override)
      return(character())

    existing <- get(package, envir = envir, inherits = FALSE)
    if (is.null(existing))
      return(character())

    source <- renv_record_source(existing, normalize = TRUE)
    if (!identical(source, "repository"))
      return(character())

  }

  # reserve the slot to prevent re-processing via dependency cycles
  assign(package, NULL, envir = envir)

  # fetch DESCRIPTION-level metadata for this record;
  # renv_graph_description_repository includes cellar via
  # renv_available_packages_latest(cellar = TRUE)
  desc <- catch(renv_graph_description(record))
  if (inherits(desc, "error")) {
    desc <- structure(list(),
      resolution_failed = TRUE,
      resolution_error = conditionMessage(desc)
    )
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
  deps <- renv_graph_deps(desc, fields = fields)

  # resolve any Remotes declared by this package; this pre-populates envir
  # with the correct (e.g. GitHub) resolution so that when the BFS later
  # encounters the bare package name, it finds it already resolved.
  # override = TRUE allows these to replace default repository records
  # that may have already been resolved into envir
  if (config$install.remotes()) {
    remotes <- paste(exclude(desc[["Remotes"]], NA), collapse = ", ")
    specs <- strsplit(remotes, "\\s*,\\s*")[[1L]]
    for (spec in specs) {
      rdeps <- catch(renv_graph_resolve(spec, envir, records = records, override = TRUE))
      if (!inherits(rdeps, "error"))
        deps <- c(deps, rdeps)
    }
  }

  deps

}

renv_graph_description <- function(record) {

  source <- renv_record_source(record, normalize = TRUE)

  case(
    source == "repository"   ~ renv_graph_description_repository(record),
    source == "bioconductor" ~ renv_graph_description_bioconductor(record),
    source == "github"       ~ renv_graph_description_github(record),
    source == "gitlab"       ~ renv_graph_description_gitlab(record),
    source == "bitbucket"    ~ renv_graph_description_bitbucket(record),
    source == "git"          ~ renv_graph_description_git(record),
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

renv_graph_description_git <- function(record) {
  desc <- renv_remotes_resolve_git_description(record)
  as.list(desc)
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
    value <- renv_description_parse_field(desc[[field]])
    deps <- c(deps, value$Package)
  }

  # exclude base packages (R, utils, methods, etc.)
  deps <- setdiff(unique(deps), renv_packages_base())
  deps

}

renv_graph_adjacency <- function(descriptions) {

  packages <- names(descriptions)
  n <- length(packages)

  # build adjacency: for each package, its deps within the set
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

  list(
    packages = packages,
    adj      = adj,
    indegree = indegree,
    revadj   = revadj
  )

}

renv_graph_sort <- function(descriptions) {

  packages <- names(descriptions)
  if (length(packages) == 0L)
    return(descriptions)

  g <- renv_graph_adjacency(descriptions)
  indegree <- g$indegree

  # Kahn's algorithm
  queue <- g$packages[indegree == 0L]
  result <- character()

  while (length(queue) > 0L) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    result <- c(result, current)

    for (dependent in g$revadj[[current]]) {
      indegree[[dependent]] <- indegree[[dependent]] - 1L
      if (indegree[[dependent]] == 0L)
        queue <- c(queue, dependent)
    }
  }

  # handle cycles: append any remaining packages
  remaining <- setdiff(g$packages, result)
  if (length(remaining) > 0L) {
    warningf("dependency cycle detected among: %s", paste(remaining, collapse = ", "))
    result <- c(result, remaining)
  }

  descriptions[result]

}

renv_graph_waves <- function(descriptions) {

  packages <- names(descriptions)
  if (length(packages) == 0L)
    return(list())

  g <- renv_graph_adjacency(descriptions)
  indegree <- g$indegree

  # iterative peeling: each iteration collects all packages with in-degree 0
  waves <- list()
  remaining <- g$packages

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
      for (dependent in g$revadj[[pkg]])
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

renv_graph_scope_retrieve <- function(scope = parent.frame()) {

  # prepare retrieve environment (repos, PPM, user agent);
  # this setup is normally done inside renv_retrieve_impl —
  # we replicate it here so parallel / forked processes inherit these options
  repos <- renv_repos_normalize()
  if (renv_ppm_enabled())
    repos <- renv_ppm_transform(repos)

  agent <- renv_http_useragent()
  if (!grepl("renv", agent))
    agent <- paste(sprintf("renv (%s)", renv_metadata_version()), agent, sep = "; ")

  renv_scope_options(repos = repos, HTTPUserAgent = agent, scope = scope)

}


renv_graph_install <- function(descriptions) {

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

  renv_graph_scope_retrieve()

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

  # staging directory for unpacked packages; scoped to this function
  # so temp files from renv_package_unpack survive until install completes
  staging <- renv_scope_tempfile("renv-graph-staging-")
  ensure_directory(staging)

  # track results, failures, and error messages
  all <- list()
  failed <- stack("character")
  errors <- stack()
  verbose <- config$install.verbose()
  jobs <- config$install.jobs()

  before <- Sys.time()

  writef(header("Downloading packages"))

  # ── Phase 1: Download all packages up front ──────────────────────

  # filter out packages that are already correctly installed;
  # safe to do up front since nothing has been installed yet
  remaining <- Filter(function(pkg) {
    path <- renv_restore_find(pkg, descriptions[[pkg]])
    !nzchar(path)
  }, packages)

  # check cache: packages with a valid cache entry can be installed
  # directly (just a link/copy), skipping download entirely
  hits <- character()
  for (pkg in remaining) {
    desc <- descriptions[[pkg]]
    cacheable <-
      renv_cache_config_enabled(project = project) &&
      renv_record_cacheable(desc) &&
      !renv_restore_rebuild_required(desc)

    if (cacheable) {
      path <- renv_cache_find(desc)
      if (renv_cache_package_validate(path)) {
        renv_install_package_cache(desc, path, linker)
        all[[pkg]] <- desc
        hits <- c(hits, pkg)
      }
    }
  }
  remaining <- setdiff(remaining, hits)

  # download all remaining packages in one parallel batch
  total <- 0
  count <- 0L
  downloaded <- list()

  if (length(remaining) > 0L) {

    urlinfo <- renv_graph_urls(descriptions[remaining])

    downloadable <- Filter(Negate(is.null), urlinfo)
    fallbacks <- setdiff(remaining, names(downloadable))

    dlbefore <- Sys.time()

    streamed <- character()

    if (length(downloadable)) {

      urls      <- vapply(downloadable, `[[`, character(1L), "url")
      destfiles <- vapply(downloadable, `[[`, character(1L), "destfile")
      types     <- vapply(downloadable, `[[`, character(1L), "type")

      ensure_parent_directory(destfiles)

      # build destfile -> package lookup for the callback
      lookup <- names(destfiles)
      names(lookup) <- destfiles

      # stream per-package progress as each download completes;
      # disabled in testing mode to keep output order deterministic
      progress <- !testing()
      awaiting <- names(downloadable)
      maxdl <- 16L

      callback <- if (progress) function(destfile, code, size, time, exitcode, error) {

        pkg <- lookup[[destfile]]
        if (is.null(pkg))
          return()

        renv_graph_status_update_clear()

        desc <- descriptions[[pkg]]
        msg <- sprintf("- Downloading %s %s ... ", desc$Package, desc$Version)
        printf(format(msg, width = the$install_step_width))

        if (exitcode == "0") {
          elapsed <- as.difftime(time, units = "secs")
          writef("OK [%s in %s]", renv_pretty_bytes(size), renv_difftime_format_short(elapsed))
        } else {
          writef("FAILED")
        }

        streamed <<- c(streamed, pkg)
        awaiting <<- setdiff(awaiting, pkg)

        if (length(awaiting) > 0L) {
          active <- head(awaiting, maxdl)
          pending <- max(length(awaiting) - maxdl, 0L)
          renv_graph_status_update("Downloading", active, pending)
        }

        flush(stdout())

      }

      if (progress && length(awaiting) > 1L) {
        active <- head(awaiting, maxdl)
        pending <- max(length(awaiting) - maxdl, 0L)
        renv_graph_status_update("Downloading", active, pending)
      }

      ok <- renv_download_parallel(urls, destfiles, types, callback = callback)

      if (progress)
        renv_graph_status_update_clear()
      for (pkg in names(downloadable)) {
        if (!isTRUE(ok[[pkg]]))
          fallbacks <- c(fallbacks, pkg)
      }

    }

    # sequential fallback for unsupported sources or failed parallel downloads
    for (pkg in fallbacks) {

      status <- catch({
        renv_scope_options(renv.download.headers = NULL)
        renv_scope_options(renv.verbose = FALSE)
        renv_retrieve_impl_one(pkg)
      })

      if (inherits(status, "error")) {
        downloadable[[pkg]] <- NULL
        next
      }

      rec <- renv_restore_state()$install$data()[[pkg]]
      if (is.null(rec))
        next

      downloadable[[pkg]] <- list(
        url      = "",
        destfile = rec$Path,
        type     = renv_record_source(rec, normalize = TRUE)
      )

    }

    dlafter <- Sys.time()

    # build downloaded records and report any packages not already streamed
    for (package in sort(remaining)) {

      info <- downloadable[[package]]
      hasfile <- !is.null(info) && file.exists(info$destfile)

      if (!hasfile) {
        if (!(package %in% streamed))
          writef("- Failed to download '%s'.", package)
        errors$push(list(package = package, message = "failed to download"))
        failed$push(package)
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

      # print progress for packages not already reported by the callback
      if (!(package %in% streamed)) {
        msg <- sprintf("- Downloading %s %s ... ", record$Package, record$Version)
        printf(format(msg, width = the$install_step_width))
        writef("OK")
      }

      downloaded[[package]] <- record

    }

    total <- as.numeric(difftime(dlafter, dlbefore, units = "secs"))
    count <- length(downloaded)

  }

  if (count > 0L) {
    fmt <- "Successfully downloaded %s in %s.\n"
    elapsed <- as.difftime(total, units = "secs")
    writef(fmt, nplural("package", count), renv_difftime_format(elapsed))
  }

  writef(header("Installing packages"))

  # ── Phase 2: Unpack + classify all packages ───────────────────
  # Separating binaries from source up front lets us install all
  # binaries before the wave loop (they're just file copies with no
  # build-time dependency ordering).  This can collapse waves: a
  # source package whose deps are all binary no longer needs to wait.
  # To revert to wave-ordered binary installs, remove Phase 2a/2b
  # and restore the binary branch inside the wave loop.

  binaries <- list()
  sources <- list()

  for (package in sort(setdiff(remaining, failed$data()))) {

    record <- downloaded[[package]]
    if (is.null(record)) {
      failed$push(package)
      next
    }

    # unpack archives and pre-build if needed; unpacked files are
    # moved into 'staging' so they outlive the unpack function's frame
    path <- catch(renv_graph_install_unpack(record, staging))
    if (inherits(path, "error")) {
      msg <- conditionMessage(path)
      writef("- Failed to prepare '%s': %s", package, msg)
      errors$push(list(package = package, message = msg))
      failed$push(package)
      next
    }

    # prepare the package for installation
    prepared <- catch(renv_graph_install_prepare(record, path, installdir))
    if (inherits(prepared, "error")) {
      msg <- conditionMessage(prepared)
      writef("- Failed to prepare '%s': %s", package, msg)
      errors$push(list(package = package, message = msg))
      failed$push(package)
      next
    }

    entry <- list(record = record, prepared = prepared)
    if (identical(prepared$method, "copy"))
      binaries[[package]] <- entry
    else
      sources[[package]] <- entry

  }

  # ── Phase 2a: Install all binaries up front ─────────────────
  # Binary installs are plain file copies with no build-time deps,
  # so they don't need wave ordering.

  for (package in names(binaries)) {

    entry <- binaries[[package]]
    renv_install_step_start("Installing", entry$record, verbose = verbose)
    t0 <- Sys.time()
    result <- catch(renv_graph_install_copy(entry$prepared))
    t1 <- Sys.time()

    if (inherits(result, "error")) {
      msg <- conditionMessage(result)
      writef("FAILED")
      errors$push(list(package = package, message = msg))
      failed$push(package)
      next
    }

    renv_install_step_ok("installed binary", elapsed = difftime(t1, t0, units = "auto"), verbose = verbose)
    renv_graph_install_finalize(entry$record, entry$prepared, installdir, project, linkable)
    all[[package]] <- entry$record

  }

  # ── Phase 2b: Install source packages ─────────────────────
  # Source packages require build-time dependency ordering.
  # On R >= 4.0, a ready-queue event loop (live Kahn's algorithm)
  # launches packages as soon as their dependencies complete,
  # keeping all worker slots busy.  On older R, a wave+batch
  # fallback uses pipe-based collection.

  sourcenames <- setdiff(names(sources), failed$data())
  sourcedescs <- descriptions[intersect(sourcenames, names(descriptions))]

  # shared closure for processing one completed package;
  # callbacks accumulates per-package backup-restore functions
  callbacks <- list()

  handle <- function(pkg, result) {
    entry <- sources[[pkg]]
    installpath <- file.path(installdir, pkg)

    renv_install_step_start("Installing", entry$record, verbose = verbose)

    if (result$success && file.exists(installpath)) {
      renv_install_step_ok("built from source", elapsed = result$elapsed, verbose = verbose)
      renv_graph_install_finalize(entry$record, entry$prepared, installdir, project, linkable)
      all[[pkg]] <<- entry$record
    } else {
      unlink(installpath, recursive = TRUE)
      writef("FAILED")
      if (verbose) writeLines(result$output)
      errors$push(list(package = pkg, message = "install failed", output = result$output))
      failed$push(pkg)
    }

    callbacks[[pkg]]()
  }

  if (getRversion() >= "4.0") {

    # ready-queue event loop (live Kahn's algorithm)
    graph <- renv_graph_adjacency(sourcedescs)
    indegree <- graph$indegree
    revadj <- graph$revadj

    ready <- sort(names(indegree)[indegree == 0L])

    server <- renv_socket_server()
    defer(close(server$socket))
    active <- list()

    showstatus <- !testing() && !verbose

    repeat {

      # fill worker slots from ready queue
      while (length(active) < jobs && length(ready) > 0L) {
        pkg <- ready[1L]
        ready <- ready[-1L]

        callbacks[[pkg]] <- renv_graph_install_backup(installdir, pkg)
        active[[pkg]] <- renv_graph_install_launch_socket(
          sources[[pkg]]$prepared, server$port
        )
      }

      if (length(active) == 0L)
        break

      if (showstatus) {
        pending <- length(ready) + sum(indegree > 0L)
        renv_graph_status_update("Building", names(active), pending)
      }

      # accept one result (blocks until a worker reports back)
      result <- renv_graph_install_accept(server$socket, active, timeout = 3600)

      if (showstatus)
        renv_graph_status_update_clear()

      if (is.null(result)) {
        # timeout or error: mark all active workers as failed
        for (pkg in names(active)) {
          elapsed <- difftime(Sys.time(), active[[pkg]]$start, units = "auto")
          handle(pkg, list(success = FALSE, output = "worker process timed out", elapsed = elapsed))
        }
        active <- list()
        next
      }

      pkg <- result$package
      handle(pkg, result)
      active[[pkg]] <- NULL

      # on success, decrement dependents' indegree and enqueue newly ready
      if (result$success) {
        for (dependent in revadj[[pkg]]) {
          indegree[[dependent]] <- indegree[[dependent]] - 1L
          if (indegree[[dependent]] == 0L)
            ready <- c(ready, dependent)
        }
      }
      # on failure: dependents keep indegree > 0, never enqueued

    }

    # mark packages stuck due to failed dependencies
    stuck <- names(indegree)[indegree > 0L]
    stuck <- setdiff(stuck, failed$data())
    for (pkg in stuck) failed$push(pkg)

  } else {

    # R < 4.0 fallback: wave-based, pipe-based collection
    waves <- renv_graph_waves(sourcedescs)

    for (wave in waves) {

      wave <- setdiff(wave, failed$data())
      if (length(wave) == 0L)
        next

      wave <- sort(wave)
      sourcepkgs <- wave

      while (length(sourcepkgs) > 0L) {

        batch <- head(sourcepkgs, jobs)
        sourcepkgs <- tail(sourcepkgs, -length(batch))

        workers <- list()

        for (pkg in batch)
          callbacks[[pkg]] <- renv_graph_install_backup(installdir, pkg)

        for (pkg in batch)
          workers[[pkg]] <- renv_graph_install_launch(sources[[pkg]]$prepared)

        for (pkg in batch) {
          result <- renv_graph_install_collect(workers[[pkg]])
          handle(pkg, result)
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
  if (staged && length(all) > 0L && !(transactional && !failed$empty())) {
    stagepaths <- file.path(templib, names(all))
    stagepaths <- stagepaths[file.exists(stagepaths)]
    targets <- file.path(library, basename(stagepaths))
    names(targets) <- stagepaths
    enumerate(targets, renv_file_move, overwrite = TRUE)

    # clear filebacked cache entries
    descpaths <- file.path(targets, "DESCRIPTION")
    renv_filebacked_clear("renv_description_read", descpaths)
    renv_filebacked_clear("renv_hash_description", descpaths)
  }

  n <- length(all)
  if (n > 0L) {
    elapsed <- difftime(after, before, units = "secs")
    writef("Successfully installed %s in %s.", nplural("package", n), renv_difftime_format(elapsed))
  }

  # report errors
  renv_graph_install_errors(errors$data(), failed$data(), descriptions)

  invisible(all)

}

renv_graph_install_errors <- function(errors, failed, descriptions) {

  if (length(errors) == 0L && length(failed) == 0L)
    return(invisible())

  # build summary lines for packages that failed directly
  messages <- map_chr(errors, function(item) {
    sprintf("[%s]: %s", item$package, item$message)
  })

  # find packages skipped due to failed dependencies (not in errors themselves)
  errpkgs <- map_chr(errors, `[[`, "package")
  skipped <- setdiff(failed, errpkgs)
  if (length(skipped) > 0L) {
    skipmsgs <- map_chr(skipped, function(pkg) {
      deps <- renv_graph_deps(descriptions[[pkg]])
      deps_failed <- intersect(deps, failed)
      sprintf("[%s]: dependency failed (%s)", pkg, paste(deps_failed, collapse = ", "))
    })
    messages <- c(messages, skipmsgs)
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
    writef(header(item$package))
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

    command <- paste(renv_shell_path(R()), paste(args, collapse = " "))

    list(
      method  = "install",
      command = command,
      path    = path,
      library = library,
      package = package
    )

  }

}

renv_graph_install_launch <- function(prepared) {

  command <- paste(prepared$command, "2>&1")

  list(
    connection = renv_pipe_create(command),
    package    = prepared$package,
    start      = Sys.time()
  )

}

renv_graph_install_collect <- function(worker) {

  conn <- worker$connection

  # readLines blocks until the process exits (EOF on pipe)
  lines <- tryCatch(
    readLines(conn, warn = FALSE),
    error = function(e) character()
  )

  # close() on a pipe connection returns the process exit status invisibly;
  # R emits a warning on non-zero exit, which we suppress
  status <- tryCatch(
    suppressWarnings(close(conn)),
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

renv_graph_install_launch_socket <- function(prepared, port) {

  command <- paste(prepared$command, "2>&1")
  package <- prepared$package

  # build a self-contained R script that runs the install command
  # and reports the result back over a socket (base R only, no renv)
  code <- expr({
    output <- suppressWarnings(
      system(!!command, intern = TRUE, ignore.stderr = FALSE)
    )
    exitcode <- attr(output, "status")
    if (is.null(exitcode)) exitcode <- 0L
    conn <- socketConnection(
      host = "127.0.0.1", port = !!port,
      open = "wb", blocking = TRUE, timeout = 60
    )
    serialize(list(package = !!package, exitcode = exitcode, output = output), conn)
    close(conn)
  })

  script <- tempfile("renv-install-", fileext = ".R")
  writeLines(deparse(code), con = script)

  system2(
    command = R(),
    args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout  = FALSE,
    stderr  = FALSE,
    wait    = FALSE
  )

  list(package = package, start = Sys.time())

}

renv_graph_install_accept <- function(socket, active, timeout = 3600) {

  conn <- tryCatch(
    renv_socket_accept(socket, open = "rb", timeout = timeout),
    error = function(e) NULL
  )

  if (is.null(conn))
    return(NULL)

  data <- tryCatch(unserialize(conn), error = function(e) NULL)
  close(conn)

  if (is.null(data))
    return(NULL)

  pkg <- data$package
  elapsed <- difftime(Sys.time(), active[[pkg]]$start, units = "auto")

  list(
    package = pkg,
    success = identical(as.integer(data$exitcode), 0L),
    output  = data$output,
    elapsed = elapsed
  )

}

renv_graph_install_backup <- function(installdir, pkg) {
  installpath <- file.path(installdir, pkg)
  callback <- renv_file_backup(installpath)
  if (renv_file_broken(installpath))
    renv_file_remove(installpath)
  callback
}

renv_graph_status_update <- function(label, items, pending = 0L) {

  n <- length(items)
  max <- 4L

  detail <- if (n <= max) {
    paste(items, collapse = ", ")
  } else {
    paste0(paste(head(items, max), collapse = ", "), ", ...")
  }

  suffix <- if (pending > 0L)
    sprintf("[%s pending]", pending)
  else
    ""

  body <- sprintf("- %s: (%s)", label, detail)
  width <- the$install_step_width
  msg <- paste0(format(body, width = width), suffix)
  printf("\r%s", format(msg, width = width + nchar(suffix)))
  flush(stdout())

}

renv_graph_status_update_clear <- function() {
  width <- the$install_step_width + 24L
  printf("\r%s\r", strrep(" ", width))
  flush(stdout())
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
