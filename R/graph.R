
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
    renv_dependencies_fields(project = project)
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

  # phase 3: check version requirements; if a resolved package
  # doesn't satisfy constraints from other packages in the graph,
  # try to upgrade it to the latest available version
  descriptions <- as.list(envir)
  requirements <- renv_graph_requirements(descriptions)

  for (package in ls(envir = requirements)) {

    reqs <- requirements[[package]]

    version <- descriptions[[package]]$Version
    if (is.null(version))
      next

    if (renv_graph_compatible(version, reqs))
      next

    latest <- catch(renv_available_packages_latest(package))
    if (inherits(latest, "error"))
      next

    desc <- descriptions[[package]]
    desc$Version <- latest$Version
    descriptions[[package]] <- desc
    assign(package, desc, envir = envir)

  }

  # warn about any remaining unsatisfied version requirements
  renv_graph_requirements_check(descriptions, requirements)

  descriptions

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

  # respect explicitly-requested package type (e.g. install(..., type = "binary"))
  # so that graph resolution finds versions available for the requested type;
  # without this, we always search source packages and may resolve a version
  # that only exists as source when the user asked for binaries
  type <- the$install_pkg_type %||% "source"

  # try available packages entry (returns full fields including Imports, Depends);
  # we need these fields for dependency graph resolution;
  # in strict mode with a URL-valued Repository, search only that repository
  repository <- record[["Repository"]]
  strict <- renv_restore_state(key = "strict") %||% FALSE
  repos <- if (strict && is.character(repository) && grepl("://", repository, fixed = TRUE))
    renv_repos_baseurl(repository)

  entry <- catch(
    renv_available_packages_entry(
      package = package,
      type    = type,
      filter  = version,
      repos   = repos,
      prefer  = record[["Repository"]]
    )
  )

  if (!inherits(entry, "error"))
    return(as.list(entry))

  # try cellar via renv_available_packages_latest (includes cellar = TRUE);
  # cellar packages won't be found by renv_available_packages_entry.
  # NOTE: renv_available_packages_latest only returns limited fields
  # (Package, Version, etc.) — no Depends/Imports/LinkingTo
  latest <- catch(renv_available_packages_latest(package, type = type))
  if (!inherits(latest, "error")) {
    if (is.null(version) || identical(latest$Version, version))
      return(as.list(latest))
  }

  # the requested version wasn't found in configured repos; try crandb for the
  # specific version's dependency fields. this must come before any fallback
  # that reuses the latest entry's fields with an overridden version, since
  # dependency constraints can change between versions (e.g. #2278)
  if (!is.null(version)) {
    crandb <- catch(renv_graph_description_crandb(package, version))
    if (!inherits(crandb, "error"))
      return(as.list(crandb))
  }

  # last-resort fallback when crandb is unreachable: use the latest entry's
  # full fields with the requested version substituted. this may return stale
  # dependency constraints if they changed between versions, but is better
  # than failing outright.
  if (!is.null(version) && !inherits(latest, "error")) {
    full <- catch(renv_available_packages_entry(package, type = type))
    if (!inherits(full, "error")) {
      desc <- as.list(full)
      desc$Version <- version
      return(desc)
    }
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

  parts <- c(if (nzchar(subdir %||% "")) subdir, "DESCRIPTION")
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
  subdir <- record$RemoteSubdir
  ref    <- record$RemoteRef

  # scope authentication
  renv_scope_auth(repo)

  # get DESCRIPTION file
  parts <- c(if (nzchar(subdir %||% "")) subdir, "DESCRIPTION")
  descpath <- paste(parts, collapse = "/")
  fmt <- "%s/repositories/%s/%s/src/%s/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref, descpath)

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

renv_graph_requirements <- function(descriptions) {

  requirements <- new.env(parent = emptyenv())

  fields <- c("Depends", "Imports", "LinkingTo")
  for (desc in descriptions) {
    for (field in fields) {
      parsed <- renv_description_parse_field(desc[[field]])
      if (is.null(parsed))
        next
      explicit <- parsed[nzchar(parsed$Require) & nzchar(parsed$Version), ]
      if (nrow(explicit) == 0L)
        next
      explicit$RequiredBy <- desc$Package
      for (i in seq_len(nrow(explicit))) {
        pkg <- explicit$Package[[i]]
        requirements[[pkg]] <- c(requirements[[pkg]], list(explicit[i, ]))
      }
    }
  }

  # bind each package's requirement rows into a single data frame
  enumerate(as.list(requirements), function(pkg, rows) {
    requirements[[pkg]] <- bind(rows)
  })

  requirements

}

renv_graph_compatible <- function(version, requirements) {

  if (is.null(requirements) || nrow(requirements) == 0L)
    return(TRUE)

  rversion <- numeric_version(version)
  all(map_lgl(seq_len(nrow(requirements)), function(i) {
    expr <- call(requirements$Require[[i]], rversion, requirements$Version[[i]])
    eval(expr, envir = baseenv())
  }))

}

renv_graph_requirements_check <- function(descriptions, requirements) {

  if (!renv_verbose())
    return(invisible())

  messages <- character()
  for (package in ls(envir = requirements)) {

    reqs <- requirements[[package]]

    version <- descriptions[[package]]$Version
    if (is.null(version))
      next

    if (renv_graph_compatible(version, reqs))
      next

    # find which requirements are unsatisfied
    rversion <- numeric_version(version)
    for (i in seq_len(nrow(reqs))) {
      expr <- call(reqs$Require[[i]], rversion, reqs$Version[[i]])
      if (!eval(expr, envir = baseenv())) {
        fmt <- "'%s' requires '%s %s %s', but '%s %s' will be installed"
        msg <- sprintf(fmt,
          reqs$RequiredBy[[i]],
          package, reqs$Require[[i]], reqs$Version[[i]],
          package, version
        )
        messages <- c(messages, msg)
      }
    }

  }

  if (empty(messages))
    return()

  bulletin(
    "The following issues were discovered while preparing for installation:",
    messages,
    "Installation of these packages may not succeed."
  )

}

# Tree printing ----

renv_graph_print <- function(records, roots, fields) {

  if (!renv_verbose())
    return(invisible())

  # keep only roots that exist in the records
  roots <- csort(intersect(roots, names(records)))
  if (empty(roots))
    return(invisible())

  # print a virtual root with top-level packages as children
  writef(".")
  renv_graph_print_impl(roots, records, fields, prefix = "")

  invisible()

}

renv_graph_print_impl <- function(children, records, fields, prefix) {

  n <- length(children)
  for (i in seq_len(n)) {

    child <- children[[i]]
    record <- records[[child]]
    label <- renv_record_format_short(record)
    last <- i == n

    connector <- if (last) "\u2514\u2500 " else "\u251c\u2500 "
    writef("%s%s%s %s", prefix, connector, child, label)

    extension <- if (last) "   " else "\u2502  "
    subdeps <- renv_graph_print_children(record, records, fields)
    renv_graph_print_impl(subdeps, records, fields, paste0(prefix, extension))

  }

}

renv_graph_print_children <- function(record, records, fields) {

  deps <- character()
  for (field in fields) {
    value <- record[[field]]
    if (is.null(value))
      next
    # extract package names, stripping version constraints
    pkgs <- sub("\\s*\\(.*", "", unlist(value))
    deps <- c(deps, pkgs)
  }

  # keep only packages present in the lockfile, sorted
  csort(intersect(unique(deps), names(records)))

}

# compute the root packages in a set of records: packages that are
# not dependencies of any other package in the set
renv_graph_roots <- function(records, fields) {

  deps <- character()
  for (record in records)
    deps <- c(deps, renv_graph_print_children(record, records, fields))

  roots <- setdiff(names(records), unique(deps))

  # if every package is a dep of something else (unlikely), fall back to all
  if (empty(roots))
    roots <- names(records)

  csort(roots)

}

renv_graph_needs_update <- function(pkg, record, requirements) {

  # check whether the resolved version is already installed
  path <- renv_restore_find(pkg, record)
  if (nzchar(path))
    return(FALSE)

  # for transitive dependencies that are already installed, check
  # whether the installed version satisfies dependency requirements;
  # this avoids upgrading dependencies during install() when the
  # existing library version is already compatible.
  # explicitly-requested packages (in state$packages) always get
  # installed, so skip this check for those.
  state <- renv_restore_state()
  if (!(pkg %in% state$packages)) {
    installed <- renv_package_version(pkg)
    if (!is.null(installed)) {
      reqs <- requirements[[pkg]]
      if (renv_graph_compatible(installed, reqs))
        return(FALSE)
    }
  }

  TRUE

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

    # resolve download URL; on error the package falls through
    # to sequential retrieval which handles its own error reporting
    result[[package]] <- tryCatch(
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
      error = function(cnd) NULL
    )

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
  #
  # include custom repository URL (if any) so packages only
  # available on non-standard repositories can be found;
  # in strict mode, use only the recorded URL
  repository <- desc[["Repository"]]
  strict <- renv_restore_state(key = "strict") %||% FALSE
  repos <- if (is.character(repository) && grepl("://", repository, fixed = TRUE)) {
    baseurl <- renv_repos_baseurl(repository)
    current <- getOption("repos")
    matches <- any(renv_repos_matches(baseurl, current))
    augmented <- when(matches, current, c(baseurl, current))
    when(strict, baseurl, augmented)
  }

  record <- catch(renv_available_packages_latest(package, repos = repos))
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
  baseurl <- renv_repos_baseurl(attr(record, "url", exact = TRUE))
  root <- renv_retrieve_repos_archive_root(baseurl, as.list(desc))

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
  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "FALSE")
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
  timer <- timer()

  progress <- spinner("", 0L)
  defer(progress$restore())

  # ── Phase 1: Download all packages up front ──────────────────────

  # filter out packages that are already correctly installed;
  # safe to do up front since nothing has been installed yet
  requirements <- renv_graph_requirements(descriptions)
  remaining <- Filter(function(pkg) {
    renv_graph_needs_update(pkg, descriptions[[pkg]], requirements)
  }, packages)

  # check cache: packages with a valid cache entry can be installed
  # directly (just a link/copy), skipping download entirely;
  # defer the actual install until the "Installing packages" phase
  # so the output appears under that header
  cachehits <- list()
  for (pkg in remaining) {
    desc <- descriptions[[pkg]]
    cacheable <-
      renv_cache_config_enabled(project = project) &&
      renv_record_cacheable(desc) &&
      !renv_restore_rebuild_required(desc)

    if (cacheable) {
      path <- renv_cache_find(desc)
      if (renv_cache_package_validate(path)) {
        cachehits[[pkg]] <- list(record = desc, cache = path)
      }
    }
  }
  remaining <- setdiff(remaining, names(cachehits))

  # download all remaining packages in one parallel batch
  count <- 0L
  downloaded <- list()

  if (length(remaining) > 0L) {

    writef(header("Downloading packages"))

    urlinfo <- renv_graph_urls(descriptions[remaining])

    downloadable <- Filter(Negate(is.null), urlinfo)
    fallbacks <- setdiff(remaining, names(downloadable))

    timer$tick()
    streamed <- character()

    if (length(downloadable)) {

      urls      <- vapply(downloadable, `[[`, character(1L), "url")
      destfiles <- vapply(downloadable, `[[`, character(1L), "destfile")
      types     <- vapply(downloadable, `[[`, character(1L), "type")

      # normalize to forward slashes so lookup keys match curl's %{filename_effective}
      # output on all platforms (renv_download_parallel does the same normalization)
      destfiles <- chartr("\\", "/", destfiles)

      ensure_parent_directory(destfiles)

      # build destfile -> package lookup for the callback
      lookup <- names(destfiles)
      names(lookup) <- destfiles

      # stream per-package progress as each download completes;
      # disabled in testing mode to keep output order deterministic
      showprogress <- !testing() && !ci()
      awaiting <- names(downloadable)

      # define a callback to be invoked upon completion of each download
      callback <- function(destfile, code, size, time, exitcode, error) {

        pkg <- lookup[[destfile]]
        if (is.null(pkg))
          return()

        if (showprogress) progress$clear()
        if (showprogress) progress$tick()

        desc <- descriptions[[pkg]]
        pkgver <- paste(desc$Package, desc$Version)

        if (exitcode == "0") {
          elapsed <- as.difftime(time, units = "secs")
          status <- sprintf("%s in %s", renv_pretty_bytes(size), renv_difftime_format_short(elapsed))
          writef("%s %s [%s]", yay(), format(pkgver, width = the$install_step_width), status)
        } else {
          writef("%s %s", boo(), format(pkgver, width = the$install_step_width))
        }

        streamed <<- c(streamed, pkg)
        awaiting <<- setdiff(awaiting, pkg)

        if (showprogress && length(awaiting) > 0L)
          progress$update(awaiting)

        flush(stdout())

      }

      # notify that we're about to download some packages
      if (showprogress) {
        progress$reset("Downloading", length(awaiting))
        if (length(awaiting))
          progress$update(awaiting)
      }

      # perform the actual download
      ok <- renv_download_parallel(
        urls      = urls,
        destfiles = destfiles,
        types     = types,
        callback  = if (!testing()) callback
      )

      # clear up and determine what packages need 'fallback' downloads
      if (showprogress)
        progress$clear()
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

    # build downloaded records and report any packages not already streamed
    for (package in sort(remaining)) {

      info <- downloadable[[package]]
      hasfile <- !is.null(info) && file.exists(info$destfile)

      if (!hasfile) {
        if (!(package %in% streamed)) {
          desc <- descriptions[[package]]
          pkgver <- paste(desc$Package, desc$Version)
          writef("%s %s", boo(), format(pkgver, width = the$install_step_width))
        }
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
        pkgver <- paste(record$Package, record$Version)
        writef("%s %s", yay(), format(pkgver, width = the$install_step_width))
      }

      downloaded[[package]] <- record

    }

    count <- length(downloaded)

  }

  if (count > 0L) {
    fmt <- "Successfully downloaded %s in %s.\n"
    elapsed <- timer$tick()
    writef(fmt, nplural("package", count), renv_difftime_format(elapsed))
  }

  writef(header("Installing packages"))
  timer$tick()

  # install packages from the cache
  for (pkg in names(cachehits)) {
    entry <- cachehits[[pkg]]
    renv_install_package_cache(entry$record, entry$cache, linker)
    all[[pkg]] <- entry$record
  }

  # ── Phase 2: Classify and install all packages ───────────────
  # Both binary and source packages participate in the same
  # dependency-ordered installation.  The only difference is the
  # install method: binaries are file copies / extractions done
  # synchronously; source packages are built via R CMD INSTALL
  # in worker subprocesses.

  entries <- list()

  for (package in sort(setdiff(remaining, failed$data()))) {

    record <- downloaded[[package]]
    if (is.null(record)) {
      failed$push(package)
      next
    }

    type <- renv_graph_install_classify(record)
    entries[[package]] <- list(record = record, type = type)

  }

  showstatus <- !testing() && !verbose && !ci()

  installnames <- setdiff(names(entries), failed$data())
  installdescs <- descriptions[intersect(installnames, names(descriptions))]

  # shared closure for processing one completed package;
  # callbacks accumulates per-package backup-restore functions
  callbacks <- list()

  handle <- function(pkg, result) {
    entry <- entries[[pkg]]
    installpath <- file.path(installdir, pkg)
    isbinary <- identical(entry$type, "binary")
    message <- if (isbinary) "installed binary" else "built from source"

    if (result$success && file.exists(installpath)) {
      renv_install_step_ok(message, entry$record, elapsed = result$elapsed)
      renv_graph_install_finalize(entry$record, entry$prepared, installdir, project, linkable)
      all[[pkg]] <<- entry$record
    } else {
      unlink(installpath, recursive = TRUE)
      renv_install_step_error(entry$record)
      if (verbose) writeLines(result$output)
      errors$push(list(package = pkg, message = "install failed", output = result$output))
      failed$push(pkg)
    }

    callbacks[[pkg]]()
  }

  if (getRversion() >= "4.0") {

    # ready-queue event loop (live Kahn's algorithm)
    graph <- renv_graph_adjacency(installdescs)
    indegree <- graph$indegree
    revadj <- graph$revadj

    ready <- sort(names(indegree)[indegree == 0L])

    server <- renv_socket_server()
    defer(close(server$socket))
    active <- list()

    if (showstatus)
      progress$reset("Installing", length(installnames))

    timeout <- getOption("renv.install.timeout", default = 3600L)
    deadline <- Sys.time() + timeout

    repeat {

      # fill worker slots from ready queue
      while (length(ready) > 0L) {
        pkg <- ready[1L]

        entry <- entries[[pkg]]

        # unpack (if needed) and prepare
        prepared <- catch(renv_graph_install_unpack_and_prepare(
          entry$record, entry$type, staging, installdir
        ))
        if (inherits(prepared, "error")) {
          ready <- ready[-1L]
          msg <- conditionMessage(prepared)
          if (showstatus) progress$clear()
          writef("- Failed to prepare '%s': %s", pkg, msg)
          errors$push(list(package = pkg, message = msg))
          failed$push(pkg)
          if (showstatus) progress$tick()
          next
        }

        entry$prepared <- prepared
        entries[[pkg]] <- entry

        callbacks[[pkg]] <- renv_graph_install_backup(installdir, pkg)

        if (identical(entry$type, "binary")) {

          # binary packages are just file copies -- install synchronously
          ready <- ready[-1L]

          t0 <- Sys.time()
          result <- catch(renv_graph_install_binary(prepared))
          t1 <- Sys.time()
          elapsed <- difftime(t1, t0, units = "auto")

          if (showstatus) {
            progress$clear()
            progress$tick()
          }

          success <- !inherits(result, "error")
          output <- if (success) character() else conditionMessage(result)
          handle(pkg, list(success = success, output = output, elapsed = elapsed))

          # update indegrees for dependents
          if (success) {
            for (dependent in revadj[[pkg]]) {
              indegree[[dependent]] <- indegree[[dependent]] - 1L
              if (indegree[[dependent]] == 0L)
                ready <- c(ready, dependent)
            }
          }

        } else {

          # source packages need a worker slot
          if (length(active) >= jobs)
            break

          ready <- ready[-1L]
          active[[pkg]] <- renv_graph_install_launch_socket(
            prepared, server$port
          )

        }

      }

      if (length(active) == 0L && length(ready) == 0L)
        break

      # if only ready (binary) packages remain, skip socketSelect
      if (length(active) == 0L)
        next

      if (showstatus)
        progress$update(names(active))

      # collect connected worker sockets into a named list
      conns <- list()
      for (pkg in names(active))
        if (!is.null(active[[pkg]]$conn))
          conns[[pkg]] <- active[[pkg]]$conn

      remaining <- as.double(deadline - Sys.time(), units = "secs")
      if (remaining <= 0) {
        for (pkg in names(active)) {
          elapsed <- difftime(
            Sys.time(), active[[pkg]]$start, units = "auto"
          )
          handle(pkg, list(
            success = FALSE,
            output  = "worker process timed out",
            elapsed = elapsed
          ))
          renv_graph_install_worker_cleanup(active[[pkg]])
        }
        active <- list()
        break
      }

      # wait for activity on server socket (new connections) or
      # worker sockets (results / EOF); server is always first
      wait <- min(5, remaining)
      sockets <- c(list(server$socket), conns)
      flags <- socketSelect(sockets, write = FALSE, timeout = wait)

      if (!any(flags)) {
        # check for workers that never connected (startup timeout)
        for (pkg in names(active)) {
          if (!is.null(active[[pkg]]$conn))
            next
          elapsed <- as.double(
            difftime(Sys.time(), active[[pkg]]$start, units = "secs")
          )
          if (elapsed > 60) {
            if (showstatus) {
              progress$clear()
              progress$tick()
            }
            handle(pkg, list(
              success = FALSE,
              output  = "worker failed to start",
              elapsed = as.difftime(elapsed, units = "secs")
            ))
            renv_graph_install_worker_cleanup(active[[pkg]])
            active[[pkg]] <- NULL
            if (showstatus && length(active) > 0L)
              progress$update(names(active))
          }
        }
        next
      }

      # handle new worker connections (server socket is first);
      # errors here are transient -- the startup timeout (60s)
      # handles workers that genuinely fail to connect
      if (flags[[1L]]) {
        conn <- tryCatch(
          socketAccept(
            server$socket,
            open     = "rb",
            blocking = TRUE,
            timeout  = 30
          ),
          error = identity
        )
        if (!inherits(conn, "error")) {
          pkg <- tryCatch(unserialize(conn), error = identity)
          valid <- is.character(pkg) && (pkg %in% names(active))
          if (valid) {
            active[[pkg]]$conn <- conn
          } else {
            close(conn)
          }
        }
      }

      # handle worker results (flags[-1L] corresponds to conns)
      wflags <- flags[-1L]
      for (pkg in names(conns)[wflags]) {

        if (is.null(active[[pkg]]))
          next

        data <- tryCatch(
          unserialize(active[[pkg]]$conn),
          error = identity
        )

        elapsed <- difftime(
          time1 = Sys.time(),
          time2 = active[[pkg]]$start,
          units = "auto"
        )

        if (showstatus) {
          progress$clear()
          progress$tick()
        }

        result <- renv_graph_install_parse_result(data, elapsed)

        handle(pkg, result)
        renv_graph_install_worker_cleanup(active[[pkg]])
        active[[pkg]] <- NULL

        # decrement dependents' indegree and enqueue newly ready
        if (result$success) {
          for (dependent in revadj[[pkg]]) {
            indegree[[dependent]] <- indegree[[dependent]] - 1L
            if (indegree[[dependent]] == 0L)
              ready <- c(ready, dependent)
          }
        }
        # on failure: dependents keep indegree > 0, never enqueued

        if (showstatus && length(active) > 0L)
          progress$update(names(active))

      }

    }

    # mark packages stuck due to failed dependencies
    stuck <- names(indegree)[indegree > 0L]
    stuck <- setdiff(stuck, failed$data())
    for (pkg in stuck) failed$push(pkg)

  } else {

    # R < 4.0 fallback: wave-based, pipe-based collection
    waves <- renv_graph_waves(installdescs)

    remaining <- installnames

    if (showstatus && length(remaining) > 0L) {
      progress$reset("Installing", length(remaining))
      progress$update(remaining)
    }

    for (wave in waves) {

      wave <- setdiff(wave, failed$data())
      if (length(wave) == 0L)
        next

      wave <- sort(wave)

      # install binary packages in this wave synchronously first
      for (pkg in wave) {
        entry <- entries[[pkg]]
        if (!identical(entry$type, "binary"))
          next

        prepared <- catch(renv_graph_install_unpack_and_prepare(
          entry$record, "binary", staging, installdir
        ))
        if (inherits(prepared, "error")) {
          msg <- conditionMessage(prepared)
          if (showstatus) progress$clear()
          writef("- Failed to prepare '%s': %s", pkg, msg)
          errors$push(list(package = pkg, message = msg))
          failed$push(pkg)
          if (showstatus) progress$tick()
          remaining <- setdiff(remaining, pkg)
          if (showstatus && length(remaining) > 0L)
            progress$update(remaining)
          next
        }

        entry$prepared <- prepared
        entries[[pkg]] <- entry

        callbacks[[pkg]] <- renv_graph_install_backup(installdir, pkg)

        t0 <- Sys.time()
        result <- catch(renv_graph_install_binary(prepared))
        t1 <- Sys.time()
        elapsed <- difftime(t1, t0, units = "auto")

        if (showstatus) {
          progress$clear()
          progress$tick()
        }

        success <- !inherits(result, "error")
        output <- if (success) character() else conditionMessage(result)
        handle(pkg, list(success = success, output = output, elapsed = elapsed))
        remaining <- setdiff(remaining, pkg)
        if (showstatus && length(remaining) > 0L)
          progress$update(remaining)
      }

      # install source packages in this wave via worker processes
      sourcepkgs <- Filter(function(pkg) {
        entry <- entries[[pkg]]
        identical(entry$type, "source") && !(pkg %in% failed$data())
      }, wave)

      while (length(sourcepkgs) > 0L) {

        batch <- head(sourcepkgs, jobs)
        sourcepkgs <- tail(sourcepkgs, -length(batch))

        workers <- list()

        for (pkg in batch) {

          entry <- entries[[pkg]]
          prepared <- catch(renv_graph_install_unpack_and_prepare(
            entry$record, "source", staging, installdir
          ))
          if (inherits(prepared, "error")) {
            msg <- conditionMessage(prepared)
            if (showstatus) progress$clear()
            writef("- Failed to prepare '%s': %s", pkg, msg)
            errors$push(list(package = pkg, message = msg))
            failed$push(pkg)
            if (showstatus) progress$tick()
            remaining <- setdiff(remaining, pkg)
            if (showstatus && length(remaining) > 0L)
              progress$update(remaining)
            next
          }

          entry$prepared <- prepared
          entries[[pkg]] <- entry

          callbacks[[pkg]] <- renv_graph_install_backup(installdir, pkg)
          workers[[pkg]] <- renv_graph_install_launch(prepared)

        }

        for (pkg in names(workers)) {
          result <- renv_graph_install_collect(workers[[pkg]])
          if (showstatus) {
            progress$clear()
            progress$tick()
          }
          handle(pkg, result)
          remaining <- setdiff(remaining, pkg)
          if (showstatus && length(remaining) > 0L)
            progress$update(remaining)
        }

      }

    }

  }

  # migrate staged packages into real library;
  # if transactional and there were failures, skip migration entirely
  # (the staging directory is cleaned up by defer, leaving the real
  # library unchanged for a clean rollback)
  trash <- NULL
  transactional <- config$install.transactional()
  if (staged && length(all) > 0L && !(transactional && !failed$empty())) {

    stagepaths <- file.path(templib, names(all))
    stagepaths <- stagepaths[file.exists(stagepaths)]
    targets <- file.path(library, basename(stagepaths))

    # move old installations into a single trash directory so they
    # can be cleaned up in one pass after reporting success, rather
    # than running a recursive unlink per package during migration
    trash <- tempfile("renv-trash-", tmpdir = library)
    dir.create(trash)

    for (target in targets) {
      if (renv_file_exists(target)) {
        ok <- file.rename(target, file.path(trash, basename(target)))
        if (!ok) unlink(target, recursive = TRUE)
      } else if (renv_file_broken(target)) {
        renv_file_remove(target)
      }
    }

    names(targets) <- stagepaths
    enumerate(targets, renv_file_move)

    # clear filebacked cache entries
    descpaths <- file.path(targets, "DESCRIPTION")
    renv_filebacked_clear("renv_description_read", descpaths)
    renv_filebacked_clear("renv_hash_description", descpaths)
  }

  n <- length(all)
  if (n > 0L) {
    fmt <- "Successfully installed %s in %s."
    elapsed <- timer$tick()
    writef(fmt, nplural("package", n), renv_difftime_format(elapsed))
  }

  # clean up old installations after reporting success
  if (!is.null(trash))
    unlink(trash, recursive = TRUE)

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
    writef(output)
    writef("")

  }

  invisible()

}

renv_graph_install_classify <- function(record) {

  # always inspect the archive or directory directly. The type attr
  # on a repository record reflects the `type` argument passed to
  # `available.packages()`, not the real contents of the downloaded
  # file -- which is wrong for Posit Package Manager "binary"
  # repositories, which serve binary packages at source-style URLs.
  renv_package_type(record$Path, quiet = TRUE)

}

renv_graph_install_needs_unpack <- function(record, type) {

  # RemoteSubdir means the package lives in a subdirectory of the
  # archive; we must extract to reach it
  subdir <- record$RemoteSubdir %||% ""
  if (nzchar(subdir))
    return(TRUE)

  # R CMD INSTALL only handles .tar.gz / .tgz archives;
  # other formats (e.g. .zip) must be unpacked first
  archtype <- renv_archive_type(record$Path)
  if (!identical(archtype, "tar"))
    return(TRUE)

  # install.build requires an unpacked directory for R CMD build;
  # only relevant for source packages (binaries are already built)
  identical(type, "source") && identical(config$install.build(), TRUE)

}

renv_graph_install_unpack_and_prepare <- function(record, type, staging, library) {

  package <- record$Package
  path <- record$Path
  isarchive <- identical(renv_file_info(path)$isdir, FALSE)

  # archives can often be used directly: R CMD INSTALL handles source
  # archives natively, and binary archives can be extracted straight
  # into the library.  We only need to unpack up front when the
  # package is nested (RemoteSubdir) or needs pre-building.
  if (isarchive && !renv_graph_install_needs_unpack(record, type)) {
    return(renv_graph_install_prepare(record, path, library, type, isarchive = TRUE))
  }

  # unpack archive or adjust path for RemoteSubdir
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
  path <- renv_path_normalize(path, mustWork = TRUE)

  renv_graph_install_prepare(record, path, library, type, isarchive = FALSE)

}

renv_graph_install_prepare <- function(record, path, library, type, isarchive) {

  package <- record$Package

  if (identical(type, "binary")) {

    # binary directory: copy into library
    # binary archive: extract directly into library
    list(
      type    = "binary",
      method  = if (isarchive) "extract" else "copy",
      path    = path,
      library = library,
      package = package
    )

  } else {

    # R CMD INSTALL handles both archives and directories
    args <- c(
      "--vanilla",
      "CMD", "INSTALL", "--preclean", "--no-multiarch",
      if (config$install.keep.source()) "--with-keep.source" else "--without-keep.source",
      r_cmd_install_option(package, "configure.args", TRUE),
      r_cmd_install_option(package, "configure.vars", TRUE),
      r_cmd_install_option(package, c("install.opts", "INSTALL_opts"), FALSE),
      "-l", renv_shell_path(library),
      renv_shell_path(path)
    )

    command <- paste(renv_shell_path(R()), paste(args, collapse = " "))

    list(
      type    = "source",
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

  # readLines blocks until the process exits (EOF on pipe);
  # if the pipe breaks, the exit status from close() is the real signal
  lines <- tryCatch(readLines(conn, warn = FALSE), error = identity)
  if (inherits(lines, "error"))
    lines <- character()

  # close() on a pipe returns the process status: 0 on success,
  # non-zero on failure (encoding is platform-specific)
  status <- close(conn) %||% 0L
  elapsed <- difftime(Sys.time(), worker$start, units = "auto")

  list(
    success = identical(status, 0L),
    output  = lines,
    elapsed = elapsed
  )

}

renv_graph_install_launch_socket <- function(prepared, port) {

  command <- paste(prepared$command, "2>&1")
  package <- prepared$package

  # build a self-contained R script that:
  # 1. connects to the parent immediately and sends the package name (hello)
  # 2. runs the install command
  # 3. sends the result on the same connection
  #
  # the persistent connection lets the parent detect worker death via
  # socketSelect() + EOF, without needing PID files or polling
  code <- expr({

    main <- function() {

      # connect to socket server created by parent
      conn <- socketConnection(
        host     = "127.0.0.1",
        port     = !!port,
        open     = "wb",
        blocking = TRUE,
        timeout  = 60
      )
      on.exit(close(conn), add = TRUE)

      # notify parent of the package we're installing
      serialize(!!package, conn)

      # run the install command and send the result back;
      # on success 'output' is a character vector with a "status"
      # attribute; on error it's a condition object
      output <- tryCatch(
        suppressWarnings(system(!!command, intern = TRUE)),
        error = identity
      )

      serialize(output, conn)

    }

    main()

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

  list(package = package, start = Sys.time(), conn = NULL, script = script)

}

renv_graph_install_worker_cleanup <- function(worker) {
  try(close(worker$conn), silent = TRUE)
  unlink(worker$script)
}

renv_graph_install_parse_result <- function(data, elapsed) {

  # NULL means the worker died before sending a result
  if (is.null(data)) {
    return(list(
      success = FALSE,
      output  = "worker process exited unexpectedly",
      elapsed = elapsed
    ))
  }

  # an error object means system() itself failed
  if (inherits(data, "error")) {
    return(list(
      success = FALSE,
      output  = conditionMessage(data),
      elapsed = elapsed
    ))
  }

  # otherwise, data is the character vector from system(intern = TRUE);
  # the exit code lives in attr(data, "status")
  status <- as.integer(attr(data, "status") %||% 0L)

  list(
    success = identical(status, 0L),
    output  = data,
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

renv_graph_install_binary <- function(prepared) {

  package <- prepared$package
  library <- prepared$library
  installpath <- file.path(library, package)

  if (identical(prepared$method, "copy"))
    renv_file_copy(prepared$path, installpath, overwrite = TRUE)
  else
    renv_archive_decompress(prepared$path, exdir = library)

  invisible(installpath)

}

renv_graph_install_finalize <- function(record, prepared, library, project, linkable) {

  package <- record$Package
  installpath <- file.path(library, package)

  # test loading; R CMD INSTALL already tests source packages,
  # so we only need this for binaries (copy / extract)
  if (identical(prepared$type, "binary")) {
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
