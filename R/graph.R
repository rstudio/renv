
#' Generate a Package Dependency Graph
#'
#' Generate a package dependency graph.
#'
#' @inheritParams renv-params
#'
#' @param root The top-most package dependencies of interest in the dependency graph.
#'
#' @param leaf The bottom-most package dependencies of interest in the dependency graph.
#'
#' @param suggests Should suggested packages be included within
#'   the dependency graph?
#'
#' @param enhances Should enhanced packages be included within
#'   the dependency graph?
#'
#' @param resolver An \R function accepting a package name, and returning the
#'   contents of its `DESCRIPTION` file (as an \R `data.frame` or `list`).
#'   When `NULL` (the default), an internal resolver is used.
#'
#' @param renderer Which package should be used to render the resulting graph?
#'
#' @param attributes An \R list of graphViz attributes, mapping node names to
#'   attribute key-value pairs. For example, to ask graphViz to prefer orienting
#'   the graph from left to right, you can use
#'   `list(graph = c(rankdir = "LR"))`. See <https://graphviz.org/doc/info/attrs.html>
#'   for a full list of the attributes supported by `graphViz`.
#'
#' @examples
#'
#' \dontrun{
#' # graph the relationship between devtools and rlang
#' graph(root = "devtools", leaf = "rlang")
#'
#' # figure out why a project depends on 'askpass'
#' graph(leaf = "askpass")
#' }
#'
#' @keywords internal
graph <- function(root = NULL,
                  leaf = NULL,
                  ...,
                  suggests   = FALSE,
                  enhances   = FALSE,
                  resolver   = NULL,
                  renderer   = c("DiagrammeR", "visNetwork"),
                  attributes = list(),
                  project    = NULL)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  # figure out packages to try and read
  root <- root %||% renv_graph_roots(project)

  # resolve fields
  fields <- c(
    "Depends", "Imports", "LinkingTo",
    if (suggests) "Suggests",
    if (enhances) "Enhances"
  )

  # resolve renderer
  renderer <- renv_graph_renderer(renderer)

  # find dependencies
  envir <- new.env(parent = emptyenv())
  revdeps <- new.env(parent = emptyenv())
  for (package in root)
    renv_graph_build(package, fields, resolver, envir, revdeps)

  # prune the tree
  tree <- renv_graph_prune(root, leaf, envir, revdeps)

  # compute the graph
  graph <- enumerate(tree, function(package, dependencies) {

    enumerate(dependencies, function(field, packages) {
      attrs <- renv_graphviz_attrs(field, renderer)
      renv_graphviz_edge(package, packages, attrs)
    })

  })

  # figure out which packages remain part of the graph after pruning
  ok <- map_lgl(graph, function(items) {
    any(map_int(items, length) > 0)
  })

  remaining <- intersect(root, names(graph)[ok])
  if (empty(remaining)) {
    fmt <- "- Could not find any relationship between the requested packages."
    writef(fmt)
    return(invisible(NULL))
  }

  defaults <- renv_graphviz_defaults(renderer)
  attributes <- overlay(defaults, attributes)

  # render attributes
  attrtext <- renv_graphviz_render(attributes, TRUE)

  # fill package names which are top-level dependencies
  topattrs <- renv_graphviz_render(
    map(named(remaining), function(name) {
      list(
        style     = "filled",
        fillcolor = "#b3cde3"
      )
    }),
    asis = FALSE
  )

  botattrs <- renv_graphviz_render(
    map(named(leaf), function(name) {
      list(
        style     = "filled",
        fillcolor = "#ccebc5"
      )
    }),
    asis = FALSE
  )

  # collapse into text
  parts <- c(
    'digraph {', '',
    attrtext, '',
    topattrs, '',
    botattrs, '',
    unlist(graph), '',
    '}'
  )

  diagram <- paste(parts, collapse = "\n")

  renderer <- case(

    identical(renderer, "DiagrammeR") ~ function(dot) {
      DiagrammeR <- renv_namespace_load("DiagrammeR")
      DiagrammeR$grViz(diagram = dot)
    },

    identical(renderer, "visNetwork") ~ function(dot) {

      visNetwork <- renv_namespace_load("visNetwork")
      graph <- visNetwork$visNetwork(dot = dot)

      graph$x$options$edges$font$background <- "white"

      # TODO: allow hierarchical layout via option?
      # graph$x$options$layout = list(
      #   hierarchical = list(
      #     blockShifting   = TRUE,
      #     levelSeparation = 50,
      #     nodeSpacing     = 1,
      #     shakeTowards    = "roots",
      #     sortMethod      = "directed"
      #   )
      # )

      graph

    },

    is.function(renderer) ~ renderer,

    ~ stop("unrecognized renderer")

  )

  renderer(diagram)
}

renv_graph_build <- function(package, fields, resolver, envir, revdeps) {

  # check if we've already scanned this package
  if (exists(package, envir = envir))
    return()

  # read package dependencies
  deps <- renv_graph_dependencies(package, fields, resolver)

  # add dependencies to graph
  assign(package, deps, envir = envir)

  # recurse
  children <- sort(unique(unlist(deps)))
  for (child in children) {
    assign(child, c(package, revdeps[[child]]), envir = revdeps)
    renv_graph_build(child, fields, resolver, envir, revdeps)
  }

}

renv_graph_revdeps <- function(packages, revdeps) {

  envir <- new.env(parent = emptyenv())
  for (package in packages)
    renv_graph_revdeps_impl(package, envir, revdeps)

  ls(envir = envir)

}

renv_graph_revdeps_impl <- function(package, envir, revdeps) {

  if (visited(package, envir))
    return()

  for (child in revdeps[[package]])
    renv_graph_revdeps_impl(child, envir, revdeps)

}

renv_graph_roots <- function(project) {

  deps <- renv_dependencies_impl(project, errors = "ignored")
  sort(unique(deps$Package))

}

renv_graph_dependencies <- function(package, fields, resolver) {

  base <- installed_packages(priority = "base")

  desc <- local({

    # try using the resolver if supplied
    if (!is.null(resolver)) {
      desc <- catch(resolver(package))
      if (inherits(desc, "error"))
        warning(desc)
      else if (!is.null(desc))
        return(desc)
    }

    # check for (and prefer) a locally-installed package
    path <- renv_package_find(package)
    if (nzchar(path))
      return(renv_description_read(path))

    # otherwise, try and see if this is a known CRAN package
    as.list(renv_available_packages_entry(package))

  })

  # parse the fields
  values <- map(fields, function(field) {

    item <- desc[[field]]
    if (is.null(item))
      return(NULL)

    parsed <- renv_description_parse_field(item)
    packages <- parsed$Package

    setdiff(packages, c("R", base$Package))

  })

  names(values) <- fields
  values

}

renv_graph_prune <- function(root, leaf, envir, revdeps) {

  # grab all computed dependencies
  all <- as.list(envir)

  # if we don't have any leaves, then just return everything
  if (empty(leaf))
    return(all)

  # otherwise, find recursive dependencies of the requested packages
  rrd <- renv_graph_revdeps(leaf, revdeps)
  map(all, function(children) {
    map(children, intersect, rrd)
  })

}

renv_graphviz_node <- function(nodes, asis, attrs) {

  keys <- names(attrs)
  vals <- renv_json_quote(attrs)
  attrtext <- paste(keys, vals, sep = "=", collapse = ", ")

  fmt <- if (asis) '%s [%s]' else '"%s" [%s]'
  sprintf(fmt, nodes, attrtext)

}

renv_graphviz_edge <- function(lhs, rhs, attrs) {

  if (empty(lhs) || empty(rhs))
    return(character())

  keys <- names(attrs)
  vals <- renv_json_quote(attrs)
  attrtext <- paste(keys, vals, sep = "=", collapse = ", ")

  fmt <- '"%s" -> "%s" [%s]'
  sprintf(fmt, lhs, rhs, attrtext)

}

renv_graphviz_attrs <- function(field, renderer) {

  dil <- "#c0c0c0"

  defaults <- list(

    Depends = list(
      color = dil,
      style = "solid"
    ),

    Imports = list(
      color = dil,
      style = "solid"
    ),

    LinkingTo = list(
      color = dil,
      style = "dashed"
    ),

    Suggests = list(
      color = "darkgreen",
      style = "dashed"
    ),

    Enhances = list(
      color = "darkblue",
      style = "dashed"
    )

  )

  attrs <- defaults[[field]]
  if (identical(renderer, "visNetwork")) {

    extra <- c(
      font.align = "middle"
    )

    attrs <- c(attrs, extra)

  }

  attrs

}

renv_graphviz_defaults <- function(renderer) {

  case(
    identical(renderer, "visNetwork") ~ renv_graphviz_defaults_visnetwork(),
    identical(renderer, "DiagrammeR") ~ renv_graphviz_defaults_diagrammer(),
  )

}

renv_graphviz_defaults_visnetwork <- function() {

  list(

    node = list(
      style     = "filled",
      shape     = "ellipse",
      color     = "black",
      fillcolor = "#e5d8bd",
      fontname  = "Helvetica"
    )

  )

}

renv_graphviz_defaults_diagrammer <- function() {

  list(

    graph = list(
      nodesep = 0.10
    ),

    node = list(
      style     = "filled",
      shape     = "ellipse",
      fillcolor = "#e5d8bd",
      fontname  = "Helvetica"
    )

  )

}

renv_graphviz_render <- function(attributes, asis) {

  rendered <- enumerate(attributes, function(key, value) {

    if (is.null(names(value))) {
      lhs <- if (asis) key else renv_json_quote(key)
      rhs <- renv_graphviz_render_value(value)
      if (length(lhs) && length(rhs))
        paste(lhs, rhs, sep = " = ")
    } else {
      keys <- names(value)
      vals <- renv_graphviz_render_value(value)
      fmt <- if (asis) '%s [%s]' else '"%s" [%s]'
      sprintf(fmt, key, paste(keys, vals, sep = "=", collapse = ", "))
    }

  })

  unlist(rendered, recursive = TRUE, use.names = FALSE)

}

renv_graphviz_render_value <- function(value) {
  if (is.numeric(value))
    format(value)
  else if (is.logical(value))
    tolower(as.character(value))
  else
    renv_json_quote(value)
}

renv_graph_renderer <- function(renderer) {

  # allow functions as-is
  if (is.function(renderer))
    return(renderer)

  # otherwise, match
  renderer <- match.arg(renderer, choices = c("DiagrammeR", "visNetwork"))
  if (!renv_package_installed(renderer)) {
    fmt <- "package '%s' is required to render graphs but is not installed"
    stopf(fmt, renderer)
  }

  renderer

}

