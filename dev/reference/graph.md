# Generate a Package Dependency Graph

Generate a package dependency graph.

## Usage

``` r
graph(
  root = NULL,
  leaf = NULL,
  ...,
  suggests = FALSE,
  enhances = FALSE,
  resolver = NULL,
  renderer = c("DiagrammeR", "visNetwork"),
  attributes = list(),
  project = NULL
)
```

## Arguments

- root:

  The top-most package dependencies of interest in the dependency graph.

- leaf:

  The bottom-most package dependencies of interest in the dependency
  graph.

- ...:

  Unused arguments, reserved for future expansion. If any arguments are
  matched to `...`, renv will signal an error.

- suggests:

  Should suggested packages be included within the dependency graph?

- enhances:

  Should enhanced packages be included within the dependency graph?

- resolver:

  An R function accepting a package name, and returning the contents of
  its `DESCRIPTION` file (as an R `data.frame` or `list`). When `NULL`
  (the default), an internal resolver is used.

- renderer:

  Which package should be used to render the resulting graph?

- attributes:

  An R list of graphViz attributes, mapping node names to attribute
  key-value pairs. For example, to ask graphViz to prefer orienting the
  graph from left to right, you can use
  `list(graph = c(rankdir = "LR"))`.

- project:

  The project directory. If `NULL`, then the active project will be
  used. If no project is currently active, then the current working
  directory is used instead.

## Examples

``` r
if (FALSE) { # \dontrun{
# graph the relationship between devtools and rlang
graph(root = "devtools", leaf = "rlang")

# figure out why a project depends on 'askpass'
graph(leaf = "askpass")
} # }
```
