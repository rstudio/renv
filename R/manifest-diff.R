
renv_manifest_diff <- function(old, new) {

  # ensure both lists have the same names, inserting missing
  # entries for those without any value
  nms <- union(names(old), names(new))
  if (length(nms)) {

    old[setdiff(nms, names(old))] <- list(NULL)
    new[setdiff(nms, names(new))] <- list(NULL)

    ord <- order(nms)
    old <- old[ord]
    new <- new[ord]

  }

  # check for differences
  diffs <- mapply(renv_manifest_diff_impl, old, new, SIMPLIFY = FALSE)

  # drop NULL entries
  diffs[!map_lgl(diffs, empty)]

}

renv_manifest_diff_impl <- function(lhs, rhs) {
  case(
    identical(lhs, rhs)          ~ NULL,
    is.list(lhs) && empty(rhs)   ~ renv_manifest_diff(lhs, list()),
    empty(lhs) && is.list(rhs)   ~ renv_manifest_diff(list(), rhs),
    is.list(lhs) && is.list(rhs) ~ renv_manifest_diff(lhs, rhs),
    TRUE                         ~ list(before = lhs, after = rhs)
  )
}

renv_manifest_diff_packages <- function(old, new) {

  old <- old$R$Packages; new <- new$R$Packages
  packages <- named(union(names(old), names(new)))
  actions <- lapply(packages, function(package) {

    before <- old[[package]]; after <- new[[package]]

    case(
      is.null(before) ~ "install",
      is.null(after)  ~ "remove",

      before$Version < after$Version ~ "upgrade",
      before$Version > after$Version ~ "downgrade",
      before$Source != after$Source  ~ "crossgrade"
    )

  })

  Filter(Negate(is.null), actions)

}

