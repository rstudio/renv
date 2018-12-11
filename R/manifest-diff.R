
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

