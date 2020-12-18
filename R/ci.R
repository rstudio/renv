
renv_ci_dependencies <- function() {

  # find the packages used in this project
  records <- renv_project_remotes(project = getwd())
  packages <- extract_chr(records, "Package")

  # find recursive dependencies of these packages
  revdeps <- tools::package_dependencies(packages, recursive = TRUE)

  # collapse into unique list
  all <- unique(unlist(revdeps))

  # get versions of these dependencies
  db <- as.data.frame(
    available.packages(filters = c("OS_type", "duplicates")),
    stringsAsFactors = FALSE
  )

  # retrieve fields
  resolved <- db[db$Package %in% all, c("Package", "Version")]
  rownames(resolved) <- NULL

  # save to file for hashing
  ensure_directory("ci")
  saveRDS(resolved, file = "ci/dependencies.rds")

}
