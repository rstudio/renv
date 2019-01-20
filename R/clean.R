
# remove user packages in system library
renv_clean_system_library <- function() {
  packages <- installed.packages(lib.loc = .Library, priority = "NA")
  remove.packages(rownames(packages), lib = .Library)
}
