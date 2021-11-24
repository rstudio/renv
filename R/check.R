
renv_check_unknown_source <- function(records, project = NULL) {

  # nothing to do if we have no records
  if (empty(records))
    return(TRUE)

  # for testing, we ignore renv
  if (renv_tests_running())
    records$renv <- NULL

  # keep only records which have unknown source
  unknown <- filter(records, function(record) {

    source <- renv_record_source(record)
    if (source != "unknown")
      return(FALSE)

    localpath <- tryCatch(
      renv_retrieve_cellar_find(record, project),
      error = function(e) ""
    )

    if (file.exists(localpath))
      return(FALSE)

    TRUE

  })

  # if all records have a known source, return TRUE
  if (empty(unknown))
    return(TRUE)

  # provide warning
  if (!renv_tests_running())
    renv_warnings_unknown_sources(unknown)

  # return FALSE to indicate failed validation
  FALSE

}


