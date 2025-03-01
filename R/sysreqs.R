
the$sysreqs <- NULL

renv_sysreqs_get <- function(package) {
  sysreqs <- renv_sysreqs_read(package)
  renv_sysreqs_get_impl(sysreqs)
}

renv_sysreqs_get_impl <- function(sysreqs) {
  rules <- renv_sysreqs_rules()
  matches <- map(sysreqs, renv_sysreqs_match, rules)
  unlist(matches, use.names = FALSE)
}

renv_sysreqs_read <- function(package) {
  desc <- renv_description_read(package)
  desc[["SystemRequirements"]] %||% ""
}

renv_sysreqs_rules <- function() {
  the$sysreqs <- the$sysreqs %||% renv_sysreqs_rules_impl()
}

renv_sysreqs_rules_impl <- function() {
  location <- system.file("sysreqs", package = "renv")
  files <- list.files(location, full.names = TRUE)
  rules <- map(files, renv_json_read)
  names(rules) <- basename(files)
  rules
}

renv_sysreqs_match <- function(req, rules) {
  map(rules, renv_sysreqs_match_impl, req = req)
}

renv_sysreqs_match_impl <- function(req, rule) {

  # check for a match in the declared system requirements
  pattern <- paste(rule$patterns, collapse = "|")
  matches <- grepl(pattern, req, ignore.case = TRUE, perl = TRUE)

  # if we got a match, pull out the dependent packages
  if (matches) {
    for (dependency in rule$dependencies) {
      for (constraint in dependency$constraints) {
        if (constraint$os == the$os) {
          if (constraint$distribution == the$distribution) {
            return(dependency$packages)
          }
        }
      }
    }
  }

}

renv_sysreqs_check <- function(records) {

  # skip if we're not enabled
  enabled <- config$sysreqs.check()
  if (!identical(enabled, TRUE))
    return(FALSE)

  # figure out which system packages are required
  syspkgs <- map(records, function(record) {

    # if we already have system requirements recorded, use those
    sysreqs <- record[["SystemRequirements"]]
    if (!is.null(sysreqs))
      return(renv_sysreqs_get_impl(sysreqs))

    # otherwise, if we have a recorded path, use that instead
    path <- record[["Path"]]
    if (!is.null(path))
      return(renv_sysreqs_get(path))

  })

  # collect list of all packages discovered
  allsyspkgs <- sort(unique(unlist(syspkgs, use.names = FALSE)))

  # check if those packages are installed
  result <- if (nzchar(Sys.which("dpkg-query"))) {
    command <- sprintf("dpkg-query -W %s 2> /dev/null", paste(allsyspkgs, collapse = " "))
    output <- suppressWarnings(system(command, intern = TRUE))
    renv_properties_read(text = output, delimiter = "\t")
  } else if (nzchar(Sys.which("rpm"))) {
    command <- sprintf("rpm -q %s 2> /dev/null", paste(allsyspkgs, collapse = " "))
    output <- suppressWarnings(system(command, intern = TRUE))
    output <- grep("is not installed", output, fixed = TRUE, value = TRUE, invert = TRUE)
    renv_properties_read(text = output, delimiter = "\t")
  } else {
    return(FALSE)
  }

  # check for matches
  matches <- map_lgl(allsyspkgs, function(syspkg) {
    any(startsWith(names(result), syspkg))
  })

  misspkgs <- allsyspkgs[!matches]
  if (empty(misspkgs))
    return(TRUE)

  # notify the user
  preamble <- "The following required system packages are not installed:"
  postamble <- "The R packages depending on these system packages may fail to install."
  parts <- map(misspkgs, function(misspkg) {
    needs <- map_lgl(syspkgs, function(syspkg) misspkg %in% syspkg)
    list(misspkg, names(syspkgs)[needs])
  })

  lhs <- extract_chr(parts, 1L)
  rhs <- map_chr(extract(parts, 2L), paste, collapse = ", ")
  messages <- sprintf("%s [required by %s]", format(lhs), format(rhs))
  caution_bullets(preamble, messages, postamble)

  installer <- case(
    nzchar(Sys.which("apt"))    ~ "apt install",
    nzchar(Sys.which("dnf"))    ~ "dnf install",
    nzchar(Sys.which("pacman")) ~ "pacman -S",
    nzchar(Sys.which("yum"))    ~ "yum install",
    nzchar(Sys.which("zypper")) ~ "zypper install",
  )

  preamble <- "An administrator can install these packages with:"
  command <- paste("sudo", installer, paste(misspkgs, collapse = " "))
  caution_bullets(preamble, command)

  cancel_if(!proceed())

}
