
the$sysreqs <- NULL

renv_sysreqs_get <- function(packages) {

  # read package system requirements
  reqs <- map_chr(packages, renv_sysreqs_get_impl)

  # get the rules for these system requirements
  rules <- renv_sysreqs_rules()

  # match rules to the requirements
  map(reqs, function(req) {
    matches <- renv_sysreqs_match(req, rules)
    unlist(matches, use.names = FALSE)
  })

}

renv_sysreqs_get_impl <- function(package) {
  desc <- renv_description_read(package)
  requirements <- desc[["SystemRequirements"]]
  requirements %||% ""
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

  # figure out which system packages are required
  paths <- map_chr(records, `[[`, "Path")
  sysreqs <- renv_sysreqs_get(paths)

  # check if those packages are installed
  reqs <- unique(unlist(sysreqs, use.names = FALSE))
  result <- if (nzchar(Sys.which("dpkg-query"))) {
    command <- sprintf("dpkg-query -W %s 2> /dev/null", paste(reqs, collapse = " "))
    output <- suppressWarnings(system(command, intern = TRUE))
    renv_properties_read(text = output, delimiter = "\t")
  } else if (nzchar(Sys.which("rpm"))) {
    command <- sprintf("rpm -q %s 2> /dev/null", paste(reqs, collapse = " "))
    output <- suppressWarnings(system(command, intern = TRUE))
    output <- grep("is not installed", output, fixed = TRUE, value = TRUE, invert = TRUE)
    renv_properties_read(text = output, delimiter = "\t")
  } else {
    return(FALSE)
  }

  # check for matches
  matches <- map_lgl(reqs, function(req) {
    any(startsWith(names(result), req))
  })

  missing <- reqs[!matches]
  if (empty(missing))
    return(TRUE)

  # notify the user
  preamble <- "The following required system packages are not installed:"
  postamble <- "The R packages depending on these system packages may fail to install."
  messages <- map_chr(missing, function(req) {
    needs <- map_lgl(sysreqs, function(sysreq) req %in% sysreq)
    sprintf("%s [required by %s]", req, paste(names(sysreqs)[needs], collapse = ", "))
  })

  caution_bullets(preamble, messages, postamble)

  preamble <- "An administrator can install these packages with:"

  installer <- case(
    nzchar(Sys.which("apt"))    ~ "apt install",
    nzchar(Sys.which("dnf"))    ~ "dnf install",
    nzchar(Sys.which("pacman")) ~ "pacman -S",
    nzchar(Sys.which("yum"))    ~ "yum install",
    nzchar(Sys.which("zypper")) ~ "zypper install",
  )

  command <- paste("sudo", installer, paste(missing, collapse = " "))
  caution_bullets(preamble, command)

  cancel_if(!proceed())

}
