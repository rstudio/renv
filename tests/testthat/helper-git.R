# create an empty git repository, for use as a local git remote
renv_tests_git_init <- function(scope = parent.frame()) {

  repo <- renv_scope_tempfile("renv-repo-", scope = scope)
  ensure_directory(repo)
  renv_scope_wd(repo)

  renv_system_exec("git", c("init", "--quiet"), action = "git init")
  renv_system_exec("git", c("config", "user.name", shQuote("User Name")), action = "git config")
  renv_system_exec("git", c("config", "user.email", shQuote("user@example.com")), action = "git config")

  repo

}

# write a DESCRIPTION for 'bread' at the requested version, commit it, and
# return the sha of that commit
renv_tests_git_commit <- function(repo, version, depends = NULL) {

  renv_scope_wd(repo)

  desc <- c(
    "Package: bread",
    "Type: Package",
    paste("Version:", version),
    if (length(depends)) paste("Depends:", depends)
  )

  writeLines(desc, con = "DESCRIPTION")
  writeLines("", con = "NAMESPACE")

  renv_system_exec("git", c("add", "-A"), action = "git add")
  renv_system_exec("git", c("commit", "--quiet", "-m", shQuote(version)), action = "git commit")
  renv_git_sha(repo)

}

# a local git repository holding 'bread' 0.5.0 on its default branch, and
# 1.0.0 on a 'release' branch
renv_tests_git_remote <- function(scope = parent.frame()) {

  repo <- renv_tests_git_init(scope = scope)
  renv_scope_wd(repo)

  head <- renv_tests_git_commit(repo, "0.5.0")

  renv_system_exec("git", c("checkout", "--quiet", "-b", "release"), action = "git checkout")
  release <- renv_tests_git_commit(repo, "1.0.0")
  renv_system_exec("git", c("checkout", "--quiet", "-"), action = "git checkout")

  # use a file URL, so that the repository path is not mistaken for a local
  # package source when the package is later retrieved
  list(
    repo = repo,
    url  = paste0("file://", renv_path_normalize(repo)),
    shas = list(head = head, release = release)
  )

}

# add empty commits to a branch, e.g. to move a commit deep into its history
renv_tests_git_extend <- function(repo, branch, count) {

  renv_scope_wd(repo)

  # a fast-import stream, which is much quicker than committing one at a time;
  # the first commit continues from the branch's current tip
  from <- c(sprintf("from refs/heads/%s^0", branch), rep.int("", count - 1L))
  commits <- sprintf(
    "commit refs/heads/%s\ncommitter User Name <user@example.com> %i +0000\ndata 0\n%s\n",
    branch,
    1700000000L + seq_len(count),
    from
  )

  stream <- renv_scope_tempfile("renv-fast-import-")
  writeLines(commits, con = stream)

  status <- system2("git", c("fast-import", "--quiet"), stdin = stream)
  if (!identical(status, 0L))
    stopf("error adding commits to '%s' [status code %i]", branch, status)

  invisible(repo)

}

# resolve the remote 'spec' to a local git remote; renv's remote parser doesn't
# accept the file URLs that these remotes use
renv_tests_git_scope_spec <- function(spec, remote, scope = parent.frame()) {

  resolve <- renv_remotes_resolve
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_remotes_resolve",
    replacement = function(x, ...) {
      if (!identical(x, spec))
        return(resolve(x, ...))
      renv_remotes_resolve_git(remote)
    },
    scope = scope
  )

}

# count the clones made of git repositories
renv_tests_git_scope_clones <- function(scope = parent.frame()) {

  counter <- env(count = 0L)

  impl <- renv_retrieve_git_impl
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_retrieve_git_impl",
    replacement = function(record, path) {
      counter$count <- counter$count + 1L
      impl(record, path)
    },
    scope = scope
  )

  counter

}
