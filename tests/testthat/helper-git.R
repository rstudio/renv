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

# write a DESCRIPTION for 'package' at the requested version, commit it, and
# return the sha of that commit
renv_tests_git_commit <- function(repo,
                                  version,
                                  depends = NULL,
                                  remotes = NULL,
                                  package = "bread")
{
  renv_scope_wd(repo)

  desc <- c(
    paste("Package:", package),
    "Type: Package",
    paste("Version:", version),
    if (length(depends)) paste("Depends:", depends),
    if (length(remotes)) paste("Remotes:", remotes)
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

# local git repositories for 'bread', whose default branch has moved on from
# 0.5.0 (tagged 'v0.5.0') to 1.0.0, and for 'bagel', which depends on 'bread'
# and declares it in its Remotes field without a ref. the remote specs
# 'baker/bagel', 'baker/bread' and 'baker/bread@v0.5.0' resolve to these
renv_tests_git_remotes_unpinned <- function(scope = parent.frame()) {

  bread <- renv_tests_git_init(scope = scope)
  old <- renv_tests_git_commit(bread, "0.5.0")
  new <- renv_tests_git_commit(bread, "1.0.0")

  local({
    renv_scope_wd(bread)
    renv_system_exec("git", c("tag", "v0.5.0", old), action = "git tag")
  })

  bagel <- renv_tests_git_init(scope = scope)
  sha <- renv_tests_git_commit(
    repo    = bagel,
    version = "1.0.0",
    depends = "bread",
    remotes = "baker/bread",
    package = "bagel"
  )

  breadurl <- paste0("file://", renv_path_normalize(bread))
  bagelurl <- paste0("file://", renv_path_normalize(bagel))

  renv_tests_git_scope_spec("baker/bagel", list(url = bagelurl, repo = "bagel"), scope = scope)
  renv_tests_git_scope_spec("baker/bread", list(url = breadurl, repo = "bread"), scope = scope)
  renv_tests_git_scope_spec("baker/bread@v0.5.0", list(url = breadurl, repo = "bread", ref = "v0.5.0"), scope = scope)

  list(
    bagel = list(url = bagelurl, sha = sha),
    bread = list(url = breadurl, shas = list(old = old, new = new))
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

  # write in binary mode, since fast-import rejects Windows line endings
  stream <- renv_scope_tempfile("renv-fast-import-")
  con <- file(stream, open = "wb")
  writeLines(commits, con = con)
  close(con)

  status <- system2("git", c("fast-import", "--quiet"), stdin = stream)
  if (!identical(status, 0L))
    stopf("error adding commits to '%s' [status code %i]", branch, status)

  invisible(repo)

}

# resolve the remote 'spec' to a local git remote; renv's remote parser doesn't
# accept the file URLs that these remotes use
renv_tests_git_scope_spec <- function(spec, remote, scope = parent.frame()) {

  # read from the namespace, so that stubs for several specs compose
  resolve <- get("renv_remotes_resolve", envir = asNamespace("renv"))
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

# count the calls made to a function in renv's namespace
renv_tests_git_scope_counter <- function(symbol, scope = parent.frame()) {

  counter <- env(count = 0L)

  impl <- get(symbol, envir = asNamespace("renv"))
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = symbol,
    replacement = function(...) {
      counter$count <- counter$count + 1L
      impl(...)
    },
    scope = scope
  )

  counter

}

# count the clones made of git repositories
renv_tests_git_scope_clones <- function(scope = parent.frame()) {
  renv_tests_git_scope_counter("renv_retrieve_git_impl", scope = scope)
}

# count the fetches of a ref's history, made when a commit can't be fetched
renv_tests_git_scope_history <- function(scope = parent.frame()) {
  renv_tests_git_scope_counter("renv_retrieve_git_history", scope = scope)
}

# make git refuse to serve a commit by its sha, as its original wire protocol
# (and some servers) do; the tests then need to fetch the history of a ref.
# git only reads this configuration from the environment as of 2.31
renv_tests_git_scope_protocol_v0 <- function(scope = parent.frame()) {

  version <- renv_tests_git_version()
  skip_if(version < "2.31", "git 2.31 or newer is required")

  renv_scope_envvars(
    GIT_CONFIG_COUNT   = "1",
    GIT_CONFIG_KEY_0   = "protocol.version",
    GIT_CONFIG_VALUE_0 = "0",
    scope = scope
  )

}

renv_tests_git_version <- function() {
  output <- renv_system_exec("git", "--version", action = "git version")
  version <- regmatches(output, regexpr("[0-9]+([.][0-9]+)+", output))
  numeric_version(version[[1L]])
}
