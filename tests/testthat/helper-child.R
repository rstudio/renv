
# An expression which loads the renv under test in a child R process.
#
# When the tests are run via devtools::test(), renv is loaded from source by
# pkgload, and a child process calling `renv:::` would otherwise pick up
# whatever version of renv happens to be installed -- which may lack the
# behavior under test. Under R CMD check, the installed renv is the package
# under test, so nothing needs to be done.
renv_tests_child_preamble <- function() {

  dev <-
    requireNamespace("pkgload", quietly = TRUE) &&
    pkgload::is_dev_package("renv")

  if (!dev)
    return(quote(invisible()))

  root <- getNamespaceInfo(asNamespace("renv"), "path")
  substitute(
    pkgload::load_all(root, quiet = TRUE, helpers = FALSE),
    list(root = root)
  )

}
