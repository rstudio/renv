
context("ACLS")

test_that("renv_acls_reset() can reset ACLs", {

  skip_on_cran()
  skip_if(!renv_platform_linux())
  skip_if(!nzchar(Sys.which("getfacl")))
  skip_if(!nzchar(Sys.which("setfacl")))

  renv_scope_tempdir()

  # create some directories
  unlink("source", recursive = TRUE)
  unlink("target", recursive = TRUE)

  # in this scenario, we'll move 'source/directory' to 'target/directory',
  # and check that the ACLs from 'target' are applied to 'target/directory'
  # when all is said and done
  dir.create("source")
  dir.create("target")

  # try setting some ACLs on the source directory;
  # this basically disallows any 'other' access
  system("setfacl -d -m o::- source")
  dir.create("source/directory")

  # try copying the directory; the ACLs should be copied
  system("cp -R source/directory target/directory")

  # try resetting its ACL
  renv_acls_reset("target/directory")

  # check that the permissions (via ACLs) were preserved
  expect_equal(file.mode("target"), file.mode("target/directory"))

})
