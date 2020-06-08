
context("Dependencies")

test_that(".Rproj files requesting devtools is handled", {
  renv_tests_scope()
  writeLines("PackageUseDevtools: Yes", "project.Rproj")
  deps <- dependencies(dev = TRUE)
  packages <- deps$Package
  expect_setequal(packages, c("devtools", "roxygen2"))
})

test_that("usages of library, etc. are properly handled", {

  deps <- dependencies("resources/code.R")
  pkgs <- deps$Package

  expect_equal(pkgs, tolower(pkgs))

  l <- pkgs[nchar(pkgs) == 1]
  expect_equal(sort(l), letters[seq_along(l)])

})

test_that("parse errors are okay in .Rmd documents", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  deps <- dependencies("resources/chunk-errors.Rmd")
  pkgs <- deps$Package
  expect_setequal(pkgs, c("rmarkdown", "dplyr"))
})

test_that("inline chunks are parsed for dependencies", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  deps <- dependencies("resources/inline-chunks.Rmd")
  pkgs <- deps$Package
  expect_setequal(pkgs, c("rmarkdown", "inline", "multiple", "separate"))
})

test_that("usages of S4 tools are discovered", {
  file <- renv_test_code({setClass("ClassSet")})
  deps <- dependencies(file)
  expect_true(deps$Package == "methods")
})

test_that("the package name is validated when inferring dependencies", {
  file <- renv_test_code({SomePackage::setClass("ClassSet")})
  deps <- dependencies(file)
  expect_true("SomePackage" %in% deps$Package)
  expect_false("methods" %in% deps$Package)
})

test_that("empty chunks don't cause issues during dependency resolution", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  deps <- dependencies("resources/empty-chunk.Rmd")
  pkgs <- deps$Package
  expect_setequal(pkgs, c("rmarkdown"))
})

test_that("pacman::p_load() usages are understood", {
  deps <- dependencies("resources/pacman.R")
  packages <- setdiff(deps$Package, "pacman")
  expect_setequal(packages, letters[1:length(packages)])
})

test_that("import:: usages are understood", {
  deps <- dependencies("resources/import.R")
  packages <- setdiff(deps$Package, "import")
  expect_setequal(packages, letters[1:length(packages)])
})

test_that("renv warns when large number of files found", {

  renv_tests_scope()

  files <- sprintf("%.3i.R", 1:10)
  file.create(files)

  output <- local({

    renv_scope_options(
      renv.verbose = TRUE,
      renv.config.dependencies.limit = 5L
    )

    capture.output(invisible(dependencies()))

  })

  expect_true(length(output) > 0)

})

test_that("evil knitr chunks are handled", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  deps <- dependencies("resources/evil.Rmd")
  packages <- deps$Package
  expect_setequal(packages, c("rmarkdown", "a", "b"))
})

test_that("renv_dependencies_requires warns once", {
  expect_warning(renv_dependencies_require("nosuchpackage", "test"))
  expect_false(renv_dependencies_require("nosuchpackage", "test"))
})

test_that("the presence of an rsconnect folder forces dependency on rsconnect", {
  renv_tests_scope()
  dir.create("rsconnect")
  deps <- renv::dependencies()
  expect_true("rsconnect" %in% deps$Package)
})

test_that("dependencies can accept multiple files", {

  deps <- renv::dependencies(
    path = c("packages/bread", "packages/breakfast"),
    root = getwd(),
    quiet = TRUE
  )

  expect_setequal(deps$Package, c("oatmeal", "toast"))

})

test_that("dependencies can infer the root directory", {

  deps <- renv::dependencies(
    path = c("packages/bread", "packages/breakfast"),
    quiet = TRUE
  )

  expect_setequal(deps$Package, c("oatmeal", "toast"))

})

test_that("no warnings are produced when crawling dependencies", {

  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")

  expect_warning(
    regexp = NA,
    renv::dependencies(
      "resources",
      root = file.path(getwd(), "resources"),
      quiet = TRUE
    )
  )

})

test_that("Suggests are dev. deps for non-package projects", {
  renv_tests_scope()
  writeLines(c("Type: Project", "Suggests: bread"), con = "DESCRIPTION")
  deps <- dependencies(dev = TRUE)
  expect_true(nrow(deps) == 1)
  expect_true(deps$Package == "bread")
  expect_true(deps$Dev)
})

test_that("Suggests are _not_ dev. deps for package projects", {
  renv_tests_scope()
  writeLines(c("Type: Package", "Suggests: bread"), con = "DESCRIPTION")
  deps <- dependencies(dev = FALSE)
  expect_true(nrow(deps) == 1)
  expect_true(deps$Package == "bread")
  expect_false(deps$Dev)
})

test_that("packages referenced by modules::import() are discovered", {
  deps <- dependencies("resources/modules.R")
  expect_setequal(deps$Package, c("A", "B", "C", "D", "G", "H", "modules"))
})

test_that("dependencies specified in R Markdown site generators are found", {

  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")

  renv_tests_scope()
  writeLines(
    c("---", "site: blogdown:::blogdown_site", "---"),
    con = "index.Rmd")
  deps <- dependencies()
  expect_true("blogdown" %in% deps$Package)
  writeLines(
    c("---", "site: bookdown::bookdown_site", "---"),
    con = "index.Rmd")
  deps <- dependencies()
  expect_true("bookdown" %in% deps$Package)

})

test_that("Suggest dependencies are ignored by default", {
  renv_tests_scope("breakfast")
  install("breakfast")
  expect_false(renv_package_installed("egg"))
})

test_that("Suggest dependencies are used when requested", {
  renv_tests_scope("breakfast")
  fields <- c("Imports", "Depends", "LinkingTo", "Suggests")
  settings$package.dependency.fields(fields)
  install("breakfast")
  expect_true(renv_package_installed("egg"))
})

test_that("a call to geom_hex() implies a dependency on ggplot2", {

  file <- renv_test_code({
    ggplot() + geom_hex()
  })

  deps <- dependencies(file)
  expect_true("hexbin" %in% deps$Package)

})

test_that("empty fields are handled in DESCRIPTION", {
  deps <- dependencies("resources/DESCRIPTION", progress = FALSE)
  expect_setequal(deps$Package, c("a", "b", "c"))
})

test_that("recursive symlinks are handled", {
  skip_on_os("windows")

  project <- renv_tempfile()
  ensure_directory(project)

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  symlink <- file.path(project, "symlink")
  file.symlink(dirname(symlink), symlink)

  renv:::dependencies()

})

test_that(".renvignore can be used to ignore all but certain files", {

  renv_tests_scope()

  writeLines(c("*", "!dependencies.R"), con = ".renvignore")
  writeLines("library(oatmeal)", con = "script.R")
  writeLines("library(bread)", con = "dependencies.R")

  deps <- dependencies(quiet = TRUE)

  expect_true("bread" %in% deps$Package)
  expect_false("oatmeal" %in% deps$Package)

})
