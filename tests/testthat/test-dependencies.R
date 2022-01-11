
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
  deps <- dependencies("resources/chunk-errors.Rmd", quiet = TRUE)
  pkgs <- deps$Package
  expect_setequal(pkgs, c("rmarkdown", "dplyr"))
})

test_that("inline chunks are parsed for dependencies", {
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

test_that("box::use() usages are handled", {
  deps <- dependencies("resources/box.R")
  expect_setequal(deps$Package, c("A", "B", "C", "D", "box"))
})

test_that("targets::tar_option_set() dependencies are handled", {
  deps <- dependencies("resources/targets.R")
  expect_setequal(deps$Package, c("A", "B", "targets"))
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
  deps <- dependencies()
  expect_true("rsconnect" %in% deps$Package)
})

test_that("dependencies can accept multiple files", {

  deps <- dependencies(
    path = c("packages/bread", "packages/breakfast"),
    root = getwd(),
    quiet = TRUE
  )

  expect_setequal(deps$Package, c("oatmeal", "toast"))

})

test_that("dependencies can infer the root directory", {

  deps <- dependencies(
    path = c("packages/bread", "packages/breakfast"),
    quiet = TRUE
  )

  expect_setequal(deps$Package, c("oatmeal", "toast"))

})

test_that("no warnings are produced when crawling dependencies", {

  expect_warning(
    regexp = NA,
    dependencies(
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

  project <- renv_scope_tempfile()
  ensure_directory(project)

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  symlink <- file.path(project, "symlink")
  file.symlink(dirname(symlink), symlink)

  renv:::dependencies()

})

test_that("exercise chunks are ignored", {
  deps <- dependencies("resources/learnr-exercise.Rmd")
  expect_true("A" %in% deps$Package)
})

test_that("dependencies in R functions can be found", {
  deps <- dependencies(function() bread::bread())
  expect_true("bread" %in% deps$Package)
})

test_that("dependencies in dotfiles are discovered", {
  renv_tests_scope()
  writeLines("library(A)", con = ".Rprofile")
  deps <- dependencies(quiet = TRUE)
  expect_true(nrow(deps) == 1L)
  expect_true(basename(deps$Source) == ".Rprofile")
  expect_true(deps$Package == "A")
})

test_that("reused knitr chunks are handled", {
  deps <- dependencies("resources/knitr-reused-chunks.Rmd")
  expect_true(all(c("A", "B") %in% deps$Package))
})

test_that("empty / missing labels are handled", {
  deps <- dependencies("resources/empty-label.Rmd", progress = FALSE)
  expect_true(all(c("A", "B") %in% deps$Package))
})

test_that("only dependencies in a top-level DESCRIPTION file are used", {
  renv_tests_scope()

  dir.create("a")
  writeLines("Depends: toast", con = "DESCRIPTION")
  writeLines("Depends: oatmeal", con = "a/DESCRIPTION")

  deps <- dependencies(quiet = TRUE)
  expect_true("toast" %in% deps$Package)
  expect_false("oatmeal" %in% deps$Package)

})

test_that("multiple output formats are handled", {
  deps <- dependencies("resources/multiple-output-formats.Rmd", progress = FALSE)
  expect_true("bookdown" %in% deps$Package)
})

test_that("glue::glue() package usages are found", {
  deps <- dependencies("resources/glue.R", progress = FALSE)
  expect_true(all(c("A", "B", "C", "D", "E", "F", "G") %in% deps$Package))
  expect_false(any(letters %in% deps$Package))
})

test_that("set_engine() package usages are found", {
  deps <- dependencies("resources/parsnip.R", progress = FALSE)
  expect_setequal(deps$Package, c("glmnet"))
})

test_that("eval=F does not trip up dependencies", {
  deps <- dependencies("resources/eval.Rmd", progress = FALSE)
  expect_true("A" %in% deps$Package)
  expect_false("a" %in% deps$Package)
})

test_that("renv.ignore=FALSE, eval=TRUE is handled", {
  deps <- dependencies("resources/ignore.Rmd", progress = FALSE)
  expect_true("A" %in% deps$Package)
  expect_false("a" %in% deps$Package)
})

test_that("piped expressions can be parsed for dependencies", {
  deps <- dependencies("resources/magrittr.R", progress = FALSE)
  expect_setequal(deps$Package, c("A", "B", "C"))
})

test_that("bslib dependencies are discovered", {
  deps <- dependencies("resources/bslib.Rmd", progress = FALSE)
  expect_true("bslib" %in% deps$Package)
})

test_that("utility script dependencies are discovered", {
  deps <- dependencies("resources/utility", progress = FALSE)
  expect_false(is.null(deps))
  expect_setequal(deps$Package, c("A", "B"))
})

test_that("we handle shiny_prerendered documents", {
  deps <- dependencies("resources/shiny-prerendered.Rmd", progress = FALSE)
  expect_true("shiny" %in% deps$Package)
})

test_that("we don't infer a dependency on rmarkdown for empty .qmd", {
  deps <- dependencies("resources/quarto-empty.qmd", progress = FALSE)
  expect_true(is.null(deps) || !"rmarkdown" %in% deps$Package)
})

test_that("we do infer dependency on rmarkdown for .qmd with R chunks", {
  deps <- dependencies("resources/quarto-r-chunks.qmd", progress = FALSE)
  expect_true("rmarkdown" %in% deps$Package)
})

test_that("we parse package references from arbitrary yaml fields", {
  deps <- dependencies("resources/rmd-base-format.Rmd", progress = FALSE)
  expect_true("bookdown" %in% deps$Package)
  expect_true("rticles" %in% deps$Package)
})

test_that("dependencies in parameterized documents are discovered", {
  deps <- dependencies("resources/params.Rmd", progress = FALSE)
  expect_true(all(c("shiny", "A") %in% deps$Package))
  expect_false("B" %in% deps$Package)
})

test_that("we ignore chunks with '#| eval: false'", {
  deps <- dependencies("resources/yaml-chunks.Rmd", progress = FALSE)
  expect_false("a" %in% deps$Package)
  expect_true("A" %in% deps$Package)
})

test_that("dependencies in hidden folders are not scoured", {
  renv_tests_scope()

  dir.create(".hidden")
  writeLines("library(A)", con = ".hidden/deps.R")

  deps <- dependencies(progress = FALSE)
  expect_false("A" %in% deps$Package)

  writeLines("!.hidden", con = ".renvignore")
  deps <- dependencies(progress = FALSE)
  expect_true("A" %in% deps$Package)

})

test_that("dependencies() doesn't barf on files without read permission", {

  skip_on_windows()
  renv_tests_scope()

  dir.create("secrets")
  writeLines("library(dplyr)", con = "secrets/secrets.R")
  Sys.chmod("secrets/secrets.R", mode = "0000")

  expect_error(renv_file_read("secrets/secrets.R"))
  deps <- dependencies(quiet = TRUE)
  expect_true(NROW(deps) == 0L)

})

test_that("dependencies() doesn't barf on malformed DESCRIPTION files", {

  skip_on_windows()
  renv_tests_scope()

  writeLines("Depends: A, B\n\nImports: C, D", con = "DESCRIPTION")
  deps <- dependencies(quiet = TRUE)
  expect_setequal(deps$Package, c("A", "B", "C", "D"))

})
