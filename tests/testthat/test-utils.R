
test_that("case() handles control flow as expected", {

  for (i in 1:3)
    case(i == 2 ~ break)

  expect_equal(i, 2)

  value <- local({
    case(FALSE ~ stop(), TRUE ~ return(42))
    24
  })

  expect_equal(value, 42)

})

test_that("common utils work as expected", {
  expect_equal(lines(1, 2, 3), "1\n2\n3")

  if (nzchar(Sys.which("git")))
    expect_equal(git(), Sys.which("git"))
  else
    expect_error(git())
})

test_that("versions are compared as expected", {

  expect_equal(renv_version_compare("0.1.0", "0.2.0"), -1L)
  expect_equal(renv_version_compare("0.2.0", "0.2.0"), +0L)
  expect_equal(renv_version_compare("0.3.0", "0.2.0"), +1L)

})

test_that("inject inserts text at expected anchor point", {

  text <- c("alpha", "beta", "gamma")

  injected <- inject(text, "beta", "BETA")
  expect_equal(injected, c("alpha", "BETA", "gamma"))

  injected <- inject(text, "BETA", "BETA", "beta")
  expect_equal(injected, c("alpha", "beta", "BETA", "gamma"))

})

test_that("renv_path_aliased() correctly forms aliased path", {
  path <- "~/some/path"
  expanded <- path.expand(path)
  expect_equal(path, renv_path_aliased(expanded))
})

test_that("find() returns first non-null matching value", {

  data <- list(x = 1, y = 2, z = 3)

  value <- find(data, function(datum) {
    if (datum == 2)
      return(42)
  })
  expect_equal(value, 42)

  value <- find(data, function(datum) {
    if (datum == 4)
      return(42)
  })
  expect_null(value)

})

test_that("recursing() reports if we're recursing", {

  f <- function(i) {

    if (recursing())
      expect_true(i == 2)
    else
      expect_true(i == 1)


    if (i < 2)
      f(i + 1)

    if (recursing())
      expect_true(i == 2)
    else
      expect_true(i == 1)

  }

  f(1)


})

test_that("sys.call(sys.parent()) does what we think it does", {

  inner <- function() { as.character(sys.call(sys.parent())[[1L]]) }
  outer <- function() { inner() }
  expect_equal(outer(), "outer")

})

test_that("new() creates objects", {

  oop <- new({
    .data <- NULL
    get <- function() { .data }
    set <- function(data) { .data <<- data }
  })

  expect_identical(oop$get(), NULL)
  expect_identical(oop$set(42L), 42L)
  expect_identical(oop$get(), 42L)

  expect_null(oop$data)

  data <- get(".data", envir = oop)
  expect_equal(data, 42L)

})

test_that("rows(), cols() does what we want", {

  indices <- list(
    c(1, 3, 5),
    c(TRUE, FALSE)
  )

  for (index in indices) {

    lhs <- mtcars[index, ]
    rhs <- rows(mtcars, index)
    rownames(lhs) <- rownames(rhs) <- NULL
    expect_equal(lhs, rhs)

    lhs <- mtcars[index]
    rhs <- cols(mtcars, index)
    expect_equal(lhs, rhs)

  }

})

test_that("visited() works as expected", {

  envir <- new.env(parent = emptyenv())
  expect_false(visited("hello", envir))
  expect_true(visited("hello", envir))
  expect_true(visited("hello", envir))

  envir$hello <- FALSE
  expect_false(visited("hello", envir))
  expect_true(visited("hello", envir))

})

test_that("ensure_directory() works even under contention", {
  skip_on_cran()
  skip_if(getRversion() < "4.0.0")

  n <- 4
  waitfile <- tempfile("renv-wait-")
  target <- tempfile("renv-directory-")

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  script <- renv_test_code({

    renv:::summon()

    # create the directory
    wait_until(file.exists, waitfile)
    ok <- tryCatch(
      { ensure_directory(target); TRUE },
      error = function(e) FALSE
    )

    # notify parent
    conn <- renv_socket_connect(port = port, open = "wb")
    defer(close(conn))
    serialize(ok, connection = conn)

  }, list(port = server$port, waitfile = waitfile, target = target))

  for (i in 1:n) {
    system2(
      command = R(),
      args = c("--vanilla", "--slave", "-f", renv_shell_path(script)),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
    )
  }

  file.create(waitfile)

  responses <- stack()
  for (i in 1:n) {
    conn <- renv_socket_accept(server$socket, open = "rb", timeout = 3)
    responses$push(unserialize(conn))
    close(conn)
  }

  expect_true(all(unlist(responses$data())))

})
