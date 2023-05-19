
test_that("bind() handles named lists", {

    data <- list(
        alpha = list(A = 1, B = 2),
        beta  = list(A = 3, B = 4),
        gamma = list(A = 5, B = 6)
    )

    actual <- bind(data)
    expected <- data.frame(
        Index = names(data),
        A = c(1, 3, 5),
        B = c(2, 4, 6),
        stringsAsFactors = FALSE
    )

    expect_equal(actual, expected)

})

test_that("bind() warns on name collision", {
    data <- list(alpha = list(Index = 1), beta = list(Index = 2))
    expect_error(bind(data))
})

test_that("bind() handles data.frames with potentially different names", {

    data <- list(
        a = data.frame(A = 1, B = 2),
        b = data.frame(A = 1, C = 3)
    )

    bound <- bind(data)
    expected <- data.frame(
        Index = c("a", "b"),
        A     = c(1, 1),
        B     = c(2, NA),
        C     = c(NA, 3),
        stringsAsFactors = FALSE
    )

    expect_identical(bound, expected)

})

test_that("bind() preserves order where possible", {

    data <- list(
        a = data.frame(A = 1,        C = 3),
        b = data.frame(A = 1, B = 2, C = 3)
    )

    bound <- bind(data)
    expect_equal(names(bound), c("Index", "A", "B", "C"))

})

test_that("bind() handles unnamed lists with explicit name", {

    data <- list(
        list(1, 2, 3),
        list(4, 5, 6),
        list(7, 8, 9)
    )

    actual <- bind(data, names = c("A", "B", "C"))

    expected <- data.frame(
        A = c(1, 4, 7),
        B = c(2, 5, 8),
        C = c(3, 6, 9)
    )

    expect_equal(actual, expected)

})

