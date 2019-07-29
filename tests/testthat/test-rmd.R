
context("R Markdown")

test_that("The chunk header parser works as expected", {
  skip_if_not_installed("knitr")

  cases <- list(

    # R Markdown ----
    list(
      input    = "```{r}",
      type     = "md",
      expected = list(engine = "r")
    ),

    list(
      input    = "```{r a-b-c}",
      type     = "md",
      expected = list(engine = "r", label = "a-b-c")
    ),

    list(
      input    = "```{r, a-b-c}",
      type     = "md",
      expected = list(engine = "r", label = "a-b-c")
    ),

    list(
      input    = "```{r, a-b-c, a=1+1}",
      type     = "md",
      expected = list(engine = "r", label = "a-b-c", a = quote(1 + 1))
    ),

    list(
      input    = "```{python}",
      type     = "md",
      expected = list(engine = "python")
    ),

    list(
      input    = "```{r engine = 'Rcpp'}",
      type     = "md",
      expected = list(engine = "Rcpp")
    ),

    list(
      input    = "```{r, engine = 'Rcpp'}",
      type     = "md",
      expected = list(engine = "Rcpp")
    ),

    list(
      input    = "```{r a?weird?label}",
      type     = "md",
      expected = list(engine = "r", label = "a?weird?label")
    ),

    list(
      input    = "```{r a'weird'label, engine = 'bash'}",
      type     = "md",
      expected = list(engine = "bash", label = "a'weird'label")
    ),

    list(
      input    = "```{r, 'a=weird=label', engine = 'bash'}",
      type     = "md",
      expected = list(engine = "bash", label = "a=weird=label")
    ),

    # Sweave ----
    list(
      input = "<<a-b-c>>=",
      type  = "rnw",
      expected = list(engine = "r", label = "a-b-c")
    ),

    list(
      input = "<<a-b-c, a=1, b=2>>=",
      type  = "rnw",
      expected = list(engine = "r", label = "a-b-c", a = 1, b = 2)
    )

  )

  for (case in cases) {
    output <- renv_dependencies_discover_parse_params(case$input, case$type)
    expect_same_elements(output, case$expected)
  }

})
