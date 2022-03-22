
json <- '{ "\U0001f4c4": "\u2728" }'
values <- renv_json_read(text = json)
expect_equal(values, list("\U0001f4c4" = "\u2728"))

actual <- renv_json_write(values, file = NULL)
expect_equal(
  gsub("[[:space:]]*", "", actual, perl = TRUE),
  gsub("[[:space:]]*", "", json, perl = TRUE)
)
