
glue(
  "{ library(A) }",
  "{ library(B) }"
)

glue(
  "< library(C) >",
  .open  = "<",
  .close = ">"
)
