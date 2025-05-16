
# base R usages
use("A")
base::use("B", c("filter", "select"))

# renv style usages
renv::use(
  C = "tidyverse/dplyr",
  D = "r-lib/rlang"
)
