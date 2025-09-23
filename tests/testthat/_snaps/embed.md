# renv::embed() works with .R files

    Code
      writeLines(readLines("test-embed.R"))
    Output
      renv::use(
        bread     = "bread@1.0.0",
        breakfast = "breakfast@1.0.0",
        oatmeal   = "oatmeal@1.0.0",
        toast     = "toast@1.0.0"
      )
      
      library(breakfast)

# renv::embed() works with .Rmd files

    Code
      writeLines(readLines("test-embed.Rmd"))
    Output
      ---
      title: Embedding Test
      ---
      
      ```{r lockfile, include=FALSE}
      renv::use(
        bread     = "bread@1.0.0",
        breakfast = "breakfast@1.0.0",
        oatmeal   = "oatmeal@1.0.0",
        toast     = "toast@1.0.0"
      )
      ```
      
      
      ```{r}
      library(breakfast)
      ```

