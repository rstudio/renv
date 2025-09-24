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

