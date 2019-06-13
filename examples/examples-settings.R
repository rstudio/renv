
# check the 'ignored.packages' option
renv::settings$ignored.packages()

# ignore the 'tidyverse' package in this project
renv::settings$ignored.packages("tidyverse", persist = FALSE)

# cache is turned on by default
renv::settings$use.cache()

# but we can disable it by default
options(renv.config.use.cache = FALSE)
renv::settings$use.cache()

# enable for current project
renv::settings$use.cache(TRUE, persist = FALSE)
renv::settings$use.cache()
