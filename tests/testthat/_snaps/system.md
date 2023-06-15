# system errors are reported as expected

    Code
      . <- renv_system_exec(command = R(), args = c("--vanilla", "-s", "-e",
        renv_shell_quote("stop('barf')")), quiet = FALSE)
    Error <error>
      error executing command [error code 1]
      <R> --vanilla -s -e "stop('barf')"
      ==============================================================================
      
      Error: barf
      Execution halted

