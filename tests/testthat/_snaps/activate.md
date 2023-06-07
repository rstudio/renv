# activate_prompt behaves as expected

    Code
      val <- renv_activate_prompt_impl("snapshot")
    Output
      It looks like you've called renv::snapshot() in a project that hasn't been activated yet
      How would you like to proceed?
      
      1: Activate the project and use the project library.
      2: Do not activate the project and use the current library paths.
      3: Cancel and resolve the situation another way.
      
      Selection: 2
      

---

    Code
      val <- renv_activate_prompt_impl("snapshot")
    Output
      It looks like you've called renv::snapshot() in a project that hasn't been activated yet
      How would you like to proceed?
      
      1: Activate the project and use the project library.
      2: Do not activate the project and use the current library paths.
      3: Cancel and resolve the situation another way.
      
      Selection: 1
      

