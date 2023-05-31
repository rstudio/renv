# activate_prompt behaves as expected

    Code
      val <- renv_activate_prompt_impl()
    Output
      This project has not yet been activated.
      What do you want to do?
      
      1: Activate the project, setting up a project library
      2: Continue, using the system library
      3: Cancel
      
      Selection: 2

---

    Code
      val <- renv_activate_prompt_impl()
    Output
      This project has not yet been activated.
      What do you want to do?
      
      1: Activate the project, setting up a project library
      2: Continue, using the system library
      3: Cancel
      
      Selection: 1

