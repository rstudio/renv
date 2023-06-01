# renv_pretty_print() creates bulleted list with optional preamble/postable

    Code
      renv_pretty_print(letters[1:3])
    Output
      - a
      - b
      - c
      
    Code
      renv_pretty_print(letters[1:3], preamble = "before")
    Output
      before
      - a
      - b
      - c
      
    Code
      renv_pretty_print(letters[1:3], postamble = "after")
    Output
      - a
      - b
      - c
      after
      

