# renv_pretty_print() creates bulleted list with optional postamble

    Code
      renv_pretty_print("preamble", letters[1:3])
    Output
      preamble
      
      - a
      - b
      - c
      
    Code
      renv_pretty_print("preamble", letters[1:3], postamble = "after")
    Output
      preamble
      
      - a
      - b
      - c
      
      after
      

