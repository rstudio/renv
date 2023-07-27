# notice() creates bulleted list with optional postamble

    Code
      notice("preamble", letters[1:3])
    Output
      preamble
      - a
      - b
      - c
      
    Code
      notice("preamble", letters[1:3], postamble = "after")
    Output
      preamble
      - a
      - b
      - c
      after
      

