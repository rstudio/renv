# bulletin() creates bulleted list with optional postamble

    Code
      bulletin("preamble", letters[1:3])
    Output
      preamble
      - a
      - b
      - c
      
    Code
      bulletin("preamble", letters[1:3], postamble = "after")
    Output
      preamble
      - a
      - b
      - c
      after
      

