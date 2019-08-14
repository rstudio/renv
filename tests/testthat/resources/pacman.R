# capture these
pacman::p_load(a)
pacman::p_load("b")
p_load(c, "d", e)
p_load(char = c("f", "g", "h"))

# don't capture these
pacman::p_load(A, character.only = TRUE)
