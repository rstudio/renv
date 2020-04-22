
# capture these
pacman::p_load(a)
pacman::p_load("b")
p_load(c, "d", e)
p_load(char = c("f", "g", "h"))
pacman::p_load(char = "i")

# don't capture these
pacman::p_load(A, character.only = TRUE)
p_load(char = B)
