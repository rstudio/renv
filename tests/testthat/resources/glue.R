
glue(
  "{library(A)}",
  "{ library(B) }"
)

glue(
  "<< <{{ library(C) }}> >>",
  .open  = "<",
  .close = ">"
)

glue(
    "{{ {library(D); { library(E) }; library(F)} }}"
)

glue(
  "Include: [[ library(G) ]]; Not included: [[[[ library(h) ]]]]",
  .open  = "[[",
  .close = "]]"
)
