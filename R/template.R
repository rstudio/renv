
renv_template_replace <- function(text, replacements) {

  enumerate(replacements, function(key, value) {
    key <- sprintf("${%s}", key)
    text <<- gsub(key, value, text, fixed = TRUE)
  })

  text

}
