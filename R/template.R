
renv_template_create <- function(template) {
  gsub("^\\n+|\\n+$", "", template)
}

renv_template_replace <- function(text, replacements, format = "${%s}") {

  enumerate(replacements, function(key, value) {
    key <- sprintf(format, key)
    text <<- gsub(key, value, text, fixed = TRUE)
  })

  text

}
