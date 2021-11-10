
renv_yaml_load <- function(text) {

  yaml::yaml.load(
    string = text,
    eval.expr = FALSE,
    handlers = list(
      r = function(yaml) {
        attr(yaml, "type") <- "r"
        yaml
      }
    )
  )

}
