#' Disable tracking python dependencies and remove associated files
#'
#' `remove_python` disables `renv` python dependency tracking and removes the
#' associated directories and files used by `renv`.
#'
#' @inherit renv-params
#' @param python_dir Remove the directory containing the Python environment and packages?
#' @param requirements_file Remove the Python requirements file?
#' @param prompt Prompt user before performing each action?
#'
#' @examples
#' \dontrun{
#' remove_python()
#' }
#'
#'
#' @export
remove_python <- function(
    project = NULL,
    python_dir = TRUE,
    requirements_file = TRUE,
    prompt = interactive()
)
{
  flag <- !prompt || askYesNo("Stop renv from tracking Python version and packages?")
  if(is.na(flag))
    return()
  else if (isTRUE(flag))
  {
    renv_python_deactivate(project)
  }

  if(python_dir)
  {
    python_path <- renv_paths_renv('python') |> renv_path_canonicalize()
    flag <- !prompt || askYesNo(paste0("Remove '", python_path, "' and its contents?"))
    if(is.na(flag))
      return()
    else if (isTRUE(flag))
      unlink(python_path, recursive = TRUE)
  }

  if(requirements_file)
  {
    req_path <- renv_paths_renv('../requirements.txt') |> renv_path_canonicalize()
    flag <- !prompt || askYesNo(paste0("Remove '", req_path, "'?"))
    if(is.na(flag))
      return()
    else if (isTRUE(flag))
      unlink(python_path, recursive = TRUE)
  }

}
