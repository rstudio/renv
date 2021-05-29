
renv_addins_embed_ui <- function() {

  miniUI::miniPage(
    miniUI::gadgetTitleBar("Embed a Lockfile"),
    miniUI::miniContentPanel(
      shiny::verticalLayout(
        shiny::fileInput(
          inputId     = "lockfile",
          label       = "Lockfile path:",
          placeholder = "(Use default)"
        )
      )
    )
  )

}

renv_addins_embed_server <- function(input, output, session) {

  shiny::observeEvent(input$done, {

    # notify the user that we're working now
    progress <- shiny::Progress$new(
      session = shiny::getDefaultReactiveDomain(),
      style   = "notification"
    )

    progress$set(message = "Embedding lockfile...")

    # get editor context
    context <- rstudioapi::getSourceEditorContext()

    # validate we have a path
    path <- context$path
    if (!nzchar(path))
      stop("cannot embed lockfile into an unsaved file", call. = FALSE)

    # get project path
    project <- rstudioapi::getActiveProject()

    # read lockfile
    lockfile <- input$lockfile
    if (!is.null(lockfile))
      lockfile <- renv_lockfile_read(file = lockfile$datapath)

    # save document and run embed
    rstudioapi::documentSave(id = context$id)

    renv::embed(path     = path,
                lockfile = lockfile,
                project  = project)

    # stop app
    invisible(shiny::stopApp())

  })

}

renv_addins_embed <- function() {

  # first, check that shiny and miniUI are available
  for (package in c("miniUI", "rstudioapi", "shiny")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      fmt <- "required package '%s' is not available"
      stopf(fmt, package)
    }
  }

  # ask the user to save the document first if necessary
  context <- rstudioapi::getSourceEditorContext()
  if (!nzchar(context$path))
    stop("this addin cannot be run with an unsaved document")

  # okay, we can run the addin
  shiny::runGadget(
    app    = renv_addins_embed_ui(),
    server = renv_addins_embed_server,
    viewer = shiny::dialogViewer(
      dialogName = "Embed Lockfile",
      width  = 400,
      height = 200
    )
  )
}

