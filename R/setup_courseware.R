#' Setup Courseware
#'
#' \code{setup_courseware} Setup Courseware
#'
#' @export
setup_courseware <- function() {
  R_required <- "3.4.1"
  RStudio_required <- "1.0.153"
  messages <- shiny::reactiveValues(status_message="")

  set_status <- function(msg) {
    messages$status_message <- paste(msg, collapse = "\n")
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Setup Courseware", left = NULL),
    miniUI::miniContentPanel(
      shiny::verbatimTextOutput("r_version", placeholder = TRUE),
      shiny::verbatimTextOutput("error_message", placeholder = FALSE),
      shiny::tags$style(type='text/css', '#error_message {color: red;}'),
      shiny::selectInput("module", "Module:", get_module_names()),
      shiny::checkboxInput("replace", label = "Update all datasets", value = FALSE),
      shiny::actionButton("setup", "Setup Courseware"),
      shiny::hr(),
      shiny::verbatimTextOutput("status_message", placeholder = FALSE),
      shiny::tags$style(type='text/css', '#status_message {color: green;}'),
      padding = 10, scrollable = TRUE)
  )

  killapp <- function() {
    invisible(shiny::stopApp())
  }

  show_modal_dialog <- function(title, message) {
    shiny::showModal(shiny::modalDialog(title = title, message))
  }

  server <- function(input, output, session) {
    output$r_version <- shiny::renderText({
      return(sprintf("%s\nRStudio version: %s", R.version.string, rstudioapi::getVersion()))
    })

    output$error_message <- shiny::renderText({
      obsolete_message <- "This version of %s is too old. Please\ndownload the latest from:\n  %s"

      if (utils::compareVersion(paste(R.version$major, R.version$minor, sep = "."), R_required) < 0)
        return(sprintf(obsolete_message, "R", "https://cran.r-project.org"))

      if (!rstudioapi::isAvailable(RStudio_required))
        return(sprintf(obsolete_message, "RStudio", "https://www.rstudio.com"))

      return("")
    })

    output$status_message <- shiny::renderText(messages$status_message)

    shiny::observeEvent(input$done, {
      killapp()
    })

    shiny::observeEvent(input$setup, {
      set_status("Do not close this window until the setup is complete.")
      tryCatch({
        sppsetup(input$module, input$replace)
        set_status("Done. You can close this window now.")
        show_message(NULL)
        show_modal_dialog("Courseware Setup Complete", "You can run the setup again anytime from the 'Addins' menu in RStudio")
      },
        error = function(e) { set_status(e) },
        warning = function(e) { set_status(e) }
      )
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 400, height = 380))
}
