#' Setup Courseware
#'
#' \code{setup_courseware} Setup Courseware
#'
#' @export
setup_courseware <- function() {
  R_required <- "3.3.1"
  RStudio_required <- "0.99.903"

  colors <- list(
    default = "black",
    success = "green",
    error = "red"
  )

  get_startup_status <- function() {

    old_component_error <- function(component, url, color) {
      msg <- htmltools::tags$p(
        "This version of", component, "is too old. Please download the latest from:",
        htmltools::tags$ul(
          htmltools::tags$li(
            htmltools::a(url, href=url)
          )
        )
      )
      list(msg = msg, color = colors$error)
    }

    if (utils::compareVersion(paste(R.version$major, R.version$minor, sep = "."), R_required) < 0)
      return(old_component_error("R", "https://cran.r-project.org"))

    if (!rstudioapi::isAvailable(RStudio_required))
      return(old_component_error("RStudio", "https://www.rstudio.com"))

    return(list(msg = "", color = colors$default))
  }

  startup_status <- get_startup_status()
  status <- shiny::reactiveValues(msg = startup_status$msg, color = startup_status$color)
  module <- shiny::reactiveValues(config = NULL)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Setup Courseware", left = NULL),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Setup",
        icon = shiny::icon("cogs"),
        miniUI::miniContentPanel(
          shiny::verbatimTextOutput("r_version", placeholder = TRUE),
          shiny::selectInput("module", "Module:", modules),
          shiny::checkboxInput("replace", label = "Update all datasets", value = FALSE),
          shiny::actionButton("setup", "Setup Courseware"),
          shiny::hr(),
          shiny::htmlOutput("status"),
          padding = 10, scrollable = TRUE)
      ),
      miniUI::miniTabPanel(
        "Help",
        icon = shiny::icon("info"),
        miniUI::miniContentPanel(
          shiny::htmlOutput("help"),
          shiny::hr(),
          shiny::textInput("name", "Name"),
          shiny::textInput("email", "Email"),
          shiny::actionButton("send_report", "Send Error Report")
        )
      )
    )
  )

  killapp <- function(msg = NULL) {
    if (!is.null(msg))
      message(msg)
    invisible(shiny::stopApp())
  }

  show_modal_dialog <- function(title, message) {
    shiny::showModal(shiny::modalDialog(title = title, message))
  }

  server <- function(input, output, session) {
    logfile <- tempfile(pattern = "sppsetup_")

    output$r_version <- shiny::renderText({
      return(sprintf("%s\nRStudio version: %s", R.version.string, rstudioapi::getVersion()))
    })

    output$status <- shiny::renderText(sprintf("<font color='%s'>%s</font>", status$color, status$msg))

    set_status <- function(msg, color, error = FALSE) {
      status$msg <- paste(msg, collapse = "<br>")
      status$color <- color
      if (error)
        message(msg)
      show_message(NULL)
    }

    output$help <- shiny::renderText(
      return("You can submit an error report if you encounter any problems during setup.")
    )

    set_success <- function(msg) { set_status(msg, colors$success) }
    set_error <- function(msg) { set_status(msg, colors$error, TRUE) }

    shiny::observeEvent(input$module, {
      module$config <- yaml::yaml.load_file(input$module)
    })

    shiny::observeEvent(input$done, {
      killapp()
    })

    shiny::observeEvent(input$setup, {
      tryCatch({
          sppsetup(module$config, input$replace, logfile)
          set_success("Setup complete. You can close this window now.")
          show_modal_dialog("Courseware Setup Complete", "You can run the setup again anytime from the 'Addins' menu in RStudio")
        },
        error = function(e) { set_error(e$message) },
        warning = function(e) { set_error(e$message) },
        finally = function(e) { sink() }
      )
    })

    shiny::observeEvent(input$send_report, {
      dsn <- module$config$sentry$url

      dsn <- "https://29845a7c0f4b4ebf929a1cbcac3be9a4:b8ed650559d84a96845361bbddd4b6dd@sentry.io/219443"
      send_report(dsn,
                  logfile = logfile,
                  user = list(name = input$name, email = input$email),
                  exception = list(type = "Diagnostic Report", message = status$msg))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 400, height = 420))
}
