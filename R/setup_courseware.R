#' Setup Courseware
#'
#' \code{setup_courseware} Setup Courseware
#'
#' @export
setup_courseware <- function() {
  R_required <- "3.4.4"
  RStudio_required <- "1.1.287"

  colors <- list(
    default = "black",
    success = "green",
    error = "red"
  )

  get_startup_status <- function() {

    old_component_error <- function(component, label, url, color) {
      msg <- htmltools::tags$p(
        "This version of", component, "is too old. Please download the latest from:",
        htmltools::tags$ul(
          htmltools::tags$li(
            htmltools::a(label, href=url)
          )
        )
      )
      list(msg = msg, color = colors$error)
    }

    if (utils::compareVersion(paste(R.version$major, R.version$minor, sep = "."), R_required) < 0)
      return(old_component_error("R",
                                 "CRAN",
                                 "https://cran.r-project.org"))

    if (!rstudioapi::isAvailable(RStudio_required))
      return(old_component_error("RStudio",
                                 "RStudio.com",
                                 "https://www.rstudio.com/products/rstudio/download/#download"))

    return(list(msg = "", color = colors$default))
  }

  startup_status <- get_startup_status()
  status <- shiny::reactiveValues(msg = startup_status$msg, color = startup_status$color)
  options <- shiny::reactiveValues(module = NULL)
  title <- paste("Courseware Setup", packageVersion("sppsetup"))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title, left = NULL, right = NULL),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Setup",
        icon = shiny::icon("cog"),
        miniUI::miniContentPanel(
          shiny::verbatimTextOutput("r_version", placeholder = TRUE),
          shiny::selectInput("module", "Module:", modules),
          shiny::fillRow(
            shiny::tags$b("Directory: "),
            shiny::uiOutput("working_dir_selector"),
            flex = c(2, 8),
            height = 20
          ),
          shiny::checkboxInput("replace", label = "Update all datasets", value = FALSE),
          shiny::actionButton("setup", "Continue",
                              icon = shiny::icon("cloud-download"),
                              class = "btn-success"),
          shiny::actionButton("exit", "Exit",
                              icon = shiny::icon("times-circle"),
                              class = "btn-danger"),

          shiny::hr(),
          shiny::htmlOutput("status"),
          padding = 10, scrollable = TRUE)
      ),
      miniUI::miniTabPanel(
        "Help",
        icon = shiny::icon("info-circle"),
        miniUI::miniContentPanel(
          shiny::htmlOutput("help"),
          shiny::hr(),
          shiny::textInput("name", "Name"),
          shiny::textInput("email", "Email"),
          shiny::actionButton("send_report", "Send Error Report",
                              icon = shiny::icon("share-square"),
                              style = "color: black",
                              class = "btn-warning")
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

    output$working_dir_selector <- shiny::renderUI({
      if (rstudioapi::isAvailable(RStudio_required))
        shiny::actionLink("working_dir", options$module$working_dir)
      else
        return(options$module$working_dir)
    })

    output$status <- shiny::renderText(sprintf("<font color='%s'>%s</font>", status$color, status$msg))

    set_status <- function(msg, color = colors$default, error = FALSE) {
      status$msg <- paste(msg, collapse = "<br>")
      status$color <- color
      if (error)
        message(msg)
      show_message(NULL)
    }

    output$help <- shiny::renderText(
      return("You can submit an error report if you encounter any problems during setup.")
    )

    clear_status <- function() { set_status(NULL) }
    set_success <- function(msg) { set_status(msg, colors$success) }
    set_error <- function(msg) { set_status(msg, colors$error, TRUE) }

    shiny::observeEvent(input$module, {
      options$module <- load_module(input$module)
    })

    shiny::observeEvent(input$working_dir, {
      verify_working_dir(options$module$working_dir)
      show_message(NULL)
      dir <- rstudioapi::selectDirectory("Select Directory",
                                         label = "Select",
                                         path = options$module$working_dir)
      if (!is.null(dir))
        options$module$working_dir <- dir
    })

    shiny::observeEvent(input$exit, {
      killapp()
    })

    shiny::observeEvent(input$setup, {
      tryCatch({
          clear_status()
          sppsetup(options$module, input$replace, logfile)
          set_success("Setup complete. You can close this window now.")
          message <- shiny::HTML(
            "You can run the setup again anytime from the <strong>Addins</strong> menu in RStudio."
          )
          show_modal_dialog(paste(title, "Complete"), message)
        },
        error = function(e) { set_error(e$message) },
        warning = function(e) { set_error(e$message) },
        finally = function(e) { sink() }
      )
    })

    shiny::observeEvent(input$send_report, {
      dsn <- options$module$sentry$url
      dsn <- "https://29845a7c0f4b4ebf929a1cbcac3be9a4:b8ed650559d84a96845361bbddd4b6dd@sentry.io/219443"
      send_report(dsn,
                  logfile = logfile,
                  user = list(name = input$name, email = input$email),
                  exception = list(type = "Diagnostic Report", message = status$msg))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 420, height = 480))
}

