# -------------------------------------------------------------------
show_message <- function(msg) {
  message(msg)
  shiny::showNotification(msg, id = "status")
}

#----------------------------------------------------------------
make_setwd_expr <- function(path) {
  sprintf("setwd('%s')", path)
}

# -------------------------------------------------------------------
load_config <- function(env, filename) {
  if (file.exists(filename)) {
    show_message("loading configuration")
    source(filename, local = env)
  }
}

# -------------------------------------------------------------------
save_config <- function(module, env, filename) {
  config_data <- c(
    list(header = sprintf("# %s configuration\n", module)),
    sapply(names(env), function(i, e) {
      return(sprintf("%s = \"%s\"", i, as.list(e)[[i]]))
    }, env)
  )

  show_message("saving configuration")
  writeLines(paste(config_data, collapse = "\n"), filename)
}

#----------------------------------------------------------------
update_startup_file <- function(config, filename) {
  if (file.exists(filename))
    return(0)

  message(paste("updating startup file", filename))

  startup_script <- c(
    "# ---- created by sppsetup::courseware_setup() on %s",
    "#",
    "# set working directory if the R session is started in interactive mode\n",
    "if (interactive()) {",
    "  message(sprintf('setting working directory to %s'))",
    "  %s",
    "}\n"
  )

  writeLines(sprintf(paste(startup_script, collapse = "\n"),
                     format(Sys.time(), "%b %d, %Y %H:%M %Z"),
                     config$working_dir,
                     make_setwd_expr(config$working_dir)),
             filename)
}

#----------------------------------------------------------------
verify_working_dir <- function(path) {
  if (dir.exists(path))
    return(0)

  show_message(paste("creating", path))
  if (!dir.create(path))
    stop(paste("Failed to create", path))

  if(!dir.exists(path))
    stop(paste(path, "does not exist"))
}

#----------------------------------------------------------------
verify_packages <- function(packages) {
  installed_packages <- rownames(utils::installed.packages())

  package_installer <- function(installer, packages, quiet = FALSE) {
    if (length(packages)) {
      show_message(paste("installing", paste(packages, collapse = ", ")))
      installer(packages, quiet = quiet)
    }
  }

  package_installer(utils::install.packages, setdiff(packages$cran, installed_packages))

  package_installer(devtools::install_github, packages$github)
}

#----------------------------------------------------------------
sppsetup <- function(module_id, replace) {
  config_env <- new.env()
  startup_file <- path.expand("~/.Rprofile")

  show_message("setting up courseware...")

  module <- get_module_config(module_id)

  config_env$working_dir <- module$working_dir

  show_message("Starting setup, please wait...")

  load_config(config_env, module$config_file)

  verify_working_dir(module$working_dir)

  verify_packages(module$packages)

  download_datasets(module$datasets$url, module$datasets$collection, module$working_dir, replace)

  save_config(module$name, config_env, module$config_file)

  update_startup_file(config_env, startup_file)

  hrule <- paste0("\n", paste(rep("*", 64), collapse = ""), "\n")
  cat(paste0(hrule, "IMPORTANT: Read this carefully", hrule))
  cat("\nEvery script you write must include the following line at the top:\n\n")
  message(make_setwd_expr(config_env$working_dir))
  cat("\nWrite it down or save it in a place where you can easily find it.\n\n")
}

