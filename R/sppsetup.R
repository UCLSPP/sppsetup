#----------------------------------------------------------------
sppsetup <- function(module, replace, logfile) {
  sink(logfile, split = TRUE)

  show_message("setting up courseware")

  show_message(paste("writing log file:", logfile))

  verify_working_dir(module$working_dir)

  show_message("downloading datasets")

  download_datasets(module$datasets$url,
                    module$datasets$collection,
                    module$working_dir,
                    replace = replace)

  params <- module[names(module) %in% c("working_dir")]
  save_config(module$name, params, module$config_file)

  install_packages(module$packages)

  hrule <- paste0("\n", paste(rep("*", 64), collapse = ""), "\n")
  cat(paste0(hrule, "IMPORTANT: Read this carefully", hrule))
  cat("\nEvery script you write must include the following line at the top:\n\n")
  message(make_setwd_expr(module$working_dir))
  cat("\nWrite it down or save it in a place where you can easily find it.\n\n")

  verify_packages(module$packages)
}

# -------------------------------------------------------------------
show_message <- function(msg) {
  id <- "status"

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (is.null(msg))
    shiny::removeNotification(id = id)
  else {
    cat(paste(timestamp, msg, "\n"))
    shiny::showNotification(msg, id = id, duration = NULL)
  }
}

#----------------------------------------------------------------
make_setwd_expr <- function(path) {
  sprintf("setwd('%s')", path)
}

# -------------------------------------------------------------------
load_module <- function(filename) {
  module_url <- url(filename)
  module <- yaml::yaml.load_file(module_url)
  close(module_url)

  config <- load_config(module$config_file)
  if ("working_dir" %in% names(config)) {
    module$working_dir <- config$working_dir
  }
  return(module)
}

# -------------------------------------------------------------------
load_config <- function(filename) {
  env <- new.env()

  if (file.exists(filename)) {
    show_message(paste("loading configuration", filename))
    source(filename, local = env)
    show_message(NULL)
  }
  return(as.list(env))
}

# -------------------------------------------------------------------
save_config <- function(name, params, filename) {
  config_data <- c(
    list(header = sprintf("# %s configuration\n", name)),
    sapply(names(params), function(i, e) {
      return(sprintf("%s = \"%s\"", i, as.list(e)[[i]]))
    }, params)
  )

  show_message(paste("saving configuration", filename))
  writeLines(paste(config_data, collapse = "\n"), filename)
}

#----------------------------------------------------------------
verify_working_dir <- function(path) {
  show_message(paste("verifying", path))

  if (dir.exists(path))
    return(0)

  show_message(paste("creating", path))
  if (!dir.create(path))
    stop(paste("failed to create", path))

  if(!dir.exists(path))
    stop(paste(path, "does not exist"))
}

#----------------------------------------------------------------
system_exec_r <- function(expr) {
  sys_command <- paste(Sys.which("Rscript"), "-e", shQuote(expr))
  show_message("running system command")
  cat(paste(sys_command, "\n"))
  system(sys_command)
}

#----------------------------------------------------------------
rscript_exec <- function(expr) {
  rscript <- Sys.which("Rscript")
  args <- paste("-e", shQuote(expr))
  show_message("running system command")
  cat(paste(rscript, args, "\n"))
  cat(paste(system2(rscript, args, stdout = TRUE, stderr = TRUE), collapse = "\n"))
  cat("\n")
}

#----------------------------------------------------------------
install_packages <- function(packages) {
  installed_packages <- rownames(utils::installed.packages())

  package_installer(utils::install.packages,
                    setdiff(packages$cran, installed_packages),
                    list(repos = shQuote("http://cran.us.r-project.org")))

  if (!is.null(packages$github)) {
    github_packages <- stats::setNames(packages$github,
                                       sapply(strsplit(packages$github, "/"), utils::tail, n=1))

    installed_github_packages <- intersect(names(github_packages), installed_packages)
    missing_github_packages <- github_packages[setdiff(names(github_packages), installed_github_packages)]

    package_installer(devtools::install_github, missing_github_packages)
    package_installer(devtools::update_packages, unique(installed_github_packages))
  }
}

#----------------------------------------------------------------
package_installer <- function(installer, packages, args = NULL) {
  if (!length(packages))
    return()

  show_message(paste("installing/updating packages:", paste(packages, collapse = ", ")))

  package_list <- sprintf("c(%s)", paste(shQuote(packages), collapse = ", "))

  args <- paste(sapply(names(args), function(x) sprintf("%s = %s", x, args[[x]])),
                collapse = ", ")

  arg_list <- paste(setdiff(c(package_list, args), ""), collapse = ", ")

  install_expr <- sprintf("%s(%s)", deparse(substitute(installer)), arg_list)

  cat(paste0("\n", install_expr, "\n\n"))
  rscript_exec(install_expr)
}

#----------------------------------------------------------------
verify_packages <- function(packages) {
  installed_packages <- rownames(utils::installed.packages())
  package_list <- sapply(unlist(packages),
                         function(s) utils::tail(unlist(strsplit(s, "/")), n=1))

  missing_packages <- setdiff(package_list, installed_packages)
  if (length(missing_packages))
    stop(c("Error: missing packages: ", paste(missing_packages, collapse=", ")))
}

