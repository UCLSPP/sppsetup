#----------------------------------------------------------------
get_modules <- function() {
  description_filename <- "https://raw.githubusercontent.com/UCLSPP/sppsetup/master/DESCRIPTION"
  description <- yaml::yaml.load_file(description_filename)
  return(unlist(description$Modules))
}

#----------------------------------------------------------------
get_module_config <- function(module) {
  yaml::yaml.load_file(module)
}
