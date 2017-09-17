#----------------------------------------------------------------
modules <- c(
  "https://uclspp.github.io/PUBLG100/config.yml"
)

#----------------------------------------------------------------
get_module_names <- function() {
  module_names <- sapply(modules, function(filename) {
    yaml::yaml.load_file(filename)$name
  }, USE.NAMES = TRUE)
  names(module_names) <- module_names
  module_names[] <- 1:length(module_names)
  return(module_names)
}


#----------------------------------------------------------------
get_module_config <- function(id) {
  yaml::yaml.load_file(modules[as.integer(id)])
}
