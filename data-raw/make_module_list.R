description <- yaml::yaml.load_file("DESCRIPTION")
modules <- unlist(description$Modules)
devtools::use_data(modules, internal = TRUE)
