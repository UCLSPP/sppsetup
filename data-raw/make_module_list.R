modules <- list(
  PUBL0055 = "https://uclspp.github.io/PUBL0055/config.yml",
  PUBLG100 = "https://uclspp.github.io/PUBLG100/config.yml"
)

devtools::use_data(modules, internal = TRUE, overwrite = TRUE)
