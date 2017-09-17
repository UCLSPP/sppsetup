#' Download datasets
#'
#' \code{download_datasets} Download datasets
#'
#' @param url URL of dataset repository
#' @param collection filename of collection under the dataset repository
#' @param target target directory for downloading
#' @param replace a boolean flag indicating whether to replace existing datasets
#'
#' @export
download_datasets <- function(url, collection, target, replace = FALSE) {
  message("downloading datasets")

  collection_filename <- file.path(url, collection)
  message(paste("  reading collection from", collection_filename))

  collection_data <- utils::read.csv(collection_filename,
                                     strip.white = TRUE,
                                     stringsAsFactors = FALSE)

  for (i in 1:nrow(collection_data)) {
    filename <- basename(collection_data$path[i])
    source_path <- file.path(url, collection_data$path[i])
    target_path <- file.path(target, filename)

    if (file.exists(target_path) && !replace)
      next

    message(sprintf("    %s", filename))
    utils::download.file(source_path, target_path, quiet = TRUE)
  }
}
