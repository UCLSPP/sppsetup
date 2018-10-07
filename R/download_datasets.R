#' Download datasets
#'
#' \code{download_datasets} Download datasets
#'
#' @param url URL of dataset repository
#' @param collection filename of collection under the dataset repository
#' @param target target directory for downloading
#' @param replace a boolean flag indicating whether to replace existing datasets
#' @param method a method argument passed to download.file
#'
#' @export
download_datasets <- function(url, collection, target, replace = FALSE, method = "auto") {
  collection_filename <- file.path(url, collection)
  cat(paste(systime(), "reading collection from", collection_filename, "\n"))

  collection_data <- utils::read.csv(collection_filename,
                                     strip.white = TRUE,
                                     stringsAsFactors = FALSE)

  for (i in 1:nrow(collection_data)) {
    filename <- basename(collection_data$path[i])
    source_path <- file.path(url, collection_data$path[i])
    target_path <- file.path(target, filename)

    if (file.exists(target_path) && !replace)
      next

    cat(paste(systime(), "  ", filename, "\n"))
    utils::download.file(source_path, target_path, method = method, quiet = FALSE)
  }
}
