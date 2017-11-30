#' Download datasets
#'
#' \code{download_datasets} Download datasets
#'
#' @param url URL of dataset repository
#' @param collection filename of collection under the dataset repository
#' @param target target directory for downloading
#' @param method a method argument passed to download.file
#' @param replace a boolean flag indicating whether to replace existing datasets
#'
#' @export
download_datasets <- function(url, collection, target, method = "auto", replace = FALSE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  collection_filename <- file.path(url, collection)
  cat(paste(timestamp, "reading collection from", collection_filename, "\n"))

  collection_data <- utils::read.csv(collection_filename,
                                     strip.white = TRUE,
                                     stringsAsFactors = FALSE)

  for (i in 1:nrow(collection_data)) {
    filename <- basename(collection_data$path[i])
    source_path <- file.path(url, collection_data$path[i])
    target_path <- file.path(target, filename)

    if (file.exists(target_path) && !replace)
      next

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste(timestamp, "  ", filename, "\n"))
    utils::download.file(source_path, target_path, method = method, quiet = TRUE)
  }
}
