#' Download CRSP and SPGMI Data
#'
#' @description Downloads stocksCRSPweekly, stocksCRSPdaily
#'
#' @param dataset a valid dataset name (see details)
#' @param cache logical variable controlling whether or not to cache the data so
#' that when calling the function for the same dataset it will be loaded from cache
#' rather than re-downloading from the github site
#' @param refresh logical variable controlling whether or not to re-download a
#' cached dataset
#'
#' @details The following are valid names of datasets available:
#' \describe{
#' \item{"stocksCRSPdaily"}{"Details same as for stocksCRSP except now daily"}
#' \item{"stocksCRSPweekly"}{"Details same as for stocksCRSP except now weekly"}
#' }
#' User must install R.cache package
#' @return An object of class \dQuote{data.table}. If the download fails,
#' e.g., because the internet resource is temporarily unavailable, the
#' function returns \code{NULL} invisibly, with an informative message.
#' @aliases getPCRAData
#' @rdname getPCRAData
#' @export
#'
#' @examples
#' \donttest{
#' stocksCRSPweekly <- getPCRAData(dataset = "stocksCRSPweekly")
#' class(stocksCRSPweekly)
#' names(stocksCRSPweekly)
#'
#' stocksCRSPdaily <- getPCRAData(dataset = "stocksCRSPdaily")
#' class(stocksCRSPdaily)
#' names(stocksCRSPdaily)
#' }
getPCRAData <- function(dataset = "stocksCRSPweekly", cache = TRUE, refresh = FALSE)
{
  valid_datasets <- c("stocksCRSP", "stocksCRSPdaily", "stocksCRSPweekly")
  dataset <- match.arg(dataset[1], valid_datasets)
  base_url <- "https://github.com/robustport/PCRA/raw/main/DataPlus/"
  data_url <- paste(base_url,dataset,".rds", sep = "")
  key <- list(data_url)
  if (!refresh) {
    data <- loadCache(key)
    if (is.null(data)) {
      data <- downloadPCRAData(data_url)
      if (is.null(data)) return(invisible(NULL))
      if (cache) saveCache(data, key = key)
    }
    return(data)
  } else {
    data <- downloadPCRAData(data_url)
    if (is.null(data)) return(invisible(NULL))
    if (cache) saveCache(data, key = key)
    return(data)
  }
}

# Download an rds file from data_url and return the object it contains.
# Fails gracefully per CRAN policy: any download or read failure results
# in an informative message and a NULL return value, never an error.
downloadPCRAData <- function(data_url)
{
  temp <- tempfile(pattern = paste("foo", Sys.getpid(), sep = "")) #create temp file
  on.exit(unlink(temp))
  status <- try(suppressWarnings(download.file(data_url, temp, quiet = TRUE)),
                silent = TRUE)
  if (inherits(status, "try-error") || status != 0L ||
      !file.exists(temp) || file.size(temp) == 0) {
    message("Download of ", data_url, " failed. The internet resource may be ",
            "temporarily unavailable; please try again later.")
    return(NULL)
  }
  data <- try(suppressWarnings(readRDS(temp)), silent = TRUE)
  if (inherits(data, "try-error")) {
    message("The file downloaded from ", data_url, " could not be read. ",
            "The internet resource may be temporarily unavailable; ",
            "please try again later.")
    return(NULL)
  }
  data
}
