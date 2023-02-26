#' Download CRSP and SPGMI Data
#'
#' @description Downloads stocksCRSPweekly, stocksCRSPdaily, factorsSPGMI
#' 
#' @param dataset a valid dataset name (see details)
#' @param cache logical variable controlling whether or not to cache the data so
#' that when calling the function for the same dataset it will be loaded from cache
#' rather than re-downloading from the github site
#' @param refresh logical variable controlling whether or not to re-download a
#' cached dataset
#' 
#' @details The following are valid names of datasets available:
#' \itemize{
#' \item{"stocksCRSPdaily"}{"Details same as for stocksCRSP except now daily"}
#' \item{"stocksCRSPweekly"}{"Details same as for stocksCRSP except now weekly"}
#' \item{"factorsSPGMI"}{"Details same as for factprsSPGMI"}
#' }
#' User must install R.cache package
#' @return An object of class \dQuote{data.table}.
#' @aliases getPCRAData
#' @rdname getPCRAData
#' @export
#'
#' @examples
#' \dontrun{
#' stocksCRSPweekly <- getPCRAData(data = stocksCRSPweekly)
#' class(stocksCRSPweekly)
#' names(stocksCRSPweekly)
#' 
#' stocksCRSPdaily <- getPCRAData(data = stocksCRSPdaily)
#' class(stocksCRSPdaily)
#' names(stocksCRSPdaily)
#' 
#' factorsSPGMI <- getPCRAData(data = factorsSPGMI)
#' class(factorsSPGMI)
#' names(factorsSPGMI)
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
      temp <- tempfile(pattern = paste("foo", Sys.getpid(), sep = "")) #create temp file
      x <- try(download.file(data_url,temp, quiet = TRUE), silent = TRUE)
      data <- readRDS(temp)
      unlink(temp)
    }
    return(data)
  } else {
    temp <- tempfile(pattern = paste("foo", Sys.getpid(), sep = "")) #create temp file
    x <- try(download.file(data_url,temp, quiet = TRUE), silent = TRUE)
    data <- readRDS(temp)
    unlink(temp)
    if (cache) saveCache(data, key = key)
    return(data)
  }
}
