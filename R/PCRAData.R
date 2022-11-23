#' PCRA Sample Datasets
#'
#' @description Downloads PCRA sample datasets
#' @param dataset a valid dataset name (see details)
#' @param cache whether to cache the operation so that when calling the function
#' using the same dataset it will be loaded from cache rather than re-downloading
#' @param refresh whether to re-download a cached dataset
#' @details The following are valid names of datasets available:
#' \itemize{
#' \item{"stocksCRSP"}{"Add Description"}
#' \item{"stocksCRSPdaily"}{"Add Description"}
#' \item{"stocksCRSPweekly"}{"Add Description"}
#' \item{"crsp.returns8"}{"Add Description"}
#' \item{"crsp.short"}{"Add Description"}
#' \item{"factorsSPGMI"}{"Add Description"}
#' \item{"FRBrates1934to2014"}{"Add Description"}
#' \item{"smallcapM"}{"Add Description"}
#' \item{"smallcapW"}{"Add Description"}
#' \item{"largecapM"}{"Add Description"}
#' \item{"largecapW"}{"Add Description"}
#' \item{"normalVsHectic"}{"Add Description"}
#' \item{"ret4withOutliers"}{"Add Description"}
#' \item{"strategies"}{"Add Description"}
#' \item{"WeeklyReturns1980"}{"Add Description"}
#' \item{"wtsGmvLS"}{"Add Description"}
#' \item{"gfunds5"}{"Add Description"}
#' \item{"invensysEPS"}{"Add Description"}
#' }
#' @return An object of class \dQuote{data.table}.
#' @aliases getPCRAData
#' @rdname getPCRAData
#' @export
#'
#'
getPCRAData <- function(dataset = "crsp.returns8", cache = TRUE, refresh = FALSE)
{
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
    x <- try(download.file(get_url,temp, quiet = TRUE), silent = TRUE)
    data <- readRDS(temp)
    unlink(temp)
    if (cache) saveCache(data, key = key)
    return(data)
  }
}
