#' @title Abbreviate a vector of names
#'
#' @param name A character vector of long character strings.
#' @param max_word_length Maximum word length before truncation.
#' @param collapse A string used to join the abbreviation letters.
#'
#' @return A character vector of abbreviated names.
#' @export
#'
#' @examples
#' abbreviate_name(c("Large Cap Growth", "Small Value"), max_word_length = 3)
abbreviate_name <- function(name, max_word_length = Inf, collapse = "") {
  
  vapply(name, function(nm) {
    # Split into words
    words <- strsplit(nm, "\\s+")[[1]]
    
    # Optionally truncate each word
    words <- ifelse(
      nchar(words) > max_word_length,
      substr(words, 1, max_word_length),
      words
    )
    
    # Take first letter of each word
    abbrev <- substr(words, 1, 1)
    
    # Collapse
    paste(abbrev, collapse = collapse)
  }, FUN.VALUE = character(1))
}
