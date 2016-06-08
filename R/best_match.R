


#' Tries to correct misspelling of character string
#'
#' This function uses fuzzy string matching to replace one possibly misspeled
#' (or in other way not fully correct) character string with a correct version
#' of the same string.
#'
#' @param x is a character string (or a character vector) that should be matched
#' to the \code{key}
#' @param key is a vector containging the correct spellings of the character strings.
#' @param no_match Output value if there is no match. Default is NA. The input is returned unchanged if not matched and no_match = NULL.
#' @param all is a boolean indicator to specify what happens if there is more than one match.
#' Default is \code{FALSE} resulting in a warning message and that only the first match is used.
#' If \code{TRUE} the returned vector will no longer have the same length as \code{x}.
#' @return The function returns a character vector of the same length as \code{x} if \code{all = FALSE}
#' but with each element substituted to its best match in the \code{key}-vector.
#' Strings that could not be matched are NA if (\code{no_match = TRUE}) or unchanged if \code{no_match = FALSE}.
#' If \code{all = TRUE}, one input character string could result in more than one output character string. The output might therefore be longer
#' than the input.
#' @examples
#' best_match(c("Hej_apa!", "erik", "babian"), c("hej apa", "hej bepa", "kungen", "Erik"))
#' best_match(c("Hej_apa", "erik", "babian"),
#'    c("hej apa", "hej bepa", "kungen", "Erik"), no_match = FALSE)
#' @export
#' @seealso \link{clean_text}

best_match <- function(x, key, no_match = NA, all = FALSE) {
  
  # The function is best suited for just one character string to match (recursion is used otherwise)
  if (length(x) == 1) {
    
    # if the key have more than one character, x must have that to
    if (min(nchar(key)) >= 3 && !is.na(x) && nchar(x) < 3) {
      warning("x is to short (NA returned). It should be at least 3 characters long ",
              "(as long as all values from key are at least 3 characters long)!")
      return(NA)
    }
    
    # A string is matched either by grep or agrep
    match_agrep <- agrep(x, key, ignore.case = TRUE, value = TRUE)
    distances   <- utils::adist(x, match_agrep, ignore.case = TRUE)
    match_agrep <- suppressWarnings(match_agrep[which(distances == min(distances))])
    match_grep  <- grep(x, key, ignore.case = TRUE)
    match       <- unique(c(key[match_grep], match_agrep))
    
    # Handle the situation with more than one match
    if (length(match) > 1) {
      if (!all){
        match <- match[1]
        warning("Only the first match will be included. Remaining matches are dropped! ",
                "This can be specified with argument all")
      }
    }
    
    # Handle the situation with no match
    if (identical(match, character(0))) {
      warning("No match!")
      if (!is.null(no_match)) no_match else x
      
      # The ideal situation with one match
    } else
      unique(match)
    
    # Vectors are matched using recursion
  } else{
    c(Recall(x[1], key, no_match, all),
      Recall(x[-1], key, no_match, all)
    )
  }
}

