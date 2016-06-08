#' Clean/standardize text
#'
#' Removes punctuation and spaces from character string. Also makes it lower case.
#' @param string a character string to "clean"
#' @return the cleaned character string (no punctuation, spaces or capital letters)
#' @examples
#' clean_text("HELLO_World!!!")
#' @export
#' @seealso \link{best_match}
clean_text <- function(string) tolower(gsub("[^[:alnum:]]", "", string))

