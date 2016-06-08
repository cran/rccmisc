
#' A safe alternative to ifelse
#' 
#' This function is similair to \code{\link{ifelse}} with differences described in the details section.
#' 
#' \code{safe_ifelse} differs from \code{\link{ifelse}} in the following ways:
#' \itemize{
#' \item{Both 'yes' and 'no' must be vectors of the same type or class. 
#' This ensures that the output will be of correct format.}
#' \item{Factors can be combined without problem}
#' \item{The argument \code{na.rm} makes it easier to handle cases when \code{cond = NA}}
#' }
#' 
#' @param test,yes,no arguments passed to \code{\link{ifelse}}
#' @param na_as_false should \code{NA} values in \code{test} be handled as \code{FALSE}?
#' \code{TRUE} (which is default) implies that \code{test & !is.na(test)} must be fullfilled to
#' return values from argument \code{yes}
#' @param drop.levels This only applies when \code{yes} and \code{no} are factor variables. 
#' The result will then also be a factor. Unused levels (from \code{yes} and \code{no} combined)
#' are dropped by default.  
#' 
#' @return
#' Vector of same length and class as \code{yes} and \code{no}.
#' @export
#' @examples
#' # Test must be TRUE to return 'yes'
#' safe_ifelse(NA, 1, 2) ## 2
#' ifelse(NA, 1, 2) ## NA
#' 
#' # Factors are problematic in ifelse
#' ifelse(TRUE, as.factor("hello"), 2) ## 1
#' \dontrun{
#' safe_ifelse(TRUE, as.factor("hello"), 2) ## Error
#' }
#' safe_ifelse(TRUE, as.factor("hello"), as.factor(2)) ## hello
#' safe_ifelse(TRUE, as.factor("hello"), as.factor(2), drop.levels = FALSE)
#' 
#' 
safe_ifelse <- function(test, yes, no, na_as_false = TRUE, drop.levels = TRUE){
  
  ## Same type of object (also OK if one is just NA)?
  if (typeof(yes) != typeof(no) & class(yes) != class(no) & !(all(is.na(yes)) | all(is.na(no)))) {
    stop("Condition 'yes' and 'no' must have the same type/class!")
  }
  
  ## Handle factor variables
  if (is.factor(yes)) {
    is_factor <- TRUE
    if (!drop.levels) {
      levels <- unique(c(levels(yes), levels(no)))
    }
    yes <- as.character(yes)
    no <- as.character(no)
  }
  
  if (na_as_false) {
    test <- test & !is.na(test)
  }
  
  x <- ifelse(test, yes, no)
  
  ## Handle output factor levels if factor
  if (exists("is_factor")) {
    if (!drop.levels) {
      x <- factor(x, levels)
    } else{
      x <- as.factor(x)
    }
  }
  
  x
} 