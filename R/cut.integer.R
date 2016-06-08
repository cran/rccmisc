#' Convert integer vector to Factor
#' 
#' S3-method for cut applied to integer vectors where all outcome 
#' factors are integer intervals.
#' 
#' @param x integer vector
#' @param ... further arguments passed to or from other methods
#' @return 
#' If \code{cut.default(x, ...)} returns only intger intervals, these are 
#' formated in a more natural way and returned as an ordered factor.
#' If non integer interval limits occur, the output of \code{cut.default(x, ...)} is retured as is.
#' 
#' @examples
#' cut.default(1:100, seq(0, 100, 20)) # Gives a quite unnatural output
#' cut(1:100, seq(0, 100, 20)) # Gives nicer and ordered output
#' cut(1:10, 3) # no integer intervals and therefor same as cut.default
#' @export

cut.integer <- function(x, ...){
  x <- cut.default(x, ...)
  
  xx <- strsplit(as.character(x), ",")
  x1 <- sapply(xx, utils::head, 1)
  x2 <- sapply(xx, utils::tail, 1)
  
  ## Helper function to remove paranthesis
  rm.paranthesis <- function(x) as.numeric(gsub("\\(*\\)*\\[*\\]*", "", x))
  
  ## Helper function to add (negative number => subtract) 1
  if_yes <- function(x, add) rm.paranthesis(x) + add
  
  ## Correct start of interval
  x1 <- ifelse(substring(x1, 1, 1) == "(", if_yes(x1, 1), rm.paranthesis(x1))

  ## Correct end of interval
  x2 <- ifelse(substring(x2, nchar(x2)) == ")", if_yes(x2, -1), rm.paranthesis(x2))
  
    if (any(!is.wholenumber(x1), !is.wholenumber(x2))){
      return(x)
    }
  
  ## Put negative end numbers inside paranthesis
  x2 <- ifelse(x2 < 0, paste0("(", x2, ")"), x2)
  
  x <- paste(x1, x2, sep = "-")
  x <- specify_missing(x, "NA-NA")
  x <- ordered(factor(x, levels = unique(x[order(x1)])))
  x
}





