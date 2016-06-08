
#' Calculate the width of the range of x
#' 
#' @param x object to calculate range for
#' 
#' @return The width of the range of \code{x} as integer.
#' 
#' @examples
#' width(1:10)
#' width(c(6748, 234, 2456, 5678))
#' width(sample(345))
#' @export
width <- function(x) UseMethod("width")

#'@export
width.default <- function(x){
  if (is_numeric(x)) return(width(as_numeric(x)))
  stop("No method for object off class ", class(x))
}
  
  
#'@export
width.numeric <- function(x){
  if (!is.wholenumber(x))
    stop(deparse(substitute(x)), " is not an integer vector!")
  range(x)[2] - range(x)[1] + 1
} 

