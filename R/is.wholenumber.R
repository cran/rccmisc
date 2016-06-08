#' Test if a numeric vector consists of whole numbers
#' 
#' Function borrowed from the examle section for \code{\link{integer}}.
#' 
#' @param x a numeric vector
#' @param tol How much is \code{x} allowed to deviate from \code{round(x)} to 
#' be a whole number.
#' @return Logical vector with same length as \code{x}.
#' @export
#' @examples
#' is.wholenumber(1) # is TRUE
#' (x <- seq(1, 5, by = 0.5) )
#' is.wholenumber( x ) #-->  TRUE FALSE TRUE ...
is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5){
  all(is_numeric(x) & abs(x - round(x)) < tol, na.rm = TRUE)
}