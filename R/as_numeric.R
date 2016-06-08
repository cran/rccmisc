#' Test object for, or coerce to, numeric
#' 
#'  \code{as_numeric} is essentially a wrapper to \code{as.numeric} except that objects of class factor are first coerced to character and then to numeric.
#'  \code{is_numeric} test if \code{x} is "somehow numeric" (see examples).
#'  
#' @param x object to be coerced or tested
#' (and return a logical vector of the same length) or should it test the whole 
#' vector as one object and return a logical vector of length one. (\code{TRUE} by default).
#' @name as_numeric
#' @export
#' @examples
#' df <- data.frame(v = c("46513", "45"))
#' class(df$v) # factor
#' 
#' # Note that
#' as.numeric(df$v) # 2 1
#' # but
#' as_numeric(df$v) # 46513    45
#' 
#' is_numeric(1) # TRUE
#' is_numeric("1") # TRUE
#' is_numeric(as.factor(1)) # TRUE
#' is_numeric(as.factor("khb")) # FALSE
as_numeric <- function(x) UseMethod("as_numeric")

#' @export
as_numeric.default <- function(x) as.numeric(x)

#' @export
as_numeric.factor <- function(x) as_numeric(as.character(x))

#' @export
#' @rdname as_numeric
is_numeric <- function(x){
  y <- !is.na(suppressWarnings(as.numeric(as.character(x))))
  y[is.na(x) | x == ""] <- NA
  y
}
