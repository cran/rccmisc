
#' Specify missing values for a vector
#' 
#' Change specified values to NA
#' 
#' @param x vector
#' @param ... values that should be changed to \code{NA} if found in \code{x}
#' @param default_missing a vector with additional default values to change to NA. 
#' These are treated the same as \code{...} but are added by default if not removed. 
#'   A special value \code{"blank"} can be used to indicate all empty strings (all
#'   characters matching \code{[:blank:]}, see \link{regex}).
#'   
#' @return x itself but with specified values set to \code{NA}.
#'   
#' @examples
#' x <- sample(100)
#' x[sample(100, 10)] <- 999
#' specify_missing(x, 999)
#' @export
#' @name specify_missing
specify_missing <- function(x, ..., default_missing = c("", NA, "blanks")){
  UseMethod("specify_missing")
}

#' @export
specify_missing.list <- function(x, ..., default_missing = c("", NA, "blanks")){
  lapply(x, specify_missing, ..., default_missing)
}

#' @export
specify_missing.data.frame <- function(x, ..., default_missing = c("", NA, "blanks")){
  x[] <- specify_missing.list(x, ..., default_missing)
  x
}

#' @export
specify_missing.matrix <- function(x, ..., default_missing = c("", NA, "blanks")){
  apply(x, 2, specify_missing, ..., default_missing)
}

#' @export
specify_missing.default <- function(x, ..., default_missing = c("", NA, "blanks")){
  
  missing <- c(default_missing, list(...))
  
  # Remove completely blank cells
  # We can not simply use trum, since that would also effect non blank cells
  if ("blanks" %in% missing){
    x <- ifelse(gsub("[[:blank:]]", "", x) == "", "", x)
  }
  
  x[x %in% missing] <- NA
  x
}
