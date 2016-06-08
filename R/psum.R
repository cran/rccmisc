#' Parallel sum
#' 
#' This functin is to \link{sum}, what \link{pmin} and \link{pmax} is to \link{min} and \link{max}.
#' 
#' @param ... numeric vectors
#' @param na.rm a logical indicating whether missing values should be removed.
#' @export
#' @examples
#' psum(1:10, 1:10, 1:10) 
psum <- function(..., na.rm = FALSE) { 
  dat         <- do.call(cbind, list(...))
  res         <- rowSums(dat, na.rm = na.rm) 
  idx_na      <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}