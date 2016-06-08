#' Template functions to generate basic S3 methods for new classes
#' 
#' \code{create_s3_method} creates a method that applies \code{NextMethod} but that also keeps additional 
#' attributes (such as class).
#' \code{create_s3_print} creates a print method.
#' 
#' Don't forget to also create for example a data.frame method by 
#' 
#' \code{as.data.frame.xxx <- as.data.frame.vector}
#' 
#' @param generic,object as described for \code{\link{NextMethod}}
#' @param fun Function to transform object before print 
#' (probably \code{\link{as.character}}, \code{\link{as.numeric}} or similair).
#' @param ... additional arguments passed to print method
#' 
#' @return S3-method.
#' @examples 
#' a <- structure(1:10, class = c("b", "numeric"))
#' a[3] # Normal subsetting makes a loose its attributes
#' `[.b` <- create_s3_method("[")
#' print.b <- create_s3_print(as.numeric)
#' a[3] # attributes preserved even if we can't see them
#' str(a[3])
#' 
#' @export
#' @name create_s3_method
create_s3_method <- function(generic = NULL, object = NULL){
  function(x, ...) {
    r <- NextMethod(generic = generic, object = object)
    mostattributes(r) <- attributes(x)
    r
  }
}

#' @export
#' @rdname create_s3_method
create_s3_print <- function(fun, ...) function(x, ...) print(fun(x), ...)
