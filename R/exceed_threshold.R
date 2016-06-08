#' Check if transformation/coercing of a vector is good enaough
#' 
#' This function is primarly aimed to check if the transformation of a  vector was 
#' succesfull enough to return the transformed value instead of the original.
#' 
#' @param original the original vector
#' @param transformed the transformed vector with NA-values for non transformed values
#' @param threshold is a numeric value in [0,1] specifying the proportion of cells in \code{transformed} that should be recognised as correctly coerced to accept the new class. 
#' This does not effect the function output (except when \code{force = TRUE}) 
#' but will have some diagnostic benefits.
#' @param force Should a candidate vector (candidate according to \code{threshold}) be forced to its suggested class (with non-coercable elements set to NA). 
#' \code{FALSE} by default but if the function is called interactivelly, the user will also have the option to set force = TRUE on the fly.
#' @param var_name a name for the object to be used in messages (you could probably just leave this as default, NULL; it is mostly used for internal purposes!).
#' @param ask this argument gives you the chance to interactively inspect your data and specify if a column is a date or not,
#' on the fly. This is \code{FALSE} by default for \code{as.Dates.default} but \code{TRUE} for  
#' \code{as.Dates.dataframe}. It only applies when the function is runed interactively and only when \code{force == FALSE}. 
#' 
#' @return Either \code{original} or \code{transformed}.
#' @export
#' @examples
#' 
#' x <- c(rep("2012-01-01", 9), "foo")
#' exceed_threshold(x, as.Date(x))
#' exceed_threshold(x, as.Date(x), force = TRUE)
#' exceed_threshold(x, as.Date(x), ask = TRUE)
#' exceed_threshold(x, as.Date(x), threshold = 1)
#' exceed_threshold(x, as.Date(x), var_name = "bar", force = TRUE)
#' 
#' x <- c(1:9, "baz")
#' exceed_threshold(x, suppressWarnings(as.numeric(x)))

exceed_threshold <- function (original, transformed, threshold = .9, force = FALSE, 
                              ask = FALSE, var_name = "the input vector") {

  ## Argument checks
  stopifnot(
    length(transformed) == length(original),
    is.scalar_in01(threshold),
    is.logical(force),
    is.logical(ask),
    is.character(var_name) & length(var_name) == 1
  )
  
  ## Message to use if variable coerced
  msg_pass <- paste(var_name, "coerced to", class(transformed)[1])
  
  ## If transformation did not introduce any new NA
  if (sum(is.na(specify_missing(transformed))) == sum(is.na(specify_missing(original)))){
    message(msg_pass)
    return(transformed)
  
  ## If not enough x:s transformed: FALSE
  } else if(mean(!is.na(transformed[!is.na(specify_missing(original))])) < threshold){
    return(original)
  
  ## If enough x:s transformed, investigate further
  } else{
    ## Original entries (non empty) that can not be coerced to target format:
    not_passing <- paste( original[is.na(transformed) & !is.na(specify_missing(original))], collapse = "\", \"")
    ## Warning message to be printed
    msg_fail <- paste0("\nMore than ", 
                        round(threshold * 100, 0), 
                       " % of the cells in \"", var_name, "\" can be coercd to \"",
                       class(transformed)[1],
                       "\" but the following can not: \"", not_passing, "\""
    )
    
    ## Ask for decision interactively  
    if (!force && ask && interactive()) {
      writeLines(paste0(msg_fail, 
                "\n\nWould you like to coerce this variable to ",
                class(transformed)[1]), ", with non coercable value/s/ (above) set to NA?",
                "\n[y/n/c]?:", sep = ""
      )
      answer <- readLines(con = stdin(), n = 1)
      switch(answer,
           y      = return(transformed),
           yes    = return(transformed),
           n      = return(original),
           no     = return(original),
           c      = stop("Cancelled"),
           cancer = stop("Cancelled"),
          stop("Invalid input!")
      )
      
    ## Coerce without asking if force
    } else if (force){
      warning(msg_pass, " with ", not_passing, " set to NA!", call. = FALSE)
      return(transformed)
    
    } else{
      message(msg_fail)
      return(original)
    }
  }
}