
#' Are we running on INCA?
#' @return TRUE if we are running on INCA, FALSE otherwise
#' @export
#' @examples 
#' is.inca()
is.inca <- function() unname(Sys.info()["nodename"] == "EXT-R27-PROD")
