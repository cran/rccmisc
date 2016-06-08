
#' Change one column name of a data.frame
#'
#' A data.frame can, by definition, have non unique column names.
#' To change a column name to a new name that is already present in the data.frame
#' might therefore lead to undesiered results. This function handles this problem
#' by dropping the original column cousing the name clash.
#'
#' @param x data.frame with a column name to change
#' @param old_name name (as character string) of column in \code{x}
#' that should be changed.
#' @param new_name new name (as character string) to use instead of \code{old_name}
#' @param warning should a warning be given if a name clash occours (\code{FALSE} by default)
#' @return The same data.frame but with one column named changed.
#' (Note that the output might have one column less if dropped after name clash.)
#'
#' @examples
#' ab <- ba <- data.frame(a = letters[1:10], b = 1:10)
#'
#' # One "traditional" way to change a column name
#' names(ab)[names(ab) == "a"] <- "b"
#' names(ab)
#' ab$b # Returns the first column with name "b"
#'
#' # Using change_col_names instead:
#' change_col_name(ba, "a", "b")
#'
#' @export
change_col_name <- function(x, old_name, new_name, warning = FALSE) {
  if (old_name != new_name && any(new_name == names(x))) {
    x[[old_name]] <- NULL
    if (warning)
      warning("There is already a column named ", new_name, " that will be dropped.")
  }
  names(x)[names(x) == old_name] <- new_name
  x
}