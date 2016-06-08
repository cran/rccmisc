#' Find variables by name
#' 
#' Function to seacrh for a variable by its name. See the "Value" section for more details on the different functions.
#' 
#' @param df A data.frame from where the column names should be identified when returned function applied
#' @param pattern A character string with name (or part of name) of the variables to find.
#' @param envir environment holding data.frames where to search for the variables (the Global environment as default).
#' @param ... Arguments passed to \code{grep}
#' @return 
#' \itemize{
#' \item \code{findvar_fun}: A function with argument \code{param} to search for \code{param} in \code{df}. See example!
#' \item \code{findvar_in_df}: A vector with variable names from df matching the pattern.
#' \item \code{findvar_anywhere}: Does not return anything but prints a message where variables matching the pattern can be found.
#' }
#' @examples
#' find_cars <- findvar_fun(cars)
#' find_cars("sp")
#' 
#' findvar_in_df("sp", cars)
#' 
#' cars <- cars; iris <- iris
#' findvar_anywhere("petal")
#' @name findvar
NULL

#' @rdname findvar
#' @export
findvar_fun <- function(df, ...) function(pattern, ...) findvar_in_df(pattern, df, ...)

#' @rdname findvar
#' @export
findvar_in_df <- function(pattern, df, ...) names(df)[grep(pattern, names(df), ignore.case = TRUE, ...)]

#' @rdname findvar
#' @export
findvar_anywhere <- function(pattern, envir = .GlobalEnv, ...) {
  ls. <- ls(envir = envir)
   
  msg <- ""
  for (df_name in ls.) {
    df <- get(df_name)
    if (is.data.frame(df)) {
      vars <- findvar_in_df(pattern, df, ...)
      if (!identical(vars, character(0)))
          msg <- paste0(msg, df_name, ": ", paste(vars, collapse = ", "), "\n")
    }
  }
  
  if (msg != "") message(msg) else message("No variables found!")
}
  