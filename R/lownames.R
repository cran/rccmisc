#' Make all names in data.frame lower case
#' 
#' Tests are also performed so that all column names will stay unique!
#' 
#' @param df A data.frame, possibly with some names with capital letters 
#' @return \code{df} is returned unchanged, except that capital letters in names are changed to lower case.
#' @export
#' @examples
#' df <- data.frame(Hello = 1:10, World = 1:10)
#' lownames(df)

lownames <-
  function(df) 
  {
    new_names <- tolower(names(df))
    double_names <- names(df)[duplicated(new_names)]
    new_double_names <- paste0(new_names[duplicated(new_names)], "_rename_to_avoid_duplicated_names") 
    
    names(df) <- new_names
    names(df)[duplicated(new_names)] <- new_double_names
    
    ## Non unique names are suffixed with 2 and warning given 
    ## This happens even if there were duplicated names already from the beginning
    if (anyDuplicated(new_names) != 0) {
        warning(
          "Variable(s) \"", paste(double_names, collapse = "\", \""), 
          "\" yields non unique names when simply renamed with lower case ",
          " and are therefore named \"",
          paste(new_double_names, collapse = "\", \""), "\"")
    } 
  
    df
   
  }
