#' Dump skript together with all functions from package
#'
#' @param script connection with script (file) to append tp function defenitions
#' @param package name of package (as character)
#' @param outfile filename for dump file
#' @param all should all (even non exported) objects from the package be exported?
#'
#' @return nothing (function called for its side effects)
#' @export
make_r_script <- function(script = NULL, package, outfile = "./inca_r_script.R", all = TRUE) {
  if (interactive()) {
    outfile <- file(outfile, "w", encoding = "UTF-8")
    writeLines(paste("#", Sys.Date()), outfile)
    writeLines("\n\n\n# ------ FUNCTIONS FROM PACKAGE ------ #\n\n\n", outfile)
    e <- getNamespace(package)
    dump(ls(e), outfile, envir = e, append = TRUE)
    if (!is.null(script)) {
      writeLines("\n\n\n# ------ SCRIPT ------ #\n\n\n", outfile)
      writeLines(readLines(script), outfile)
    }
    close(outfile)
  } else {
    warning("make_r_script called but not run in non interactive mode!")
  }
}
