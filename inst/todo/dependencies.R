#' Create filename and URL for CRAN package
#'
#' Filename for tar.gz-files used for package source files
#' or URL for web page about the pacjkage on CRAN
#'
#' @param pkg package name as character string
#' @param version version number as character string
#' @param archive should the URL point to the archive of old packages?
#' \code{NULL} (as by default) checks where to find the specified version 
#' (if version specified)
#' @return filename or URL as character
#' @export
#' @name package_filename
#' @examples
#' \dontrun{
#' package_filename("plyr", "1.8.3")
#' package_url("plyr")
#' }
package_filename <- function(pkg, version) paste0(pkg, "_", version, ".tar.gz")

#' @rdname package_filename
#' @export
package_url <- function(pkg, version = NULL, archive = NULL) {
    if (is.null(archive) && !is.null(version)) archive <- version != latest_cran_version(pkg)$version
    cran_mirror <- getOption("repos")[names(getOption("repos")) == "CRAN"]
    paste0(cran_mirror, "src/contrib", if (!is.null(archive) && archive) paste0("/Archive/", pkg, "/") else "/",
           if (!is.null(version)) package_filename(pkg, version) else "")
}



#' Extract fields from DESCRIPTION file
#'
#' Download source package of specified version and
#' extract fields from its README file
#'
#' @param pkg package name as character
#' @param version version number as character
#' @param archive should the package be downloaded from the archive?
#' This can be left as NULL (as default) but can be specified to avoid
#' unnecesarry computation.
#' @param fields fields to read from the DESCRIPTION a character vector
#' @return Vector with concatenated field cells as character.
#' @export
#' @examples
#' \dontrun{
#' description_fields("commentr", "1.0.1", "Author")
#' }
description_fields <- function(pkg, version, fields, archive = NULL) {

    if (is.null(archive)) archive <- version != latest_cran_version(pkg)$version

    # URL:s and file paths
    url_tarfile <- package_url(pkg, version, archive)
    tar_dest    <- file.path(tempdir(), package_filename(pkg, version))
    untar_dest  <- file.path(tempdir(), pkg)

    # Download package source
    on.exit(unlink(c(tar_dest, untar_dest), recursive = TRUE))
    utils::download.file(url_tarfile, tar_dest, quiet = TRUE, method = "curl")
    utils::untar(tar_dest, exdir = tempdir())
    dsc <- as.list(read.dcf(file.path(untar_dest, "DESCRIPTION"), fields = fields))

    # Parse field data from DESCRIPTION file
    f <- lapply(dsc, function(x) strsplit(x, ", "))
    as.character(stats::na.omit(unname(unlist(f))))
}




#' Get data frame with release dates for package versions
#'
#' Use web scraping to extract release dates from CRAN archive.
#'
#' @param pkg package name as character string
#' @return data.frame with columns "date" and "version"
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' date_version_table("plyr")
#' }
date_version_table <- function(pkg) rbind(archived_cran_versions(pkg), latest_cran_version(pkg))


# Non exported help function to date_version_table
latest_cran_version <- function(pkg) {
    version <-
        as.data.frame(utils::available.packages(), stringsAsFactors = FALSE) %>%
        dplyr::filter_(~Package == pkg) %>%
        dplyr::select_(~Version)

    date <- description_fields(pkg, version, archive = FALSE, "Date/Publication")
    data.frame(date = as.POSIXlt(date, format = "%Y-%m-%d %H:%M"),
               version = version$Version,
               archive = FALSE,
               stringsAsFactors = FALSE)
}

# Non exported help function to date_version_table
archived_cran_versions <- function(pkg) {
    x <- package_url(pkg, archive = TRUE) %>%
        xml2::read_html() %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        dplyr::select_(~Name, ~`Last modified`) %>%
        dplyr::filter_(~!is.na(Name), ~Name != "Parent Directory") %>%
        dplyr::transmute_(
            date    = ~gsub("May", "Maj", `Last modified`),
            date    = ~gsub("Oct", "Okt", date),
            version = ~extract_version(Name),
            archive = TRUE)
    x$date <- as.POSIXlt(x$date, format = "%d-%b-%Y %H:%M")
    x[order(x$date), ]
}



#' Conversion between version and date
#'
#' Find which version was the latest release at a specified date
#' or vice versa.
#'
#' @param pkg package name as character
#' @param date date to check
#' @param version version number as character string
#' @return Date as POSIXtl or character string with version number
#' @examples
#' \dontrun{
#' # Find the latest version available today on CRAN
#' date2version("plyr", Sys.time())
#'
#' # When was version 1.8.3 of plyr published?
#' version2date("plyr", "1.8.3")
#' }
#' @name date2version
#' @export
#' @seealso compatible_package_version
date2version <- function(pkg, date) {
    dv <- date_version_table(pkg)
    utils::tail(dv$version[dv$date <= date], 1)
}
#' @rdname date2version
#' @export
version2date <- function(pkg, version) {
    dv <- date_version_table(pkg)
    dv$date[dv$version == version]
}



#' Which version of an independent package is garantead to work with a dependent package
#'
#' @param dep name of dependent package as character string
#' @param version version number of dependent package as character string
#' @param indep name of independent package as character string
#' @return Version number (as character string) of independent package that is gurantead
#' to work with the dependent package
#' @export
#' @examples
#' \dontrun{
#' compatible_package_version("knitr", "1.6", "plyr")
#' }
#' @seealso date2version
compatible_package_version <- function(dep, version, indep) date2version(indep, version2date(dep, version))



#' Return max version number from vector
#'
#' Compare version numbers using \code{\link{compareVersion}} and return the maximum
#'
#' @param x vector with version numbers as character
#' @return Vesion number as character vector of length one
#' @export
#' @examples
#' max_version(c("1.0", "1.01", "1.01.1", "1.0-1"))
max_version <- function(x) {
    lx <- length(x)
    if (lx == 1) x
    else if (lx == 2) if (utils::compareVersion(x[1], x[2]) == 1) x[1] else x[2]
    else Recall(c(x[1], Recall(x[x != x[1]])))
}

#' Is a package part of Base R
#'
#' @param pkg package name as character
#' @return \code{TRUE} if \code{pkg} is a package of Base R. \code{FALSE} otherwise.
#' @export
#' @examples
#' is.basepkg("base")
#' is.basepkg("compiler")
#' is.basepkg("commentr")
is.basepkg <- function(pkg) {
    pkg %in% c("R", "base", "compiler", "datasets", "graphics", "grDevices", "grid",
               "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
}

#' Extract information from character
#'
#' Functions relying in regular expressions to extract information from charcater
#'
#' @param x character vector
#' @return \code{extract_version} returns package version numbers as character.
#' \code{extract_package_name} returns package names as character.
#' @export
#' @name extract
#' @examples
#' x <- "commentr (>= 1.0.1)"
#' extract_version(x)
#' extract_package_name(x)
extract_version <- function(x) {
    y <- regmatches(x, gregexpr("([[:digit:]]+[\\.-])+[[:digit:]]+", x))
    y <- vapply(y, function(x) ifelse(identical(x, character()), NA_character_, x), character(1))
    unname(y)
}

#' @export
#' @rdname extract
extract_package_name <- function(x)
    ifelse(is.na(x), NA, regmatches(x, regexpr("^[[:alpha:]][[:alnum:]]*", x)))





#' Find package dependencies for possibly outdated packages
#'
#' Extract package informatoin from DESCRIPTION files recursivly
#' to produce a data.frame with all non Base R dependency packages.
#'
#' @param pkg package name as character
#' @param version version as character
#' @param recursive should dependent packages be found recursivly?
#' (\code{TRUE} by default)
#' @param fields type of dependency to consider on the top level. 
#' \code{c("Depends", "Imports")} by default.
#' Should be a subset of \code{c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo",
#' "Additional_repositories"}. Note that this only applies to to the package in the top 
#' level call to \code{dependencies}. Only \code{c("Depends", "Imports")} is used in recursive calls
#' (since cross references might atherwise couse infinite recursive loops).
#'
#' @return data.frame with one column \code{pkg} for package names and one column \code{version}
#' for versions.
#'
#' @section Implementation details:
#'
#' To find package dependencies for the lates version of a package on CRAN is easy.
#' All direct dependencies are clearly stated in relevant fields of the DECRIPTION file
#' published on CRAN/MRAN/METACRAN etc.
#' Let's say we are interested in dependencies for the \code{knitr} package.
#' We can find it here \url{https://mran.revolutionanalytics.com/package/knitr/}
#' On MRAN, it is also quite easy to find the recursive dependency tree by the "Dependencies Graph".
#'
#' We see from the dependency table (not the graph since it doesn't show version numbers)
#' that \code{knitr} imports for example \code{evaluate (>= 0.7.2)} and \code{digest}.
#' In MRAN (as opposed to CRAN) there are also hyperlinks direct to the dependent packages
#' that we can follow to find the recursive dependencies. If we click \code{evaluate} however,
#' we see that the current package version is newer than 0.7.2. This is not what we want
#' since we are searching for the absolute minimum set of packages to update.
#' It turns out that CRAN and friends only provide information for the latest package version
#' (MRAN possibly with some exceptions from September [August] 2014).
#'
#' Old package versions are archived as source (compressed tape archives). We can find all archived
#' versions for \code{evaluate} at \url{https://cran.r-project.org/src/contrib/Archive/evaluate/},
#' including version \code{0.7.2} from 2015-08-13. To continue, we now have to download the
#' archive, unzip it and read its DESCRIPTION file.
#' 
#' Note that the latest release of each package is not archived.
#' To find all versions of a package we therefore need to search both in
#' the archive and on the public web site.
#'
#' But what about the other \code{knitr} dependency package, \code{digest}?
#' Here we have only the package name, not its version. The latest available version can
#' be found either on the web site or from within R by function \code{\link{available.packages}}.
#' From the website we can also find the release date of that version
#' (unfortenately not by \code{\link{available.packages}}). We can then compare the release
#' date of the current version of \code{knitr} (2015-09-18 as of this writing) with all release
#' dates for the \code{digest} package (note once again that we need to check both the archive and
#' the latest version  separately). We here find that the latest version on CRAN, 0.6.8, was published
#' 2014-12-31 (and is therefore what we are looking for). 
#' Versions are always found for the date of the package immediately above in the recursive 
#' dependency tree. It therefore doesn't nececarly corresponds with the date of the package 
#' in the top level function call.
#'  
#'
#' We continue the search through each step in the dependency tree until we reach a package
#' that is part of the Base R installation (they are not included in the output).
#'
#' Note that the search can also start at an earlier version. If we are interested
#' not in the latest version of \code{knitr} but in version 1.6, we need to start in the archive
#' (but we might need to check in both places if we are not sure weather a package version is
#' current or archived).
#'
#' A package might occur more than once in a complex dependency tree. Only the latest version
#' is included in the output.
#'
#' @section Additional details:
#' Note that this function make extensive use of internet calls (downloading archive files
#' etcetera). A relaible connection is therefore needed during execution. 
#' 
#' Files (packages and package archives) are
#' temporarly stored in the R sessions temporary directory (found by \code{\link{tempdir}})
#' but are removed when no longer needed.
#' 
#' Version numbers can sometimes have a quite perquliar form (see 
#' \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file} 
#' for definition). For example "1.95-4.1" is a valid version number (for the XML package).
#' Comparing version numbers is therefore not as straight forward as comparing numerics or 
#' characters. \code{\link{compareVersion}} is used for version comparisons.
#' 
#' 
#'
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' dependencies("knitr", "1.6")
#' }

dependencies <- function(pkg, version, fields = c("Depends", "Imports"), 
                         recursive = TRUE) {

  # Base case for base packages and R itself
  if (is.basepkg(pkg)) {
      return(data.frame(pkg = character(0), version = character(0),
                        stringsAsFactors = FALSE))
  }
  message("Processing: ", pkg, " (", version, ")")


  # Find dependency packages through DESCRIPTION
  pkgs    <- description_fields(pkg, version, fields)
  pkgs_df <- data.frame(
        pkg     = extract_package_name(pkgs),
        version = extract_version(pkgs),
        stringsAsFactors = FALSE
    )
  pkgs_df <- pkgs_df[!is.basepkg(pkgs_df$pkg), ]

  # Substitute version with best guess if not stated explicitly
  v_na <- is.na(pkgs_df$version)
  pkgs_df$version[v_na] <- vapply(pkgs_df$pkg[v_na],
                                function(x) compatible_package_version(pkg, version, x),
                                character(1))

  # Recursion
  if (recursive) {
      recall <- function(i) {
          with(pkgs_df,
               dependencies(
                   pkg = pkg[i], version = version[i],
                   recursive = TRUE, fields = c("Depends", "Imports")))
      }
      rev_deps <- lapply(seq_len(nrow(pkgs_df)), recall)
      rev_deps <- do.call("rbind", rev_deps)
      pkgs_df  <- rbind(pkgs_df, rev_deps)
  }

  # Clean before output
  pkgs_df %>%
      dplyr::group_by_(~pkg) %>%
      dplyr::summarise_(version = ~max_version(version)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_(~pkg)
}

