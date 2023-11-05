.onLoad <- function(libname, pkgname) {
    if (! requireNamespace('rjd3toolkit', quietly = TRUE)) stop("Loading rjd3 libraries failed")
    if (! requireNamespace("rjd3tramoseats", quietly = TRUE)) stop("Loading rjd3 libraries failed")
    if (! requireNamespace("rjd3x13", quietly = TRUE)) stop("Loading rjd3 libraries failed")
    rjd3toolkit::reload_dictionaries()
}