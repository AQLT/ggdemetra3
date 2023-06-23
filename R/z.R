.onLoad <- function(libname, pkgname) {
    if (! requireNamespace('rjd3toolkit', quietly = T)) stop("Loading rjd3 libraries failed")
    rjd3toolkit::reload_dictionaries()
}