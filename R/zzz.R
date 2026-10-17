#' @importFrom rJava .jpackage .jaddClassPath
#' @importFrom rjd3jars reload_dictionaries reload_tsproviders
#' @importFrom rjd3jars reload_safactories check_java_version
#' @keywords internals
#' @noRd
.onLoad <- function(libname, pkgname) {
    # Loading dependencies
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
        stop("Loading {rjd3toolkit} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3x13", quietly = TRUE)) {
        stop("Loading {rjd3x13} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3tramoseats", quietly = TRUE)) {
        stop("Loading {rjd3tramoseats} failed", call. = FALSE)
    }
    
    # If java >= 21, then reload dictionnaries
    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
        rjd3jars::reload_tsproviders()
        rjd3jars::reload_safactories()
    }
}