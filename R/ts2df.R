#' Convert a 'ts' or 'mts' Object to a 'data.frame'
#'
#' Converts a `ts` or `mts` object into a `data.frame` that can be directly used in plotting functions.
#'
#' @param x A `ts` or `mts` object.
#'
#' @return A `data.frame` object.
#' 
#' @examplesIf rjd3jars::check_java_version(silent = TRUE) 
#' # To get the ipi_c_eu_df object:
#' ts2df(ipi_c_eu)
#' 
#' @export
ts2df <- function(x){
    UseMethod("ts2df", x)
}
#' @name ts2df
#' @export
ts2df.ts <- function(x){
    date <- as.numeric(time(x))
    name <- deparse(substitute(x))
    result <- data.frame(date = date,
               as.numeric(x))
    colnames(result) <- c("date", name)
    result
}
#' @name ts2df
#' @export
ts2df.mts <- function(x){
    date <- as.numeric(time(x))
    data <- as.matrix(x)
    rownames(data) <- NULL
    data.frame(date = date,
               data)
}
