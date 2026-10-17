#' Initialise a 'ggplot2' Object with an SA Model
#'
#' @param x A seasonally adjusted model.
#' @param context The optional context used to estimate the model.
#' @param ... Other parameters passed to [ggplot2::ggplot()].
#' @param dates Parameters used to re-estimate the model (see details).
#'
#' @details `init_ggplot()` initializes a \pkg{ggplot2} object by storing the seasonal adjustment model used.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' mod <- rjd3x13::jx13(window(rjd3toolkit::ABS$X0.2.20.10.M, start = 2000))
#' init_ggplot(mod) +
#'     geom_line(color = "#F0B400") +
#'     geom_sa(component = "sa", color = "#155692")
#'     
#' @return A \pkg{ggplot2} object that can be further customized.
#' @export
init_ggplot <- function(x, ...) {
    UseMethod("init_ggplot", x)
}

#' @name init_ggplot
#' @export
init_ggplot.JD3_X13_OUTPUT<- function(x, context = NULL, ...) {
    spec <- x$result_spec
    y <- raw(x)
    d_y <- ts2dataframe(y)
    seasonal_adjustment(
        data = d_y,
        method = "x13",
        spec = spec,
        context = context,
        frequency = frequency(y),
        message = FALSE,
        new_data = TRUE)
    ggplot2::ggplot(
        data = d_y, 
        ggplot2::aes(x = x, y = y), 
        ...
    )
}
#' @name init_ggplot
#' @export
init_ggplot.JD3_TRAMOSEATS_OUTPUT <- function(x, context = NULL, ...) {
    spec <- x$result_spec
    y <- raw(x)
    d_y <- ts2dataframe(y)
    seasonal_adjustment(
        data = d_y,
        method = "tramoseats",
        spec = spec,
        context = context,
        frequency = frequency(y),
        message = FALSE,
        new_data = TRUE)
    ggplot2::ggplot(
        data = d_y, 
        ggplot2::aes(x = x, y = y), 
        ...
    )
}
#' @name init_ggplot
#' @export
init_ggplot.JD3_Object <- function(x, ...){
    y <- raw(x)
    d_y <- data <- ts2dataframe(y)
    frequency <- frequency(y)
    message <- FALSE
    spec <- NULL
    if (is_jx13(x)) {
        method <- "x13"
    } else if (is_jtramoseats(x)) {
        method <- "tramoseats"
    } else {
        stop("The object is not a seasonal adjustment model.")
    }
    .ggdemetra$data_ts <-
        dataframe2ts(data = data, frequency = frequency, message = message)
    
    .ggdemetra$sa <- x
    .ggdemetra$spec <- spec
    .ggdemetra$method <- method
    .ggdemetra$data_y <- data$y
    ggplot2::ggplot(
        data = d_y, 
        ggplot2::aes(x = x, y = y), 
        ...
    )
}
#' @keywords internal
#' @noRd
is_jx13 <- function(x) {
    if (inherits(x, "JD3_Object")) {
        x <- x$internal
    }
    rJava::.jinstanceof(
        x, 
        "jdplus/x13/base/core/x13/X13Results"
        )
}
#' @keywords internal
#' @noRd
is_jtramoseats <- function(x) {
    if (inherits(x, "JD3_Object")) {
        x <- x$internal
    }
    rJava::.jinstanceof(
        x, 
        "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults"
        )
}
#' @name init_ggplot
#' @export
init_ggplot.hf_decomposition <- function(
        x, dates = NULL,
        ...) {
    
    y <- raw(x)
    if (!is.null(x$parameters$series_time)){
        dates <- x$parameters$series_time
    } else if (is.null(dates)) {
        dates <- seq_along(raw(x))
    }
    d_y <-  data.frame(x = dates,
                       y = y)
    .ggdemetra$data_ts <- x
    .ggdemetra$sa <- x
    .ggdemetra$spec <- x$parameters
    .ggdemetra$data_y <- y
    # Method not important since no reestimation is done
    .ggdemetra$method <- "hf_decomposition" 
    
    ggplot2::ggplot(
        data = d_y, 
        ggplot2::aes(x = x, y = y), 
        ...
    )
}
# 
# guess_hf_method <- function(x, frequency = NULL, dates = NULL, spec = NULL, method = NULL){
#     if (!is.null(method)) {
#         return(method)
#     }
#     default_spec_warning <- 
#         paste0(
#             "The 'spec' is not provided, the parameters of the model will be used as spec.",
#             "The parameters being an incomplete list of the specification, the result might be different.",
#             sep = "\n"
#         )
#     if ("ucarima" %in% names(x)) {
#         if (length(x$period) == 1) {
#             method <- "fractionalairline"
#             frequency <- x$parameters$period
#             if (!is.null(x$parameters$series_time)){
#                 dates <- x$parameters$series_time
#             }
#             spec <- x$parameters
#         } else {
#             method <- "multiairline"
#             frequency <- x$parameters$period
#             if (is.null(spec)){
#                 spec <- x$parameters
#                 spec$sn <- NULL # wrongly in the parameters
#                 warning(default_spec_warning)
#             }
#             if (!is.null(x$parameters$series_time)){
#                 dates <- x$parameters$series_time
#             }
#         }
#     } else if ("user_defined" %in% names(x)) {
#         method <- "x11-extended"
#         frequency <- x$parameters$period
#     } else if (any(grepl("^s\\d+$", colnames(x$decomposition)))) {
#         method <- "stl"
#         if (is.null(frequency)){
#             stop("The frequency must be provided for STL decomposition")
#         }
#         if (is.null(spec)){
#             spec <- x$parameters
#             warning(default_spec_warning)
#         }
#     } else {
#         if (is.null(method)) {
#             stop("The method must be provided to distinguish MSTL and ISTL")
#         }
#         if (!tolower(method[1]) %in% c("istl", "mstl")) {
#             stop("The method should be mstl or istl")
#         } 
#         if (is.null(frequency)){
#             stop("The frequency must be provided for MSTL/ISTL decomposition")
#         }
#         if (is.null(spec)){
#             spec <- x$parameters
#             warning(default_spec_warning)
#         }
#     }
#     
#     list(
#         method = method, 
#         frequency = frequency, 
#         dates = dates, 
#         spec = spec
#     )
#     
# }
# 
