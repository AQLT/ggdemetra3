#' Plot 'rjdemetra3' model
#' 
#' @param object a seasonal adjustment model.
#' @param components components to print, can be \code{"y"} (input time series), 
#' \code{"sa"} (seasonal adjusted), \code{"t"} (trend-cycle), 
#' \code{"y_cal"} (calendar adjusted), \code{"s"} (seasonal), \code{"i"} (irregular),
#' \code{"cal"} (calendar). The vector can be named to change the label.
#' @param forecast boolean indicating if the forecast series should be printed.
#' @param ... unused arguments.
#' @examples 
#' x = rjd3x13::x13(ipi_c_eu[,"FR"])
#' ggplot2::autoplot(x)
#' @importFrom ggplot2 autoplot
#' @importFrom stats ts.union
#' @method autoplot JD3_X13_OUTPUT
#' @export
autoplot.JD3_X13_OUTPUT  <- function(object, 
                                     components = c("y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"), 
                                     forecast = FALSE, ...){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @method autoplot JD3_TRAMOSEATS_OUTPUT
#' @export
autoplot.JD3_TRAMOSEATS_OUTPUT <- function(object, 
                                     components = c("y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"), 
                                     forecast = FALSE, ...){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @method autoplot JD3_X13_RSLTS
#' @export
autoplot.JD3_X13_RSLTS <- function(object, 
                                   components = c("y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"), 
                                   forecast = FALSE, ...){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @method autoplot JD3_TRAMOSEATS_RSLTS
#' @export
autoplot.JD3_TRAMOSEATS_RSLTS <- function(object, 
                                   components = c("y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"), 
                                   forecast = FALSE, ...){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
autoplot_rjd <- function(object, 
                         components = c("y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"), 
                         forecast = FALSE, ...) {
    components_ <- match.arg(tolower(components),
                             choices = c("y", "t", "sa", "y_cal", "s", "i", "cal"),
                             several.ok = TRUE)
    names(components_) <- names(components)
    if (is.null(names(components_))) {
        names(components_) <- components_
    }
    names(components_)[names(components_) == ""] <- components_[names(components_) == ""]
    data <- ts.union(raw(object, ...), trendcycle(object, ...), 
                     seasonaladj(object, ...), calendaradj(object, ...), 
                     seasonal(object, ...), irregular(object, ...), calendar(object, ...))
    # data <- ts.union(raw(object), trendcycle(object), 
    #                  seasonaladj(object), calendaradj(object), 
    #                  seasonal(object), irregular(object), calendar(object))
    colnames(data) <- c("y", "t", "sa", "y_cal", "s", "i", "cal")
    data <- data[,components_, drop = FALSE]
    
    colnames(data) <- names(components_)
    data_plot <- data.frame(date = rep(time(data), ncol(data)),
                            y = c(data), 
                            label = factor(rep(colnames(data), 
                                               each = nrow(data)), levels = colnames(data)))
    p <- ggplot2::ggplot(ggplot2::aes(x = date, y = y), 
                         data = data_plot) + 
        ggplot2::geom_line()
    if (forecast) {
        data_f <- ts.union(raw(object, forecast = TRUE), trendcycle(object, forecast = TRUE), 
                           seasonaladj(object, forecast = TRUE), calendaradj(object, forecast = TRUE), 
                           seasonal(object, forecast = TRUE), irregular(object, forecast = TRUE), 
                           calendar(object, forecast = TRUE))
        colnames(data_f) <- c("y", "t", "sa", "y_cal", "s", "i", "cal")
        data_f_plot <- data.frame(date = rep(time(data_f), ncol(data_f)),
                                  y = c(data_f), 
                                  label = factor(rep(colnames(data_f), 
                                                     each = nrow(data_f)), levels = colnames(data_f)))
        data_f <- data_f[,components_, drop = FALSE]
        colnames(data_f) <- colnames(data)
        p <- p + ggplot2::geom_line(data = data_f_plot, linetype = 2)
    }
    p + ggplot2::facet_grid("label ~ .", scales = "free_y", 
                            switch = "y") + 
        ggplot2::ylab(NULL)
}
utils::globalVariables(c("date", "y"))
