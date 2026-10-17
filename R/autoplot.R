#' Plot Seasonal Adjustment Objects
#'
#' @param object A seasonal adjustment model 
#' (from \pkg{rjd3x13}, \pkg{rjd3tramoseats}, or \pkg{rjd3highfreq}).
#' @param components Components to display. Can be `"y"` (input time series), 
#' `"sa"` (seasonally adjusted), `"t"` (trend-cycle), 
#' `"y_cal"` (calendar-adjusted), `"s"` (seasonal), `"i"` (irregular),
#' and `"cal"` (calendar). The vector can be named to change the strip label.
#' @param forecast Logical indicating whether the forecast series should be 
#' plotted (default is `FALSE`).
#' Ignored for `"hf_decomposition"` objects (\pkg{rjd3highfreq}).
#' @param dates For `"hf_decomposition"` objects (\pkg{rjd3highfreq}), 
#' a numeric or date vector corresponding to the time series. 
#' If `NULL`, a sequence of integers is used.
#' For models created with [rjd3highfreq::fractional_airline_decomposition()] 
#' or [rjd3highfreq::multi_airline_decomposition()], 
#' the `series_time` parameter is used if available.
#' 
#' @param ... Additional arguments passed to methods.
#' 
#' @return A \pkg{ggplot2} object representing the faceted components.
#' @importFrom ggplot2 autoplot
#' @importFrom stats ts.union
#' @method autoplot JD3_X13_OUTPUT
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' autoplot(rjd3x13::jx13(ipi_c_eu[, "FR"]))
#' 
#' hf_data <- usclaims[usclaims$date <= "2019-12-01",]
#' mod_hf <- rjd3highfreq::fractional_airline_decomposition(
#'   hf_data[, "Initial claims"], 
#'   period = 52,
#'   series_time = hf_data[, "date"]
#' )
#' autoplot(mod_hf, dates = usclaims$date[usclaims$date <= "2019-12-01"])
#' 
#' @name autoplot_ggdemetra3
#' @export
autoplot.JD3_X13_OUTPUT  <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @name autoplot_ggdemetra3
#' @method autoplot JD3_TRAMOSEATS_OUTPUT
#' @export
autoplot.JD3_TRAMOSEATS_OUTPUT <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @name autoplot_ggdemetra3
#' @method autoplot JD3_X13_RSLTS
#' @export
autoplot.JD3_X13_RSLTS <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @name autoplot_ggdemetra3
#' @method autoplot JD3_TRAMOSEATS_RSLTS
#' @export
autoplot.JD3_TRAMOSEATS_RSLTS <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
){
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @name autoplot_ggdemetra3
#' @method autoplot JD3_Object
#' @export
autoplot.JD3_Object<- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
){
    if (!is_jx13(object) && !is_jtramoseats(object)) {
        stop("The object is not a seasonal adjustment model.")
    }
    autoplot_rjd(object = object,
                 components = components,
                 forecast = forecast, 
                 ...)
}
#' @name autoplot_ggdemetra3
#' @keywords internal
#' @noRd
autoplot_rjd <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        forecast = FALSE, ...
) {
    components_ <- match.arg(
        tolower(components),
        choices = c("y", "t", "sa", "y_cal", "s", "i", "cal"),
        several.ok = TRUE
    )
    names(components_) <- names(components)
    if (is.null(names(components_))) {
        names(components_) <- components_
    }
    names(components_)[names(components_) == ""] <- 
        components_[names(components_) == ""]
    data <- cbind(
        raw(object, ...), trendcycle(object, ...), 
        seasonaladj(object, ...), calendaradj(object, ...), 
        seasonal(object, ...), irregular(object, ...), 
        calendar(object, ...)
    )
    colnames(data) <- c("y", "t", "sa", "y_cal", "s", "i", "cal")
    data <- data[,components_, drop = FALSE]
    
    colnames(data) <- names(components_)
    data_plot <- data.frame(
        date = rep(time(data), ncol(data)),
        y = c(data), 
        label = factor(
            rep(colnames(data), 
                each = nrow(data)), 
            levels = colnames(data))
    )
    p <- ggplot2::ggplot(ggplot2::aes(x = date, y = y), 
                         data = data_plot) + 
        ggplot2::geom_line()
    if (forecast) {
        data_f <- ts.union(
            raw(object, forecast = TRUE),
            trendcycle(object, forecast = TRUE), 
            seasonaladj(object, forecast = TRUE), 
            calendaradj(object, forecast = TRUE), 
            seasonal(object, forecast = TRUE), 
            irregular(object, forecast = TRUE), 
            calendar(object, forecast = TRUE)
        )
        colnames(data_f) <- c("y", "t", "sa", "y_cal", "s", "i", "cal")
        data_f <- data_f[,components_, drop = FALSE]
        data_f_plot <- data.frame(
            date = rep(time(data_f), ncol(data_f)),
            y = c(data_f), 
            label = factor(
                rep(colnames(data_f), 
                    each = nrow(data_f)), 
                levels = colnames(data_f))
        )
        colnames(data_f) <- colnames(data)
        p <- p + ggplot2::geom_line(data = data_f_plot, linetype = 2)
    }
    p + ggplot2::facet_grid("label ~ .", scales = "free_y", 
                            switch = "y") + 
        ggplot2::ylab(NULL)
}
utils::globalVariables(c("date", "y"))

#' @name autoplot_ggdemetra3
#' @method autoplot hf_decomposition
#' @export
autoplot.hf_decomposition <- function(
        object, 
        components = c(
            "y", "sa", "trend" = "t", "seasonal" = "s", "irregular" = "i"
            ), 
        dates = NULL,
        ...
){
    components_ <- match.arg(tolower(components),
                             choices = c("y", "t", "sa", "s", "i"),
                             several.ok = TRUE)
    names(components_) <- names(components)
    if (is.null(names(components_))) {
        names(components_) <- components_
    }
    names(components_)[names(components_) == ""] <- 
        components_[names(components_) == ""]
    data <- cbind(
        raw(object, ...), trendcycle(object, ...), 
        seasonaladj(object, ...),
        seasonal(object, ...), irregular(object, ...)
    )
    colnames(data) <- c("y", "t", "sa", "s", "i")
    data <- data[,components_, drop = FALSE]
    
    colnames(data) <- names(components_)
    
    if (is.null(dates)){
        if (!is.null(object$parameters$series_time)){
            dates <- object$parameters$series_time
        } else {
            dates <- seq.int(from = 0, length.out = nrow(data) , by = 1) 
        }
    }
    
    data_plot <- data.frame(
        date = rep(dates, ncol(data)),
        y = c(data), 
        label = factor(
            rep(colnames(data), each = nrow(data)), 
            levels = colnames(data)
        )
    )
    p <- ggplot2::ggplot(ggplot2::aes(x = date, y = y), 
                         data = data_plot) + 
        ggplot2::geom_line() + 
        ggplot2::facet_grid("label ~ .", scales = "free_y", 
                            switch = "y") + 
        ggplot2::ylab(NULL)
    p
}


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

