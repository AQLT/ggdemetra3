#' @keywords internal
#' @noRd
StatSa <- ggproto(
    "StatSa", Stat, 
    required_aes = c("x", "y"),
    compute_group = function(
        data, scales,
        method = c(
            "x13","tramoseats","x11-extended", "fractionalairline",
            "fractionalairlineestimation",  "multiairline", "stl",
            "istl", "mstl"
        ), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        component = "sa",
        new_data = TRUE
    ) {
        result <- seasonal_adjustment(
            data = data,
            method = method,
            spec = spec,
            frequency = frequency,
            message = message,
            new_data = new_data
        )
        method = method[1]
        data <- result[["data"]]
        sa <- result[["sa"]]
        component <- component[1]
        if (method %in% c("x13", "tramoseats")){
            component_ts <- rjd3toolkit::result(sa, component)
            if (!is.ts(component_ts)) {
                warning(sprintf(
                    "The component %s isn't a time series!", component
                    ))
                return(NULL)
            }
            component_df <- ts2dataframe(component_ts)
        } else {
            if (component == "y"){
                component <- "series"
            } 
            # TODO : for X-11, reestimate the model to get more data with userdefined?
            component_ts <- sa$decomposition[[component]]
            component_df <- data.frame(x = data$x, y = component_ts)
        }
        
        
        
        # # if the ts is a forecast we add the last observed value:
        # if (length(grep("^.*_f$", component)) > 0) {
        #     component_df <- rbind(tail(data[,c("x", "y")],1),
        #                           component_df)
        # }
        
        data$x <- data$y <- NULL
        if (nrow(component_df) <= nrow(data)) {
            component_df <- cbind(
                component_df,
                data[seq_len(nrow(component_df)), ]
            )
        }
        
        component_df
    }
)
#' Seasonal Adjustment Time Series
#'
#' Performs a seasonal adjustment and plots a specific component of the decomposition. 
#' `geom_sa()` and `stat_sa()` are aliases: they both use the same arguments. 
#' Use `stat_sa()` if you want to display the results with a non-standard geom.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data A `data.frame` that contains the data used for the seasonal adjustment.
#' @param geom The geometric object to use to display the data.
#' @param stat The statistical transformation to use on the data for this
#'   layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'   a call to a position adjustment function.
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`.
#' @param method The method used for the seasonal adjustment. 
#'   `"x13"` (by default) for the X-13 method (using [rjd3x13::jx13()]) and 
#'   `"tramoseats"` for TRAMO-SEATS (using [rjd3tramoseats::.jtramoseats()]).
#'   Both methods are only compatible with series of frequency 12 (monthly), 6 (bimonthly), 4 (quarterly), and 2 (semi-annual).
#'   For other frequencies, the following methods are available: 
#'   `"x11-extended"` (see [rjd3highfreq::x11plus()]), 
#'   `"fractionalairline"` (see [rjd3highfreq::fractional_airline_decomposition()]), 
#'   `"multiairline"` (see [rjd3highfreq::multi_airline_decomposition()]), 
#'   `"stl"` (see [rjd3highfreq::stlplus()]), 
#'   `"istl"` (see [rjd3highfreq::istl()]), and 
#'   `"mstl"` (see [rjd3highfreq::mstl()]).
#' @param spec The specification used for the seasonal adjustment. 
#'   For `"x13"` and `"tramoseats"` methods, it can be the name of a pre-defined specification 
#'   or a user-defined specification created with [rjd3x13::spec_x13()] or [rjd3tramoseats::spec_tramoseats()].
#'   For other methods, it can be a list of parameters used in the associated seasonal adjustment function.
#' @param frequency The frequency of the time series. By default (`frequency = NULL`),
#'   the frequency is computed automatically.
#' @param message A logical value indicating whether a message is printed with the frequency used.
#' @param component A character string specifying the component to plot. The result must be a time series. 
#'   See [rjd3x13::userdefined_variables_x13()] and [rjd3tramoseats::userdefined_variables_tramoseats()] for the available
#'   parameters for `"x13"` and `"tramoseats"`. 
#'   For other methods, the available components are the columns of the `decomposition` element of the result of 
#'   the seasonal adjustment function (see documentation of the associated function for more details).
#'   By default (`component = "sa"`), the seasonally adjusted component is plotted.
#' @param show.legend Logical. Should this layer be included in the legends?
#'   `NA`, the default, includes it if any aesthetics are mapped.
#'   `FALSE` never includes it, and `TRUE` always includes it.
#'   It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#' @return A \pkg{ggplot2} layer object to be added to a plot.
#'   
#' @examplesIf rjd3jars::check_java_version(silent = TRUE) 
#' p_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
#'     geom_line() +
#'     labs(
#'         title = "Seasonal adjustment of the French industrial production index",
#'         x = "time", y = NULL
#'     )
#'
#' # To add the seasonally adjusted series:
#' p_ipi_fr +
#'     geom_sa(color = "red")
#'
#' # To add the forecasts of the input data and the seasonally adjusted series:
#' p_sa <- p_ipi_fr +
#'     geom_sa(component = "y_f", linetype = 2, message = FALSE) + 
#'     geom_sa(component = "sa", color = "red", message = FALSE) +
#'     geom_sa(component = "sa_f", color = "red", linetype = 2, message = FALSE)
#' p_sa 
#'
#' @importFrom ggplot2 GeomLine
#' @export
geom_sa <- function(
        mapping = NULL, data = NULL, stat = "sa",
        position = "identity", ...,
        method = c(
            "x13","tramoseats","x11-extended", "fractionalairline",
            "multiairline", "stl",
            "istl", "mstl"
        ),
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        component = "sa",
        show.legend = NA, 
        inherit.aes = TRUE
) {
    ggplot2::layer(
        data = data, mapping = mapping, stat = stat, geom = GeomLine, 
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(
            method = method, spec = spec, 
            frequency = frequency, message = message,
            component = component,
            new_data = !missing(data) || !is.null(data),
            ...)
    )
}
#' @rdname geom_sa
#' @name geom_sa
#' @export
stat_sa <- function(
        mapping = NULL, data = NULL, geom = "line",
        position = "identity", ...,
        method = c(
            "x13","tramoseats","x11-extended", "fractionalairline",
            "fractionalairlineestimation",  "multiairline", "stl",
            "istl", "mstl"
        ), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        component = "sa",
        show.legend = NA, 
        inherit.aes = TRUE) {
    ggplot2::layer(
        stat = StatSa, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(
            method = method, spec = spec, 
            frequency = frequency, message = message,
            component = component,
            new_data = !missing(data) || !is.null(data),
            ...
        )
    )
}
