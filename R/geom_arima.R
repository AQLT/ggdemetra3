#' @keywords internal
#' @noRd
#' @importFrom ggplot2 ggproto
StatArima <- ggproto(
    "StatArima", Stat, 
    required_aes = c("x", "y"),
    default_aes = aes(x = x, y = y, label = after_stat(arima_model)),
    compute_group = function(
        data, scales,
        method = c("x13", "tramoseats"), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        x_arima = NULL, y_arima = NULL,
        new_data = TRUE
    ) {
        result <- seasonal_adjustment(
            data = data,
            method = method,
            spec = spec,
            frequency = frequency,
            message = message,
            new_data = new_data)
        if (!.ggdemetra$method %in% c("x13", "tramoseats")) {
            warning("The ARIMA model is only available for X-13 and TRAMO-SEATS methods.")
            return(NULL)
        } # TODO : do it for other methods
        data <- result[["data"]]
        sa <- result[["sa"]]
        arima_model <- rjd3toolkit::user_defined(
            sa,
            sprintf("arima.%s",
                    c("p","d","q","bp","bd","bq")))
        
        arima_model <- paste0(
            "ARIMA(",arima_model[[1]], ",",
            arima_model[[2]], ",",
            arima_model[[3]], ")(",
            arima_model[[4]], ",",
            arima_model[[5]], ",",
            arima_model[[6]], ")")
        data <- data[1, ]
        if (!is.null(x_arima))
            data$x <- x_arima
        if (!is.null(y_arima))
            data$y <- y_arima
        data$arima_model <- arima_model
        data
    }
)
#' Add ARIMA Model to Plots
#'
#' Function to directly add the ARIMA model used in the pre-adjustment process of seasonal adjustment to the plot.
#' Only available for X-13 ([rjd3x13::x13()]) and TRAMO-SEATS ([rjd3tramoseats::tramoseats()]) methods.
#'
#' @inheritParams geom_sa
#' @param spec the specification used for the seasonal adjustment. 
#'  For `"x13"` and `"tramoseats"` methods, it can be the name of a pre-defined specification 
#'  or a user-defined specification created with [rjd3x13::spec_x13()] or [rjd3tramoseats::spec_tramoseats()].
#' @param geom Character. The geometric object to use to display the data: 
#'   `GeomText` (`geom = "text"`, the default, see [ggplot2::geom_text()]) or 
#'   `GeomLabel` (`geom = "label"`, see [ggplot2::geom_label()]).
#' @param x_arima,y_arima Position of the ARIMA model text. 
#' By default, the first position of the `data` is used.
#'
#' @details 
#' With `geom = "text"`, the ARIMA model used in the pre-adjustment process of seasonal adjustment is directly added to the plot. 
#' With `geom = "label"`, a rectangle is drawn behind the ARIMA model text, making it easier to read.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' p_sa_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
#'     geom_line() +
#'     labs(
#'         title = "Seasonal adjustment of the French industrial production index",
#'         x = "time", y = NULL
#'     ) +
#'     geom_sa(color = "red", message = FALSE)
#'          
#' # To add the ARIMA model
#' p_sa_ipi_fr + 
#'     geom_arima(
#'         geom = "label",
#'         x_arima = -Inf, y_arima = -Inf, 
#'         vjust = -1, hjust = -0.1,
#'         message = FALSE
#'     )       
#' @importFrom ggplot2 GeomText GeomLabel
#' @importFrom ggrepel GeomTextRepel GeomLabelRepel
#' @return A \pkg{ggplot2} layer object to be added to a plot.
#' @export
geom_arima <- function(
        mapping = NULL, data = NULL, stat = "arima",
        geom = c("text", "label"),
        position = "identity", ...,
        method = c("x13", "tramoseats"), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        x_arima = NULL, y_arima = NULL,
        show.legend = NA, 
        inherit.aes = TRUE
) {
    geom <- match.arg(geom)
    if (geom == "text") {
        geom <- GeomText
    } else {
        geom <- GeomLabel
    }
    ggplot2::layer(
        data = data, mapping = mapping, stat = stat, geom = geom, 
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(method = method, spec = spec, 
                      frequency = frequency, message = message,
                      x_arima = x_arima, y_arima = y_arima,
                      new_data = !missing(data) || !is.null(data),
                      ...))
}

