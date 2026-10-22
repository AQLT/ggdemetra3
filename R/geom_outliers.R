#' @keywords internal
#' @noRd
StatOutlier <- ggproto(
    "StatOutlier", Stat, 
    required_aes = c("x", "y"),
    default_aes = aes(x = x, y = y, label = after_stat(outlier)),
    compute_group = function(
        data, scales,
        method = c("x13", "tramoseats"), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        first_date = NULL,
        last_date = NULL,
        coefficients = FALSE,
        digits = 1,
        new_data = TRUE
    ){
        result <- seasonal_adjustment(
            data = data,
            method = method,
            spec = spec,
            frequency = frequency,
            message = message,
            new_data = new_data
        )
        if (!.ggdemetra$method %in% c("x13", "tramoseats")) {
            warning("The ARIMA model is only available for X-13 and TRAMO-SEATS methods.")
            return(NULL)
        } # TODO : do it for other methods
        data <- result[["data"]]
        sa <- result[["sa"]]
        frequency <- result[["frequency"]]
        
        reg_names <- rjd3toolkit::result(sa, "regression.description")
        liste_outlier <- grep("(^LS)|(^AO)|(^TC)|(^SO) ",
                              reg_names)
        
        if (length(liste_outlier) == 0)
            return(NULL)
        liste_outlier_name <- reg_names[liste_outlier]
        
        # Extraction of the date
        date <- gsub("(^.* )|(\\()|(\\))", "", liste_outlier_name)
        date <- vapply(strsplit(date, "-", fixed = TRUE),function(x){
            x <- as.numeric(x)
            x[1] + (x[2] - 1)/frequency
        },
        FUN.VALUE = numeric(1)
        )
        
        out_to_keep <- seq_along(date)
        if (!is.null(first_date))
            out_to_keep <- intersect(out_to_keep, which(date >= first_date))
        if (!is.null(last_date))
            out_to_keep <- intersect(out_to_keep, which(date <= last_date))
        
        date <- date[out_to_keep]
        liste_outlier_name <- liste_outlier_name[out_to_keep]
        
        # if (length(liste_outlier) == 0)
        #     return(NULL)
        if (length(out_to_keep) == 0)
            return(NULL)
        
        label_outlier <- liste_outlier_name
        if (coefficients) {
            reg_coef <- rjd3toolkit::result(
                sa, "regression.details.coefficients"
                )[liste_outlier]
            reg_coef <- reg_coef[out_to_keep]
            label_outlier <- sprintf(
                paste0("%s: %.",digits,"f"),
                liste_outlier_name,
                reg_coef
            )
        }
        
        id_date <- match(as.character(round(date, 3)),
                         as.character(round(result[["dates"]], 3)))
        data_final <- data.frame(
            x = data$x[id_date],
            y = data$y[id_date],
            outlier =  label_outlier,
            stringsAsFactors = FALSE
        )
        data_final
    }
)


#' Outliers Texts
#'
#' Function to directly add the outliers used in the pre-adjustment process of the seasonal adjustment to the plot.
#' Only available for X-13 ([rjd3x13::x13()]) and TRAMO-SEATS ([rjd3tramoseats::tramoseats()]) methods.
#'
#' @inheritParams geom_sa
#' @inheritParams geom_arima
#' @param geom Character. The geometry to use to display the data: 
#'   `GeomText` (`geom = "text"`, see [ggplot2::geom_text()]); 
#'   `GeomLabel` (`geom = "label"`, see [ggplot2::geom_label()]); 
#'   `GeomTextRepel` (`geom = "text_repel"`, the default, see [ggrepel::geom_text_repel()]); 
#'   `GeomLabelRepel` (`geom = "label_repel"`, see [ggrepel::geom_label_repel()]).
#' @param ... Other arguments passed on to [ggplot2::layer()]. They may be parameters of 
#'   [ggplot2::geom_text()] (if `geom = "text"`), 
#'   [ggplot2::geom_label()] (if `geom = "label"`), 
#'   [ggrepel::geom_text_repel()] (if `geom = "text_repel"`) 
#'   or [ggrepel::geom_label_repel()] (if `geom = "label_repel"`).
#' @param first_date A numeric specifying the first date from which the outliers are plotted. 
#'   By default (`first_date = NULL`), the outliers are plotted from the beginning of the time series.
#' @param last_date A numeric specifying the last date up to which the outliers are plotted. 
#'   By default (`last_date = NULL`), the outliers are plotted until the end of the time series.
#' @param coefficients Logical indicating whether the estimated coefficients are printed. 
#'   By default `coefficients = FALSE`.
#' @param digits Integer indicating the number of decimal places to be used for the estimated coefficients of the outliers. 
#'   By default `digits = 1`.
#'
#' @details 
#' With `geom = "text"`, the outliers used in the pre-adjustment process of the seasonal adjustment are directly added to the plot. 
#' With `geom = "label"`, a rectangle is drawn behind the names of the outliers, making them easier to read. 
#' The same applies to `geom = "text_repel"` or `geom = "label_repel"`, but text labels are also repelled away from each other and away from the data points (see [ggrepel::geom_label_repel()]).
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
#' # To add the outliers:
#' p_sa_ipi_fr + geom_outlier(
#'     geom = "label",
#'     message = FALSE
#' )
#'
#' # To have a more readable plot with outlier names repelled away from each other 
#' # and from the data points:
#' p_sa_ipi_fr + 
#'     geom_outlier(
#'         geom = "label_repel",
#'         message = FALSE,
#'         vjust = 4,
#'         ylim = c(NA, 65), force = 10,
#'         arrow = arrow(
#'             length = unit(0.03, "npc"),
#'             type = "closed", ends = "last"
#'         )
#'     )
#'
#' # To only plot the outliers from a specific date (2009):
#' p_sa_ipi_fr + 
#'     geom_outlier(
#'         geom = "label_repel",
#'         message = FALSE,
#'         first_date = 2009,
#'         vjust = 4,
#'         ylim = c(NA, 65), force = 10,
#'         arrow = arrow(
#'             length = unit(0.03, "npc"),
#'             type = "closed", ends = "last"
#'         )
#'     )
#' @return A \pkg{ggplot2} layer object to be added to a plot.
#' @export
geom_outlier <- function(
        mapping = NULL, data = NULL,
        stat = "outlier",
        geom = c("text", "label",
                 "text_repel", "label_repel"),
        position = "identity", ...,
        method = c("x13", "tramoseats"), 
        spec = NULL,
        frequency = NULL,
        message = TRUE,
        first_date = NULL,
        last_date = NULL,
        coefficients = FALSE,
        digits = 1,
        show.legend = NA, 
        inherit.aes = TRUE
) {
    geom <- match.arg(geom)
    if (geom == "label_repel") {
        geom <- GeomLabelRepel
    } else if (geom == "text_repel") {
        geom <- GeomTextRepel
    } else if (geom == "label") {
        geom <- GeomLabel
    } else {
        geom <- GeomText
    }
    
    ggplot2::layer(
        data = data, mapping = mapping, stat = stat, geom = geom, 
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(
            method = method, spec = spec, 
            frequency = frequency, message = message,
            first_date = first_date, last_date = last_date,
            coefficients = coefficients, digits = digits,
            new_data = !missing(data) || !is.null(data),
            ...
        )
    )
}

