#' 'ggplot2' functions for 'rjd3filters' plots
#'
#' @param x,zero_as_na,q,ylab,nxlab,xlim,n,normalized,... see function [rjd3filters::plot_coef].
#'
#' @export
#' @name ggplot_filters
#' @rdname ggplot_filters
ggplot_coef <- function(x, zero_as_na = TRUE, ylab = "Coefficients", ...){
    UseMethod("ggplot_coef", x)
}
#' @importFrom stats na.omit
#' @export
ggplot_coef.default <- function(x, zero_as_na = TRUE, ylab = "Coefficients", q = 0, ...){
    if (zero_as_na)
        x <- apply(x, 2, trailingzero_as_na)
    col_to_plot <- sprintf("q=%i",q)
    col_to_plot <- col_to_plot[col_to_plot %in% colnames(x)]
    data <- data.frame(x[,col_to_plot])
    colnames(data) <- col_to_plot
    data$date <- factor(rownames(data), levels = rownames(data),ordered = TRUE)
    dataGraph <- reshape2::melt(data, id.vars = "date")
    dataGraph <- na.omit(dataGraph)
    ggplot2::ggplot(data = dataGraph,
                    ggplot2::aes(x = date, y = value, group = variable,
                                 colour = variable)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = ylab)
}
utils::globalVariables(c("value", "variable", "y"))
#' @rdname ggplot_filters
#' @export
ggplot_coef.finite_filters <- function(x, zero_as_na = TRUE, ylab = "Coefficients", q = 0, ...){
    ggplot_coef(as.matrix(x, zero_as_na = zero_as_na),
                zero_as_na = FALSE,
                q = q,
                ylab = ylab,
                ...)
}

#' @importFrom stats coef
#' @export
ggplot_coef.moving_average <- function(x, zero_as_na = TRUE, ylab = "Coefficients", ...){
    x <- coef(x)
    var <- NULL # to avoid warning
    data <- data.frame(date = factor(names(x), levels = names(x), ordered = TRUE),
                       y = x, var = "x")
    ggplot2::ggplot(data = data,
                    ggplot2::aes(x = date, y = y, group = var, color = var)) +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::geom_point() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = ylab)
}
#' @rdname ggplot_filters
#' @export
ggplot_gain <- function(x, nxlab = 7,
                        xlim = c(0, pi),
                        ..., n = 101){
    UseMethod("ggplot_gain", x)
}
#' @export
ggplot_gain.finite_filters <- function(x, nxlab = 7,
                                       xlim = c(0, pi),
                                       q = 0,
                                       ..., n = 101){
    x_values <- seq.int(xlim[1], xlim[2], length.out = n)
    gsym <- rjd3filters::get_properties_function(x, "Symmetric Gain")
    gasym <- rjd3filters::get_properties_function(x, "Asymmetric Gain")
    all_g_f <- c(list(gsym), gasym)
    names(all_g_f)[1] <- sprintf("q=%i", rjd3filters::upper_bound(x@sfilter))
    
    col_to_plot <- sprintf("q=%i",q)
    col_to_plot <- col_to_plot[col_to_plot %in% names(all_g_f)]
    all_g_f <- all_g_f[col_to_plot]
    y_val <- sapply(all_g_f, function(f) f(x_values))
    
    data = data.frame(x = x_values, y_val)
    colnames(data) <- c("x", colnames(y_val))
    dataGraph <- reshape2::melt(data, id.vars = "x")
    
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    ggplot2::ggplot(data = dataGraph,
                    ggplot2::aes(x = x, y = value, group = variable,
                                 colour = variable)) +
        ggplot2::geom_line() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = "Gain") +
        ggplot2::scale_x_continuous(NULL,
                                    breaks = x_lab_at*pi,
                                    labels = parse(text=xlabel_ggplot(x_lab_at)))
}
#' @export
ggplot_gain.moving_average <- function(x, nxlab = 7,
                                       xlim = c(0, pi),
                                       ..., n = 101){
    x_values <- seq.int(xlim[1], xlim[2], length.out = n)
    gsym <- rjd3filters::get_properties_function(x, "Symmetric Gain")
    y_val <- gsym(x_values)
    
    data = data.frame(x = x_values, value = y_val, variable = "x")
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    ggplot2::ggplot(data = data,
                    ggplot2::aes(x = x, y = value, group = variable,
                                 colour = variable)) +
        ggplot2::geom_line() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = "Gain") +
        ggplot2::scale_x_continuous(NULL,
                                    breaks = x_lab_at*pi,
                                    labels = parse(text=xlabel_ggplot(x_lab_at)))
}
#' @rdname ggplot_filters
#' @export
ggplot_phase <- function(x, nxlab = 7,
                         xlim = c(0, pi), normalized = FALSE,
                         ..., n = 101){
    UseMethod("ggplot_phase", x)
}
#' @export
ggplot_phase.finite_filters <- function(x, nxlab = 7,
                                        xlim = c(0, pi), normalized = FALSE, q = 0,
                                        ..., n = 101){
    x_values <- seq.int(xlim[1], xlim[2], length.out = n)
    gsym <- rjd3filters::get_properties_function(x, "Symmetric Phase")
    gasym <- rjd3filters::get_properties_function(x, "Asymmetric Phase")
    all_g_f <- c(list(gsym), gasym)
    names(all_g_f)[1] <- sprintf("q=%i", rjd3filters::upper_bound(x@sfilter))
    
    col_to_plot <- sprintf("q=%i",q)
    col_to_plot <- col_to_plot[col_to_plot %in% names(all_g_f)]
    all_g_f <- all_g_f[col_to_plot]
    y_val <- sapply(all_g_f, function(f) f(x_values))
    if(normalized){
        y_val[-1,] <- y_val[-1,] / x_values[-1]
    }
    data = data.frame(x = x_values, y_val)
    colnames(data) <- c("x", colnames(y_val))
    dataGraph <- reshape2::melt(data, id.vars = "x")
    
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    ggplot2::ggplot(data = dataGraph,
                    ggplot2::aes(x = x, y = value, group = variable,
                                 colour = variable)) +
        ggplot2::geom_line() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = "Phase") +
        ggplot2::scale_x_continuous(NULL,
                                    breaks = x_lab_at*pi,
                                    labels = parse(text=xlabel_ggplot(x_lab_at)))
}
#' @export
ggplot_phase.moving_average <- function(x, nxlab = 7,
                                        xlim = c(0, pi), normalized = FALSE,
                                        ..., n = 101){
    x_values <- seq.int(xlim[1], xlim[2], length.out = n)
    gsym <- rjd3filters::get_properties_function(x, "Symmetric Phase")
    y_val <- gsym(x_values)
    
    data = data.frame(x = x_values, value = y_val, variable = "x")
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
    ggplot2::ggplot(data = data,
                    ggplot2::aes(x = x, y = value, group = variable,
                                 colour = variable)) +
        ggplot2::geom_line() +
        theme_ggplot() +
        ggplot2::labs(x = NULL, y = "Phase") +
        ggplot2::scale_x_continuous(NULL,
                                    breaks = x_lab_at*pi,
                                    labels = parse(text=xlabel_ggplot(x_lab_at)))
}

trailingzero_as_na <- function(x){
    i <- length(x)
    while (x[i] == 0 && i > 0) {
        x[i] <- NA
        i <- i - 1
    }
    x
}

#'@importFrom ggplot2 theme_grey `%+replace%`
theme_ggplot <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                       panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
                       panel.grid.major = ggplot2::element_line(colour = "grey92"),
                       panel.grid.minor = ggplot2::element_line(colour = "grey92",
                                                                size = 0.25),
                       strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20"),
                       complete = TRUE, plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.title=ggplot2::element_blank())
}

#' @importFrom MASS fractions
xlabel_ggplot <- function(x, symbol = "pi"){
    fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")  # convert to fractions
    labels <- sapply(fracs, function(i)
        if (length(i) > 1) { paste(i[1], "*", symbol, "/", i[2]) }
        else { paste(i, "*", symbol) })
    labels <- sub("0 * pi", "0", labels, fixed = TRUE)
    labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
    labels
}
