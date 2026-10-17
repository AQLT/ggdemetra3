#' @importFrom stats ts is.mts is.ts time
#' @keywords internal
#' @noRd
seasonal_adjustment <- function(
        data,
        method = c(
            "x13","tramoseats","x11-extended", "fractionalairline",
            "multiairline", "stl",
            "istl", "mstl"
        ),
        spec = NULL,
        context = NULL,
        frequency = NULL,
        message = TRUE,
        new_data = TRUE
){
    method <- match.arg(
        tolower(method[1]),
        choices = c(
            "x13","tramoseats","x11-extended", "fractionalairline",
            "multiairline", "stl",
            "istl", "mstl"
        )
    )
    data <- data[order(data$x), ]
    
    use_previous_model <- pre_check_param(
        frequency = frequency, method = method,
        spec = spec, new_data = new_data,
        data_y = data$y)
    
    if(use_previous_model){
        sa <- .ggdemetra$sa
        data_ts <- .ggdemetra$data_ts
    }else{
        data_ts <- .ggdemetra$data_ts <-
            dataframe2ts(data = data, frequency = frequency, message = message)
        
        if (method == "x13") {
            if (is.null(spec)) {
                sa <- rjd3x13::jx13(
                    data_ts, context = context
                )
            }else{
                sa <- rjd3x13::jx13(
                    data_ts, spec = spec, context = context
                )
            }
        } else if (method == "tramoseats") {
            if (is.null(spec)) {
                sa <- rjd3tramoseats::.jtramoseats(
                    data_ts, context = context
                )
            }else{
                sa <- rjd3tramoseats::.jtramoseats(
                    data_ts, spec = spec, context = context
                )
            }
        } else {
            hf_fun <- list(
                "fractionalairline" = rjd3highfreq::fractional_airline_decomposition,
                "multiairline"      = rjd3highfreq::multi_airline_decomposition,
                "x11-extended"      = rjd3highfreq::x11plus,
                "stl"               = rjd3highfreq::stlplus,
                "istl"              = rjd3highfreq::istl,
                "mstl"              = rjd3highfreq::mstl
            )
            if (method %in% names(hf_fun)) {
                spec$series <- data_ts
                spec$period <- frequency
                if (method %in% c("fractionalairline", "multiairline")) {
                    spec$series_time <- data$x
                }
                sa <- do.call(hf_fun[[method]], spec)
            }
        }
        .ggdemetra$sa <- sa
        .ggdemetra$spec <- spec
        .ggdemetra$method <- method
        .ggdemetra$data_y <- data$y
    }
    
    
    # data$sa_model <- list(sa)
    list(data = data, sa = sa, dates = as.numeric(time(data_ts)),
         frequency = frequency(data_ts))
}
#' @keywords internal
#' @noRd
dataframe2ts <- function(data, frequency = NULL, message = TRUE){
    dates <- data$x
    
    if (inherits(dates, "Date")) {
        years <- as.numeric(format(dates, format = "%Y"))
        months <- as.numeric(format(dates, format = "%m"))
        if (is.null(frequency)) {
            frequency <- max(table(years))
            if (message)
                message(sprintf("Frequency used: %i", frequency))
        }
        dates <- years + (months - 1) / frequency
        first_date <- dates[1]
    }else{
        # Numeric format
        if (is.null(frequency)) {
            years <- trunc(round(dates, 3))
            frequency <- max(table(years))
            if (message)
                message(sprintf("Frequency used: %i", frequency))
        }
        first_date <- dates[1]
    }
    
    .ggdemetra$frequency <- frequency
    if (length(frequency) == 1 && frequency %in% c(2, 4, 6, 12)) {
        ts(data$y, start = first_date, frequency = frequency)
    } else {
        data$y
    }
    
}
#' @keywords internal
#' @noRd
ts2dataframe <- function(x){
    if (is.ts(x) && !is.mts(x)) {
        data.frame(x = as.numeric(time(x)),
                   y = as.numeric(x))
    }else{
        NULL
    }
}

.ggdemetra <- new.env(parent = emptyenv())
.ggdemetra$frequency <- 
    .ggdemetra$method <-
    .ggdemetra$sa <-
    .ggdemetra$spec <-
    .ggdemetra$data_ts <-
    .ggdemetra$data_y <-
    NULL
#' @keywords internal
#' @noRd
pre_check_param <- function(
        frequency = NULL,  
        method = c("x13","tramoseats"),
        spec = NULL,
        new_data = TRUE,
        data_y = NULL
){
    use_previous_model <- FALSE
    if(any(new_data,
           is.null(.ggdemetra$frequency),
           is.null(.ggdemetra$method),
           is.null(.ggdemetra$sa)
    )){
        .ggdemetra$frequency <- 
            .ggdemetra$method <-
            .ggdemetra$sa <-
            .ggdemetra$data_ts <-
            .ggdemetra$data_y <-
            NULL
        return(use_previous_model)
    }
    
    method <- method[1]
    
    if((is.null(spec) || identical(spec, .ggdemetra$spec)) & 
       (is.null(method) || identical(method, .ggdemetra$method)) & 
       (is.null(frequency) || identical(frequency, .ggdemetra$frequency)) & 
       (identical(data_y, .ggdemetra$data_y))){
        use_previous_model <- TRUE
    }else{
        .ggdemetra$frequency <- 
            .ggdemetra$method <-
            .ggdemetra$sa <-
            .ggdemetra$spec <-
            .ggdemetra$data_ts <-
            .ggdemetra$data_y <-
            NULL
    }
    use_previous_model
}
