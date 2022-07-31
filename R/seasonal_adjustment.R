#' @importFrom stats ts is.mts is.ts time
seasonal_adjustment <- function(data,
                                method = c("x13","tramoseats",
                                           "x11-extended", "fractionalairline", "multiairline", "stl"),
                                spec = NULL,
                                frequency = NULL,
                                message = TRUE,
                                new_data = TRUE){
    method <- match.arg(tolower(method[1]),
                        choices = c("x13","tramoseats","x11-extended", "fractionalairline", "multiairline", "stl"))
    data <- data[order(data$x), ]
    
    use_previous_model <- pre_check_param(frequency = frequency, method = method,
                                          spec = spec, new_data = new_data,
                                          data_y = data$y)
    
    if(use_previous_model){
        sa <- .demetra$sa
        data_ts <- .demetra$data_ts
    }else{
        data_ts <- .demetra$data_ts <-
            dataframe2ts(data = data, frequency = frequency, message = message)
        
        if (method == "x13") {
            if (is.null(spec)) {
                sa <- rjd3x13::jx13(data_ts)
            }else{
                sa <- rjd3x13::jx13(data_ts, spec = spec)
            }
        } else if (method == "tramoseats") {
            if (is.null(spec)) {
                sa <- rjd3tramoseats::jtramoseats(data_ts)
            }else{
                sa <- rjd3tramoseats::jtramoseats(data_ts, spec = spec)
            }
        } else if (method == "fractionalairline") {
            spec$y = data_ts
            spec$period = frequency
            sa <- do.call(rjd3highfreq::fractionalAirlineDecomposition, spec)
        } else if (method == "multiairline") {
            spec$y = data_ts
            spec$period = frequency
            sa <- do.call(rjd3highfreq::multiAirlineDecomposition, spec)
        } else if (method == "x11-extended") {
            spec$y = data_ts
            spec$period = frequency
            sa <- do.call(rjd3highfreq::x11, spec)
        } else if (method == "stl") {
            spec$y = data_ts
            spec$period = frequency
            sa <- do.call(rjd3highfreq::stl, spec)
        }
        .demetra$sa <- sa
        .demetra$spec <- spec
        .demetra$method <- method
        .demetra$data_y <- data$y
    }
    
    
    # data$sa_model <- list(sa)
    list(data = data, sa = sa, dates = as.numeric(time(data_ts)),
         frequency = frequency(data_ts))
}
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
    
    .demetra$frequency <- frequency
    if (frequency %in% c(2, 4, 6, 12)) {
        ts(data$y, start = first_date, frequency = frequency)
    } else {
        data$y
    }
    
}
ts2dataframe <- function(x){
    if (is.ts(x) & !is.mts(x)) {
        data.frame(x = as.numeric(time(x)),
                   y = as.numeric(x))
    }else{
        NULL
    }
    
}

.demetra <- new.env(parent = emptyenv())
.demetra$frequency <- 
    .demetra$method <-
    .demetra$sa <-
    .demetra$spec <-
    .demetra$data_ts <-
    .demetra$data_y <-
    NULL

pre_check_param <- function(frequency = NULL,  
                            method = c("x13","tramoseats"),
                            spec = NULL,
                            new_data = TRUE,
                            data_y = NULL){
    use_previous_model <- FALSE
    if(any(new_data,
           is.null(.demetra$frequency),
           is.null(.demetra$method),
           is.null(.demetra$sa)
    )){
        .demetra$frequency <- 
            .demetra$method <-
            .demetra$sa <-
            .demetra$data_ts <-
            .demetra$data_y <-
            NULL
        return(use_previous_model)
    }
    
    method <- method[1]
    
    if((is.null(spec) || identical(spec, .demetra$spec)) & 
       (is.null(method) || identical(method, .demetra$method)) & 
       (is.null(frequency) || identical(frequency, .demetra$frequency)) & 
       (identical(data_y, .demetra$data_y))){
        use_previous_model <- TRUE
    }else{
        .demetra$frequency <- 
            .demetra$method <-
            .demetra$sa <-
            .demetra$spec <-
            .demetra$data_ts <-
            .demetra$data_y <-
            NULL
    }
    use_previous_model
}
