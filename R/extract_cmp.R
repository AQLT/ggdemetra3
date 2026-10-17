#' Extract Components from Seasonal Adjustment Objects
#'
#' @param x A seasonal adjustment model (from \pkg{rjd3x13}, \pkg{rjd3tramoseats}, or \pkg{rjd3highfreq}).
#' @param forecast,backcast Logicals indicating whether the forecast or backcast series should be returned.
#' If `forecast = TRUE` and `backcast = TRUE`, the function only returns the forecast series. 
#' Default is `FALSE`.
#' Ignored for \pkg{rjd3highfreq} objects.
#' @param calendar If `TRUE`, `seasonal()` returns the seasonal and calendar components.
#' Ignored for \pkg{rjd3highfreq} objects.
#' @param corrected If `TRUE`, large extreme values are removed from the components.
#' Ignored for \pkg{rjd3tramoseats} and \pkg{rjd3highfreq} objects.
#' @param ... Additional arguments passed to methods.
#' @param spec The specification used to estimate the model.
#' Only used for `"JD3_TRAMOSEATS_OUTPUT"` objects for functions 
#' `calendaradj()`, `calendar()` and `raw()` (when `forecast = FALSE` and `backast = TRUE`) because
#' the output doesn't contain the desired component and the specification.
#' @param seas_freq For methods with multiple seasonal frequencies 
#' ([rjd3highfreq::multi_airline_decomposition()], [rjd3highfreq::istl()] or [rjd3highfreq::mstl()]), 
#' the seasonal frequency to extract. If `NULL`, the first seasonal frequency is returned.
#' 
#' @details
#' These functions are S3 generics supporting several classes from the `rjdverse` ecosystem:
#' * X-13 models (\pkg{rjd3x13}): `JD3_X13_OUTPUT`, `JD3_X13_RSLTS`, `JD3_Object`;
#' * TRAMO-SEATS models (\pkg{rjd3tramoseats}): `JD3_TRAMOSEATS_OUTPUT`, `JD3_TRAMOSEATS_RSLTS`;
#' * High-frequency decompositions (\pkg{rjd3highfreq}): `hf_decomposition` 
#' (except for `calendaradj()` and `calendar()`, which are not supported for this class).
#' 
#'
#' @return A `ts` object (or a vector of numeric for `"hf_decomposition"` objects) containing the requested component.
#' 
#' @importFrom stats window start end
#' @name components
#' @rdname components
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' model <- rjd3x13::jx13(ipi_c_eu[, "FR"])
#' seasonal(model)
#' raw(model, forecast = TRUE)
#' @returns A `ts` object with the requested component.
#' @export
seasonal <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
) {
    UseMethod("seasonal", x)
}
#' @export
seasonal.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
){
    seasonal(x$result, forecast = forecast)
}
#' @export
seasonal.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
){
    seasonal(x$result, forecast = forecast)
}
#' @export
seasonal.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
){
    if (forecast) {
        if (calendar) {
            x$final$d16a
        } else {
            window(x$decomposition$d10, start = start(x$final$d11a))
        }
    } else {
        if (calendar) {
            x$final$d16
        } else {
            window(x$decomposition$d10, end = end(x$preadjust$a1))
        }
    }
}
#' @export
seasonal.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
){
    if (forecast) {
        if (calendar) {
            x$final$s$fcast
        } else {
            x$decomposition$stochastics$s$fcasts
        }
    } else {
        if (calendar) {
            x$final$s$data
        } else {
            x$decomposition$stochastics$s$data
        }
    }
}
#' @export
seasonal.JD3_Object <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL, ...
){
    if (forecast) {
        if (calendar) {
            rjd3toolkit::result(x, "s_f")
        } else {
            if (is_jx13(x)) {
                window(rjd3toolkit::result(x, "decomposition.d10"),
                       start = start(rjd3toolkit::result(x, "finals.d11a")))
            } else  if (is_jtramoseats(x)) {
                rjd3toolkit::result(x, "decomposition.s_cmp_f")
            } else {
                stop("The object is not a seasonal adjustment model.")
            }
        }
    } else {
        if (calendar) {
            rjd3toolkit::result(x, "s")
        } else {
            if (is_jx13(x)) {
                window(rjd3toolkit::result(x, "decomposition.d10"),
                       end = end(rjd3toolkit::result(x, "y")))
            } else  if (is_jtramoseats(x)) {
                rjd3toolkit::result(x, "decomposition.s_f")
            } else {
                stop("The object is not a seasonal adjustment model.")
            }
        }
    }
}
#' @rdname components
#' @export
trendcycle <- function(x, forecast = FALSE, ...) {
    UseMethod("trendcycle", x)
}
#' @export
trendcycle.JD3_X13_OUTPUT <- function(x, forecast = FALSE, ...){
    trendcycle(x$result, forecast = forecast)
}

# seasonal.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, calendar = FALSE, spec = "rsafull", ...){
#     if (forecast) {
#         if (calendar) {
#             x$final$s$fcasts
#         } else {
#             mod <- rjd3tramoseats::.jtramoseats(raw(x), spec = spec, ...)
#             rjd3toolkit::result(mod, "decomposition.s_cmp_f")
#         }
#     } else {
#         if (calendar) {
#             x$final$s$data
#         } else {
#             mod <- rjd3tramoseats::.jtramoseats(raw(x), spec = spec, ...)
#             rjd3toolkit::result(mod, "decomposition.s_cmp")
#         }
#     }
# }
#' @export
trendcycle.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, ...){
    trendcycle(x$result, forecast = forecast)
}
#' @export
trendcycle.JD3_X13_RSLTS <- function(x, forecast = FALSE, ...){
    if (forecast) {
        x$final$d12a
    } else {
        x$final$d12final
    }
}
#' @export
trendcycle.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, ...){
    if (forecast) {
        x$final$t$fcasts
    } else {
        x$final$t$data
    }
}
#' @export
trendcycle.JD3_Object <- function(x, forecast = FALSE, ...){
    if (forecast) {
        rjd3toolkit::result(x, "t_f")
    } else {
        rjd3toolkit::result(x, "t")
    }
}
#' @rdname components
#' @export
irregular <- function(x, forecast = FALSE, corrected = FALSE, ...) {
    UseMethod("irregular", x)
}
#' @export
irregular.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    irregular(x$result, forecast = forecast, corrected = corrected)
}
#' @export
irregular.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    irregular(x$result, forecast = forecast, corrected = corrected)
}
#' @export
irregular.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    if (forecast) {
        window(x$decomposition$d13, start = start(x$final$d11a))
    } else {
        if (corrected) {
            x$final$e3
        } else {
            x$final$d13final
        }
    }
}
#' @export
irregular.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, corrected = FALSE, ...)
{
    if (forecast) {
        x$final$i$fcasts
    } else {
        x$final$i$data
    }
}
#' @export
irregular.JD3_Object <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        if (is_jx13(x)) {
            window(rjd3toolkit::result(x, "decomposition.d13"),
                   start = start(rjd3toolkit::result(x, "finals.d11a")))
        } else {
            rjd3toolkit::result(x, "i_f")
        }
    } else {
        if (is_jx13(x)) {
            if (corrected) {
                rjd3toolkit::result(x, "finals.e3")
            } else {
                rjd3toolkit::result(x, "i")
            }
        } else {
            rjd3toolkit::result(x, "i")
        }
    }
}

#' @rdname components
#' @export
seasonaladj <- function(x, forecast = FALSE, corrected = FALSE, ...) {
    UseMethod("seasonaladj", x)
}
#' @export
seasonaladj.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    seasonaladj(x$result, forecast = forecast, corrected = corrected)
}
#' @export
seasonaladj.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    seasonaladj(x$result, forecast = forecast, corrected = corrected)
}
#' @export
seasonaladj.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    if (forecast) {
        x$final$d11a
    } else {
        if (corrected) {
            x$final$e2
        } else {
            x$final$d11final
        }
    }
}
#' @export
seasonaladj.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    if (forecast) {
        x$final$sa$fcasts
    } else {
        x$final$sa$data
    }
}
#' @export
seasonaladj.JD3_Object <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    if (forecast) {
        rjd3toolkit::result(x, "sa_f")
    } else {
        if (is_jx13(x)) {
            if (corrected) {
                rjd3toolkit::result(x, "finals.e2")
            } else {
                rjd3toolkit::result(x, "sa")
            }
        } else  {
            rjd3toolkit::result(x, "sa")
        }
    }
}

#' @rdname components
#' @export
calendaradj <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    UseMethod("calendaradj", x)
}
#' @export
calendaradj.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    calendaradj(x$result, forecast = forecast)
}
#' @export
calendaradj.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    calendaradj(x$result, forecast = forecast, spec = x$result_spec)
}
#' @export
calendaradj.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    if (forecast) {
        y <- x$preadjust$a1a
    } else {
        y <- x$preadjust$a1
    }
    if (x$preprocessing$description$log) {
        y / calendar(x, forecast = FALSE)
    } else {
        y - calendar(x, forecast = FALSE)
    }
}
#' @export
calendaradj.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    mod <- rjd3tramoseats::.jtramoseats(raw(x), spec = spec, ...)
    if (forecast) {
        rjd3toolkit::result(mod, "ycal_f")
    } else {
        rjd3toolkit::result(mod, "ycal")
    }
}
#' @export
calendaradj.JD3_Object <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    if (forecast) {
        rjd3toolkit::result(x, "ycal_f")
    } else {
        rjd3toolkit::result(x, "ycal")
    }
}

#' @rdname components
#' @export
calendar <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    UseMethod("calendar", x)
}
#' @export
calendar.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    calendar(x$result, forecast = forecast)
}
#' @export
calendar.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    calendar(x$result, forecast = forecast, spec = x$result_spec)
}
#' @export
calendar.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    if (forecast) {
        x$final$d18a
    } else {
        x$final$d18
    }
}
#' @export
calendar.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    mod <- rjd3tramoseats::.jtramoseats(raw(x), spec = spec, ...)
    if (forecast) {
        rjd3toolkit::result(mod, "cal_f")
    } else {
        rjd3toolkit::result(mod, "cal")
    }
}
#' @export
calendar.JD3_Object <- function(
        x, forecast = FALSE, spec = NULL, ...
){
    if (forecast) {
        rjd3toolkit::result(x, "cal_f")
    } else {
        rjd3toolkit::result(x, "cal")
    }
}

#' @rdname components
#' @export
raw <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
) {
    UseMethod("raw", x)
}
#' @export
raw.JD3_X13_OUTPUT <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
){
    raw(x$result, forecast = forecast, backcast = backcast)
}
#' @export
raw.JD3_TRAMOSEATS_OUTPUT <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
){
    raw(x$result, forecast = forecast, backcast = backcast, spec = x$result_spec)
}
#' @export
raw.JD3_X13_RSLTS <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
){
    if (forecast) {
        x$preadjust$a1a
    } else if (backcast) {
        x$preadjust$a1b
    } else {
        x$preadjust$a1
    }
}
#' @export
raw.JD3_TRAMOSEATS_RSLTS <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
){
    if (forecast) {
        x$final$series$fcasts
    } else if (backcast) {
        y <- x$final$series$data
        raw(
            rjd3tramoseats::.jtramoseats(raw(x), spec = spec, ...),
            forecast = FALSE,
            backast = TRUE
        )
    }else {
        x$final$series$data
    }
}
#' @export
raw.JD3_Object <- function(
        x, forecast = FALSE, backcast = FALSE, spec = NULL, ...
){
    if (forecast) {
        rjd3toolkit::result(x, "y_f")
    } else if (backcast) {
        rjd3toolkit::result(x, "y_b")
    } else {
        rjd3toolkit::result(x, "y")
    }
}


# rjd3highfreq:
#' @export
seasonal.hf_decomposition <- function(
        x, forecast = FALSE, calendar = FALSE, seas_freq = NULL,...){
    if (!is.null(x$decomposition$s)) {
        x$decomposition$s
    } else {
        # Multiple seasonal frequencies
        i_seas <- NULL
        if (!is.null(seas_freq)) {
            i_seas <- grep(
                sprintf("^(ts_component\\.)*s-*%s$", seas_freq),
                colnames(x$decomposition)
            )
        }
        if (is.null(i_seas)) {
            i_seas <- grep(
                "^(ts_component\\.)*s-*\\d+$",
                colnames(x$decomposition)
            )[1]
        }
        
        x$decomposition[, i_seas]
    }
}
#' @export
trendcycle.hf_decomposition <- function(x, forecast = FALSE, ...){
    i_tc <- grep("^(ts_component\\.)*t$",colnames(x$decomposition))
    x$decomposition[,i_tc]
}
#' @export
irregular.hf_decomposition <- function(x, forecast = FALSE, ...){
    i_irr <- grep("^(ts_component\\.)*i$",colnames(x$decomposition))
    x$decomposition[,i_irr]
}
#' @export
seasonaladj.hf_decomposition <- function(
        x, forecast = FALSE, corrected = FALSE, ...
){
    x$decomposition$sa
}
#' @export
raw.hf_decomposition <- function(x, forecast = FALSE, backcast = FALSE, spec = NULL, ...){
    x$decomposition$series
}
