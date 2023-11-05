#' Extract Component from 'RJDemetra' model
#' 
#' @param x a seasonal adjustment object.
#' @param forecast,backcast boolean indicating if the forecast/backcast series should be returned.
#' @param calendar if `TRUE`, `seasonal()` returns de seasonal and calendar components.
#' @param corrected if `TRUE` large extreme values are removed from the components.
#' @param ... unused parameters.
#' 
#' @importFrom stats window start end
#' @name components
#' @rdname components
#' @export
seasonal <- function(x, forecast = FALSE, calendar = FALSE, ...) {
    UseMethod("seasonal", x)
}
#' @export
seasonal.JD3_X13_OUTPUT <- function(x, forecast = FALSE, calendar = FALSE, ...){
    seasonal(x$result, forecast = forecast)
}
#' @export
seasonal.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, calendar = FALSE, ...){
    seasonal(x$result, forecast = forecast, spec = x$result_spec)
}
#' @export
seasonal.JD3_X13_RSLTS <- function(x, forecast = FALSE, calendar = FALSE, ...){
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
seasonal.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, calendar = FALSE, spec = "rsafull", ...){
    if (forecast) {
        if (calendar) {
            x$final$s$fcasts
        } else {
            mod = rjd3tramoseats::jtramoseats(raw(x), spec = spec, ...)
            rjd3toolkit::result(mod, "decomposition.s_cmp_f")
        }
    } else {
        if (calendar) {
            x$final$s$data
        } else {
            mod = rjd3tramoseats::jtramoseats(raw(x), spec = spec, ...)
            rjd3toolkit::result(mod, "decomposition.s_cmp")
        }
    }
}
#' @export
seasonal.JD3_Object <- function(x, forecast = FALSE, calendar = FALSE, ...){
    if (forecast) {
        if (calendar) {
            rjd3toolkit::result(x, "s_f")
        } else {
            if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
                window(rjd3toolkit::result(x, "decomposition.d10"),
                       start = start(rjd3toolkit::result(x, "finals.d11a")))
            } else  if (rJava::.jinstanceof(x$internal, "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults")) {
                rjd3toolkit::result(x, "decomposition.s_cmp_f")
            }
        }
    } else {
        if (calendar) {
            rjd3toolkit::result(x, "s")
        } else {
            if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
                window(rjd3toolkit::result(x, "decomposition.d10"),
                       end = end(rjd3toolkit::result(x, "y")))
            } else  if (rJava::.jinstanceof(x$internal, "jdplus/tramoseats/base/core/tramoseats/TramoSeatsResults")) {
                rjd3toolkit::result(x, "decomposition.s_f")
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
irregular.JD3_X13_OUTPUT <- function(x, forecast = FALSE, corrected = FALSE, ...){
    irregular(x$result, forecast = forecast, corrected = corrected)
}
#' @export
irregular.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, corrected = FALSE, ...){
    irregular(x$result, forecast = forecast, corrected = corrected)
}
#' @export
irregular.JD3_X13_RSLTS <- function(x, forecast = FALSE, corrected = FALSE, ...){
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
irregular.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, ...){
    if (forecast) {
        x$final$i$fcasts
    } else {
        x$final$i$data
    }
}
#' @export
irregular.JD3_Object <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
            window(rjd3toolkit::result(x, "decomposition.d13"),
                   start = start(rjd3toolkit::result(x, "finals.d11a")))
        } else {
            rjd3toolkit::result(x, "i_f")
        }
    } else {
        if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
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
seasonaladj.JD3_X13_OUTPUT <- function(x, forecast = FALSE, corrected = FALSE, ...){
    seasonaladj(x$result, forecast = forecast, corrected = corrected)
}
#' @export
seasonaladj.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, corrected = FALSE, ...){
    seasonaladj(x$result, forecast = forecast, corrected = corrected)
}
#' @export
seasonaladj.JD3_X13_RSLTS <- function(x, forecast = FALSE, corrected = FALSE, ...){
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
seasonaladj.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        x$final$sa$fcasts
    } else {
        x$final$sa$data
    }
}
#' @export
seasonaladj.JD3_Object <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        rjd3toolkit::result(x, "sa_f")
    } else {
        if (rJava::.jinstanceof(x$internal, "jdplus/x13/base/core/x13/X13Results")) {
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
calendaradj <- function(x, forecast = FALSE, ...) {
    UseMethod("calendaradj", x)
}
#' @export
calendaradj.JD3_X13_OUTPUT <- function(x, forecast = FALSE, ...){
    calendaradj(x$result, forecast = forecast)
}
#' @export
calendaradj.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, ...){
    calendaradj(x$result, forecast = forecast, spec = x$result_spec)
}
#' @export
calendaradj.JD3_X13_RSLTS <- function(x, forecast = FALSE, ...){
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
calendaradj.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, spec = "rsafull", ...){
    mod = rjd3tramoseats::jtramoseats(raw(x), spec = spec, ...)
    if (forecast) {
        rjd3toolkit::result(mod, "ycal_f")
    } else {
        rjd3toolkit::result(mod, "ycal")
    }
}
#' @export
calendaradj.JD3_Object <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        rjd3toolkit::result(x, "ycal_f")
    } else {
        rjd3toolkit::result(x, "ycal")
    }
}

#' @rdname components
#' @export
calendar <- function(x, forecast = FALSE, ...) {
    UseMethod("calendar", x)
}
#' @export
calendar.JD3_X13_OUTPUT <- function(x, forecast = FALSE, ...){
    calendar(x$result, forecast = forecast)
}
#' @export
calendar.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, ...){
    calendar(x$result, forecast = forecast, spec = x$result_spec)
}
#' @export
calendar.JD3_X13_RSLTS <- function(x, forecast = FALSE, ...){
    if (forecast) {
        x$final$d18a
    } else {
        x$final$d18
    }
}
#' @export
calendar.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, spec = "rsafull", ...){
    mod = rjd3tramoseats::jtramoseats(raw(x), spec = spec, ...)
    if (forecast) {
        rjd3toolkit::result(mod, "cal_f")
    } else {
        rjd3toolkit::result(mod, "cal")
    }
}
#' @export
calendar.JD3_Object <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        rjd3toolkit::result(x, "cal_f")
    } else {
        rjd3toolkit::result(x, "cal")
    }
}

#' @rdname components
#' @export
raw <- function(x, forecast = FALSE, backcast = FALSE, ...) {
    UseMethod("raw", x)
}
#' @export
raw.JD3_X13_OUTPUT <- function(x, forecast = FALSE, backcast = FALSE, ...){
    raw(x$result)
}
#' @export
raw.JD3_TRAMOSEATS_OUTPUT <- function(x, forecast = FALSE, backcast = FALSE, ...){
    raw(x$result)
}
#' @export
raw.JD3_X13_RSLTS <- function(x, forecast = FALSE, backcast = FALSE, ...){
    if (forecast) {
        x$preadjust$a1a
    } else if (backcast) {
        x$preadjust$a1b
    } else {
        x$preadjust$a1
    }
}
#' @export
raw.JD3_TRAMOSEATS_RSLTS <- function(x, forecast = FALSE, backcast = FALSE, ...){
    if (forecast) {
        x$final$series$fcasts
    } else {
        x$final$series$data
    }
}
#' @export
raw.JD3_Object <- function(x, forecast = FALSE, backcast = FALSE, ...){
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
seasonal.JDX11 <- function(x, forecast = FALSE, calendar = FALSE, spec = "rsafull", ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$s
    }
}
#' @export
trendcycle.JDX11 <- function(x, forecast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$t
    }
}
#' @export
irregular.JDX11 <- function(x, forecast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$i
    }
}
#' @export
seasonaladj.JDX11 <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$sa
    }
}
#' @export
raw.JDX11 <- function(x, forecast = FALSE, backcast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$y
    }
}

#' @export
seasonal.JDFractionalAirlineDecomposition <- function(x, forecast = FALSE, calendar = FALSE, spec = "rsafull", ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$s
    }
}
#' @export
trendcycle.JDFractionalAirlineDecomposition <- function(x, forecast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$t
    }
}
#' @export
irregular.JDFractionalAirlineDecomposition <- function(x, forecast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$i
    }
}
#' @export
seasonaladj.JDFractionalAirlineDecomposition <- function(x, forecast = FALSE, corrected = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$sa
    }
}
#' @export
raw.JDFractionalAirlineDecomposition <- function(x, forecast = FALSE, backcast = FALSE, ...){
    if (forecast) {
        NULL
    } else {
        x$decomposition$y
    }
}
