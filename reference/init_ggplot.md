# Initialise a 'ggplot2' Object with an SA Model

Initialise a 'ggplot2' Object with an SA Model

## Usage

``` r
init_ggplot(x, ...)

# S3 method for class 'JD3_X13_OUTPUT'
init_ggplot(x, context = NULL, ...)

# S3 method for class 'JD3_TRAMOSEATS_OUTPUT'
init_ggplot(x, context = NULL, ...)

# S3 method for class 'JD3_Object'
init_ggplot(x, ...)

# S3 method for class 'hf_decomposition'
init_ggplot(x, dates = NULL, ...)
```

## Arguments

- x:

  A seasonally adjusted model.

- ...:

  Other parameters passed to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- context:

  The optional context used to estimate the model.

- dates:

  Parameters used to re-estimate the model (see details).

## Value

A ggplot2 object that can be further customized.

## Details

`init_ggplot()` initializes a ggplot2 object by storing the seasonal
adjustment model used.

## Examples

``` r
mod <- rjd3x13::jx13(window(rjd3toolkit::ABS$X0.2.20.10.M, start = 2000))
init_ggplot(mod) +
    geom_line(color = "#F0B400") +
    geom_sa(component = "sa", color = "#155692")

    
```
