# Add ARIMA Model to Plots

Function to directly add the ARIMA model used in the pre-adjustment
process of seasonal adjustment to the plot. Only available for X-13
([`rjd3x13::x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html))
and TRAMO-SEATS
([`rjd3tramoseats::tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html))
methods.

## Usage

``` r
geom_arima(
  mapping = NULL,
  data = NULL,
  stat = "arima",
  geom = c("text", "label"),
  position = "identity",
  ...,
  method = c("x13", "tramoseats"),
  spec = NULL,
  frequency = NULL,
  message = TRUE,
  x_arima = NULL,
  y_arima = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  If specified and `inherit.aes = TRUE` (the default), it is combined
  with the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  A `data.frame` that contains the data used for the seasonal
  adjustment.

- stat:

  The statistical transformation to use on the data for this layer, as a
  string.

- geom:

  Character. The geometric object to use to display the data: `GeomText`
  (`geom = "text"`, the default, see
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html))
  or `GeomLabel` (`geom = "label"`, see
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)).

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value,
  like `colour = "red"` or `size = 3`.

- method:

  The method used for the seasonal adjustment. `"x13"` (by default) for
  the X-13 method (using
  [`rjd3x13::jx13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html))
  and `"tramoseats"` for TRAMO-SEATS (using
  [`rjd3tramoseats::.jtramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)).
  Both methods are only compatible with series of frequency 12
  (monthly), 6 (bimonthly), 4 (quarterly), and 2 (semi-annual). For
  other frequencies, the following methods are available:
  `"x11-extended"` (see
  [`rjd3highfreq::x11plus()`](https://rjdverse.github.io/rjd3highfreq/reference/x11plus.html)),
  `"fractionalairline"` (see
  [`rjd3highfreq::fractional_airline_decomposition()`](https://rjdverse.github.io/rjd3highfreq/reference/fractional_airline_decomposition.html)),
  `"multiairline"` (see
  [`rjd3highfreq::multi_airline_decomposition()`](https://rjdverse.github.io/rjd3highfreq/reference/multi_airline_decomposition.html)),
  `"stl"` (see
  [`rjd3highfreq::stlplus()`](https://rjdverse.github.io/rjd3highfreq/reference/stlplus.html)),
  `"istl"` (see
  [`rjd3highfreq::istl()`](https://rjdverse.github.io/rjd3highfreq/reference/istl.html)),
  and `"mstl"` (see
  [`rjd3highfreq::mstl()`](https://rjdverse.github.io/rjd3highfreq/reference/mstl.html)).

- spec:

  the specification used for the seasonal adjustment. For `"x13"` and
  `"tramoseats"` methods, it can be the name of a pre-defined
  specification or a user-defined specification created with
  [`rjd3x13::spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.html)
  or
  [`rjd3tramoseats::spec_tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/deprecated-rjd3tramoseats.html).

- frequency:

  The frequency of the time series. By default (`frequency = NULL`), the
  frequency is computed automatically.

- message:

  A logical value indicating whether a message is printed with the
  frequency used.

- x_arima, y_arima:

  Position of the ARIMA model text. By default, the first position of
  the `data` is used.

- show.legend:

  Logical. Should this layer be included in the legends? `NA`, the
  default, includes it if any aesthetics are mapped. `FALSE` never
  includes it, and `TRUE` always includes it. It can also be a named
  logical vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

## Value

A ggplot2 layer object to be added to a plot.

## Details

With `geom = "text"`, the ARIMA model used in the pre-adjustment process
of seasonal adjustment is directly added to the plot. With
`geom = "label"`, a rectangle is drawn behind the ARIMA model text,
making it easier to read.

## Examples

``` r
p_sa_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(
        title = "Seasonal adjustment of the French industrial production index",
        x = "time", y = NULL
    ) +
    geom_sa(color = "red", message = FALSE)
         
# To add the ARIMA model
p_sa_ipi_fr + 
    geom_arima(
        geom = "label",
        x_arima = -Inf, y_arima = -Inf, 
        vjust = -1, hjust = -0.1,
        message = FALSE
    )       
```
