# Seasonal Adjustment Time Series

Performs a seasonal adjustment and plots a specific component of the
decomposition. `geom_sa()` and `stat_sa()` are aliases: they both use
the same arguments. Use `stat_sa()` if you want to display the results
with a non-standard geom.

## Usage

``` r
geom_sa(
  mapping = NULL,
  data = NULL,
  stat = "sa",
  position = "identity",
  ...,
  method = c("x13", "tramoseats", "x11-extended", "fractionalairline", "multiairline",
    "stl", "istl", "mstl"),
  spec = NULL,
  frequency = NULL,
  message = TRUE,
  component = "sa",
  show.legend = NA,
  inherit.aes = TRUE
)

stat_sa(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  ...,
  method = c("x13", "tramoseats", "x11-extended", "fractionalairline",
    "fractionalairlineestimation", "multiairline", "stl", "istl", "mstl"),
  spec = NULL,
  frequency = NULL,
  message = TRUE,
  component = "sa",
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

  The specification used for the seasonal adjustment. For `"x13"` and
  `"tramoseats"` methods, it can be the name of a pre-defined
  specification or a user-defined specification created with
  [`rjd3x13::spec_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.html)
  or
  [`rjd3tramoseats::spec_tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/deprecated-rjd3tramoseats.html).
  For other methods, it can be a list of parameters used in the
  associated seasonal adjustment function.

- frequency:

  The frequency of the time series. By default (`frequency = NULL`), the
  frequency is computed automatically.

- message:

  A logical value indicating whether a message is printed with the
  frequency used.

- component:

  A character string specifying the component to plot. The result must
  be a time series. See
  [`rjd3x13::userdefined_variables_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.html)
  and
  [`rjd3tramoseats::userdefined_variables_tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/deprecated-rjd3tramoseats.html)
  for the available parameters for `"x13"` and `"tramoseats"`. For other
  methods, the available components are the columns of the
  `decomposition` element of the result of the seasonal adjustment
  function (see documentation of the associated function for more
  details). By default (`component = "sa"`), the seasonally adjusted
  component is plotted.

- show.legend:

  Logical. Should this layer be included in the legends? `NA`, the
  default, includes it if any aesthetics are mapped. `FALSE` never
  includes it, and `TRUE` always includes it. It can also be a named
  logical vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

- geom:

  The geometric object to use to display the data.

## Value

A ggplot2 layer object to be added to a plot.

## Examples

``` r
p_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(
        title = "Seasonal adjustment of the French industrial production index",
        x = "time", y = NULL
    )

# To add the seasonally adjusted series:
p_ipi_fr +
    geom_sa(color = "red")


# To add the forecasts of the input data and the seasonally adjusted series:
p_sa <- p_ipi_fr +
    geom_sa(component = "y_f", linetype = 2, message = FALSE) + 
    geom_sa(component = "sa", color = "red", message = FALSE) +
    geom_sa(component = "sa_f", color = "red", linetype = 2, message = FALSE)
p_sa 
```
