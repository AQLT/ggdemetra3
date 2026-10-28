# Table of Diagnostics

Adds a table of diagnostics to the plot. Only available for X-13
([`rjd3x13::x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html))
and TRAMO-SEATS
([`rjd3tramoseats::tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html))
methods.

## Usage

``` r
geom_diagnostics(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  method = c("x13", "tramoseats"),
  spec = NULL,
  frequency = NULL,
  message = TRUE,
  diagnostics = NULL,
  digits = 2,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  table_theme = ttheme_default(),
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

- diagnostics:

  Character vector containing the names of the diagnostics to plot. See
  [`rjd3x13::userdefined_variables_x13()`](https://rjdverse.github.io/rjd3x13/reference/deprecated-rjd3x13.html)
  or
  [`rjd3tramoseats::userdefined_variables_tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/deprecated-rjd3tramoseats.html)
  for the available parameters.

- digits:

  Integer indicating the number of decimal places to be used for numeric
  diagnostics (default is `2`).

- xmin, xmax:

  Horizontal location of the raster in data coordinates.

- ymin, ymax:

  Vertical location of the raster in data coordinates.

- table_theme:

  List of theme parameters for the table of diagnostics (see
  [`gridExtra::ttheme_default()`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html)).

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

## Value

A ggplot2 layer object to be added to a plot.

## Examples

``` r
p_sa_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(
        title = "Seasonal adjustment of the French industrial production index",
        x = "time", y = NULL
    ) +
    geom_sa(color = "red", message = FALSE)

# To add a table of diagnostics with the result of the X-11 combined test 
# and the p-values of the residual seasonality QS and F-tests:
diagnostics <- c("diagnostics.seas-sa-combined", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-f")
p_sa_ipi_fr + 
    geom_diagnostics(
        diagnostics = diagnostics,
        ymin = 58, ymax = 72, xmin = 2010,
        table_theme = gridExtra::ttheme_default(base_size = 8),
        message = FALSE
    )


# To customize the names of the diagnostics in the plot:
diagnostics <- c(
    `Combined test` = "diagnostics.seas-sa-combined",
    `Residual qs-test (p-value)` = "diagnostics.seas-sa-qs",
    `Residual f-test (p-value)` = "diagnostics.seas-sa-f"
)
p_sa_ipi_fr + 
    geom_diagnostics(
        diagnostics = diagnostics,
        ymin = 58, ymax = 72, xmin = 2010,
        table_theme = gridExtra::ttheme_default(base_size = 8),
        message = FALSE
    )


# To add the table below the plot: 
p_diag <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_diagnostics(
        diagnostics = diagnostics,
        table_theme = gridExtra::ttheme_default(base_size = 8),
        message = FALSE
    ) + 
    theme_void()

gridExtra::grid.arrange(
    p_sa_ipi_fr, p_diag,
    nrow = 2, heights = c(4, 1)
)
```
