# Outliers Texts

Function to directly add the outliers used in the pre-adjustment process
of the seasonal adjustment to the plot. Only available for X-13
([`rjd3x13::x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html))
and TRAMO-SEATS
([`rjd3tramoseats::tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html))
methods.

## Usage

``` r
geom_outlier(
  mapping = NULL,
  data = NULL,
  stat = "outlier",
  geom = c("text", "label", "text_repel", "label_repel"),
  position = "identity",
  ...,
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

  Character. The geometry to use to display the data: `GeomText`
  (`geom = "text"`, see
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html));
  `GeomLabel` (`geom = "label"`, see
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html));
  `GeomTextRepel` (`geom = "text_repel"`, the default, see
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html));
  `GeomLabelRepel` (`geom = "label_repel"`, see
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)).

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  They may be parameters of
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  (if `geom = "text"`),
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  (if `geom = "label"`),
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  (if `geom = "text_repel"`) or
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  (if `geom = "label_repel"`).

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

- first_date:

  A numeric specifying the first date from which the outliers are
  plotted. By default (`first_date = NULL`), the outliers are plotted
  from the beginning of the time series.

- last_date:

  A numeric specifying the last date up to which the outliers are
  plotted. By default (`last_date = NULL`), the outliers are plotted
  until the end of the time series.

- coefficients:

  Logical indicating whether the estimated coefficients are printed. By
  default `coefficients = FALSE`.

- digits:

  Integer indicating the number of decimal places to be used for the
  estimated coefficients of the outliers. By default `digits = 1`.

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

With `geom = "text"`, the outliers used in the pre-adjustment process of
the seasonal adjustment are directly added to the plot. With
`geom = "label"`, a rectangle is drawn behind the names of the outliers,
making them easier to read. The same applies to `geom = "text_repel"` or
`geom = "label_repel"`, but text labels are also repelled away from each
other and away from the data points (see
[`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)).

## Examples

``` r
p_sa_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(
        title = "Seasonal adjustment of the French industrial production index",
        x = "time", y = NULL
    ) +
    geom_sa(color = "red", message = FALSE)
         
# To add the outliers:
p_sa_ipi_fr + geom_outlier(
    geom = "label",
    message = FALSE
)


# To have a more readable plot with outlier names repelled away from each other 
# and from the data points:
p_sa_ipi_fr + 
    geom_outlier(
        geom = "label_repel",
        message = FALSE,
        vjust = 4,
        ylim = c(NA, 65), force = 10,
        arrow = arrow(
            length = unit(0.03, "npc"),
            type = "closed", ends = "last"
        )
    )


# To only plot the outliers from a specific date (2009):
p_sa_ipi_fr + 
    geom_outlier(
        geom = "label_repel",
        message = FALSE,
        first_date = 2009,
        vjust = 4,
        ylim = c(NA, 65), force = 10,
        arrow = arrow(
            length = unit(0.03, "npc"),
            type = "closed", ends = "last"
        )
    )
```
