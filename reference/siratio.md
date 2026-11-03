# SI-Ratio Calculation and Plotting

Computes and visualizes the Seasonal-Irregular (SI) ratio from a
seasonal adjustment model (X-13 or TRAMO-SEATS).

## Usage

``` r
siratio(x, ...)

siratioplot(
  x,
  labels = NULL,
  add = FALSE,
  box = TRUE,
  col.s = "darkblue",
  col.i = "gray",
  col.mean = "red",
  cex.i = 0.1,
  lwd.s = par("lwd"),
  lwd.mean = lwd.s,
  main = "SI ratio",
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  ...
)

ggsiratioplot(
  x,
  labels = NULL,
  col.s = "darkblue",
  col.i = "gray",
  col.mean = "red",
  cex.i = 0.5,
  lwd.s = 1,
  lwd.mean = lwd.s,
  main = "SI ratio",
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- x:

  An input model (X-13 or TRAMO-SEATS) or data object.

- ...:

  Additional arguments passed to methods or low-level plotting
  functions.

- labels:

  Character vector of labels for the periods (e.g., months or quarters).

- add:

  Logical indicating whether the plot should be added to an existing
  one.

- box:

  Logical indicating whether a box should be drawn around the plot.

- col.s, col.i, col.mean:

  Colors for the seasonal component, the SI points, and the mean lines.

- cex.i, lwd.s, lwd.mean:

  Graphical parameters for point size and line widths.

- main, xlab, ylab:

  Character strings for the main title, X-axis label, and Y-axis label.

- xlim, ylim:

  Numeric vectors of length 2 specifying the X and Y axis limits.

## Value

- For `siratio()`: A `ts` object containing the SI ratio and the
  seasonal component.

- For `siratioplot()`: Called for its side effects (base 'R' plot).

- For `ggsiratioplot()`: A ggplot2 object representing the SI ratio
  chart.

## Examples

``` r
x <- rjd3x13::x13(ipi_c_eu[, "FR"])
siratioplot(x)

ggsiratioplot(x)
```
