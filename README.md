
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/plotly.png" width="200" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/ropensci/plotly/workflows/R-CMD-check/badge.svg)](https://github.com/plotly/plotly.R/actions)
[![CRAN Status](https://www.r-pkg.org/badges/version/plotly)](https://cran.r-project.org/package=plotly)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/plotly)](https://cranlogs.r-pkg.org/badges/grand-total/plotly)
[![monthly](https://cranlogs.r-pkg.org/badges/plotly)](https://cranlogs.r-pkg.org/badges/plotly)
<!-- badges: end -->

An R package for creating interactive web graphics via the open source
JavaScript graphing library
[plotly.js](https://github.com/plotly/plotly.js).

<div align="center">
  <a href="https://dash.plotly.com/project-maintenance">
    <img src="https://dash.plotly.com/assets/images/maintained-by-community.png" width="400px" alt="Maintained by the Plotly Community">
  </a>
</div>

## Installation

Install from CRAN:

``` r
install.packages("plotly")
```

Or install the latest development version (on GitHub) via `{remotes}`:

``` r
remotes::install_github("plotly/plotly")
```

## Getting started

### Using plotly without ggplot2

The `plot_ly()` function provides a more direct interface to plotly.js
so you can leverage more specialized chart types (e.g., [parallel
coordinates](https://plotly.com/r/parallel-coordinates-plot/) or
[maps](https://plotly.com/r/maps/)) or even some visualization that the
ggplot2 API won’t ever support (e.g., surface,
[mesh](https://plotly.com/r/3d-mesh/),
[trisurf](https://plotly.com/r/trisurf/), etc).

``` r
plot_ly(z = ~volcano, type = "surface")
```

![<https://plotly.com/~brnvg/1134>](https://plotly.com/~brnvg/1134.png)

## Learn more

To learn more about special features that the plotly R package provides (e.g., [client-side linking](https://plotly-r.com/client-side-linking.html), [**shiny** integration](https://plotly-r.com/linking-views-with-shiny.html), [editing and generating static images](https://plotly-r.com/publish.html), [custom events in JavaScript](https://plotly-r.com/javascript.html), and more), see <https://plotly-r.com>. You may already be familiar with existing plotly documentation (e.g., <https://plotly.com/r/>), which is essentially a language-agnostic how-to guide for learning plotly.js, whereas <https://plotly-r.com> is meant to be more wholistic tutorial written by and for the R user. The package itself ships with a number of demos (list them by running `demo(package = "plotly")`) and shiny/rmarkdown examples (list them by running `plotly_example("shiny")` or `plotly_example("rmd")`). [Carson](https://cpsievert.me) also keeps numerous [slide decks](https://talks.cpsievert.me) with useful examples and concepts.

## Contributing

Please read through our [contributing
guidelines](https://github.com/plotly/plotly.R/blob/master/CONTRIBUTING.md).
Included are directions for opening issues, asking questions,
contributing changes to plotly, and our code of
conduct.
