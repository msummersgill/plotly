#' plotly: interactive web graphics with plot_ly() and ggplotly()
#'
#' plotly builds interactive plots with [plot_ly()] and [ggplotly()]. Plots
#' are rendered by the plotly.core package, which bundles plotly.js and
#' provides the widget, Shiny integration and static image export, along
#' with graph objects (`go_*()`) and Plotly Express (`px_*()`) interfaces.
#' A `plot_ly()` object can be turned into a plotly.core figure with
#' [legacy_figure()].
#'
#' @keywords internal
#' @rawNamespace import(plotly.core, except = c(schema))
#' @rawNamespace import(ggplot2, except = c(last_plot, set_last_plot))
#' @evalNamespace sprintf("S3method(add_%s,plotly)", plotly.core::available_trace_types())
"_PACKAGE"
