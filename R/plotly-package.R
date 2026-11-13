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

# add_<type>() methods for plot_ly() objects, for every trace type
# (plotly.core's add_bar(), add_heatmap(), ... generics).
for (.type in plotly.core::available_trace_types()) {
  assign(paste0("add_", .type, ".plotly"), lp_add_method(.type))
}
rm(.type)

# Functions of plotly 4.x that plotly.core now provides --------------------------------

#' @export
plotly.core::add_trace
#' @export
plotly.core::add_choropleth
#' @export
plotly.core::add_contour
#' @export
plotly.core::add_heatmap
#' @export
plotly.core::add_histogram
#' @export
plotly.core::add_histogram2d
#' @export
plotly.core::add_histogram2dcontour
#' @export
plotly.core::add_image
#' @export
plotly.core::add_pie
#' @export
plotly.core::add_scattergeo
#' @export
plotly.core::add_surface
#' @export
plotly.core::add_table
#' @export
plotly.core::as_widget
#' @export
plotly.core::config
#' @export
plotly.core::event_data
#' @export
plotly.core::event_register
#' @export
plotly.core::event_unregister
#' @export
plotly.core::kaleido
#' @export
plotly.core::last_plot
#' @export
plotly.core::plotlyOutput
#' @export
plotly.core::plotlyProxy
#' @export
plotly.core::plotlyProxyInvoke
#' @export
plotly.core::plotly_json
#' @export
plotly.core::renderPlotly
#' @export
plotly.core::save_image
#' @export
plotly.core::TeX
