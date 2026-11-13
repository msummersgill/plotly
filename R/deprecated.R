defunct <- function(fn, instead) {
  stop("`", fn, "()` is no longer available: ", instead, call. = FALSE)
}

deprecated <- function(fn, instead) {
  warning("`", fn, "()` is deprecated: ", instead, call. = FALSE)
}

LP_CHART_STUDIO <- "Chart Studio's API has been retired. Use htmlwidgets::saveWidget() to share a plot as HTML, or save_image() for a static image."

#' @rdname chart-studio
#' @export
plotly_POST <- function(...) defunct("plotly_POST", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
get_figure <- function(...) defunct("get_figure", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
plotly <- function(...) defunct("plotly", "credentials are no longer used; see plot_ly() and ggplotly().")

#' @rdname plotly-deprecated
#' @export
offline <- function(p, ...) {
  .Deprecated("plot_ly")
  p
}


#' @rdname plotly-deprecated
#' @export
as.widget <- function(x, ...) {
  .Deprecated("as_widget")
  as_widget(x, ...)
}

