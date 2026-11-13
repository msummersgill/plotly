#' Modify traces of a built plot
#'
#' Sets attributes on the traces of a [plot_ly()] object after it is built
#' (or of a [go_figure()]). Names may use dots for nested attributes, e.g.
#' `marker.color`.
#'
#' @param p A [plot_ly()] object or [go_figure()].
#' @param ... Attribute values.
#' @param traces Trace indices (1-based); all by default.
#' @return The modified plot.
#' @export
#' @examples
#' mtcars |> plot_ly(x = ~wt, y = ~mpg, color = ~factor(cyl)) |> style(marker.size = 12, traces = 2)
style <- function(p, ..., traces = NULL) {
  if (inherits(p, c("gg", "ggmatrix"))) p <- plotly_build(p)
  vals <- list(...)
  if (inherits(p, "go_figure")) {
    names(vals) <- gsub(".", "_", names(vals), fixed = TRUE)
    return(do.call(update_traces, c(list(p, selector = traces), vals)))
  }
  p <- plotly_build(p)
  n <- length(p$x$data)
  idx <- traces %||l% seq_len(n)
  if (any(idx > n)) warning("You've referenced non-existent traces", call. = FALSE)
  paths <- strsplit(names(vals), ".", fixed = TRUE)
  p$x$data[idx] <- lapply(p$x$data[idx], function(tr) {
    for (k in seq_along(paths)) tr <- set_path_value(tr, paths[[k]], vals[[k]])
    tr
  })
  p
}

