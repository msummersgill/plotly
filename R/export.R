#' @rdname plotly-deprecated
#' @export
export <- function(p = last_plot(), file = "plotly.png", ...) {
  deprecated("export", "use save_image() (needs Kaleido) or htmlwidgets::saveWidget() instead.")
  if (identical(tolower(tools::file_ext(file)), "html")) {
    htmlwidgets::saveWidget(as_widget(p), file)
  } else {
    save_image(p, file)
  }
  invisible(file)
}
