#' Export a plotly graph to a static file
#' 
#' This function is deprecated, use [save_image] instead.
#' 
#' @details For SVG plots, a screenshot is taken via `webshot::webshot()`.
#' Since `phantomjs` (and hence `webshot`) does not support WebGL,
#' the RSelenium package is used for exporting WebGL plots. 
#' 
#' @param p a plotly or ggplot object.
#' @param file a filename. The file type is inferred from the file extension.
#' Valid extensions include 'jpeg' | 'png' | 'webp' | 'svg' | 'pdf'
#' @param selenium used only when `p` is a WebGL plot or the output 
#' format is 'webp' or 'svg'. Should be an object of class "rsClientServer"
#' returned by `RSelenium::rsDriver`.
#' @param ... if `p` is non-WebGL and the output file format is 
#' jpeg/png/pdf arguments are passed along to `webshot::webshot()`.
#' Otherwise, they are ignored.
#' @export
#' @author Carson Sievert
#'
export <- function(p = last_plot(), file = "plotly.png", selenium = NULL, ...) {
  deprecated("export", "use save_image() (needs Kaleido) or htmlwidgets::saveWidget() instead.")
  if (identical(tolower(tools::file_ext(file)), "html")) {
    htmlwidgets::saveWidget(as_widget(p), file)
  } else {
    save_image(p, file)
  }
  invisible(file)
}
