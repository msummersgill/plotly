#' Use a partial bundle of plotly.js
#'
#' Leveraging plotly.js' partial bundles can lead to smaller file sizes 
#' and faster rendering. The full list of available bundles, and the
#' trace types that they support, are available 
#' [here](https://github.com/plotly/plotly.js/blob/master/dist/README.md#partial-bundles) 
#' 
#' @details WARNING: use this function with caution when rendering multiple 
#' plotly graphs on a single website. That's because, if multiple plotly.js 
#' bundles are used, the most recent bundle will override the other bundles.
#' See the examples section for an example.
#'
#' @param p a plotly object.
#' @param type name of the (partial) bundle. The default, `'auto'`, attempts to  
#' find the smallest single bundle that can render `p`. If no single partial bundle
#' can render `p`, then the full bundle is used.
#' @param local whether or not to download the partial bundle so that it can be
#' viewed later without an internet connection.
#' @param minified whether or not to use a minified js file (non-minified file can be useful for debugging plotly.js)
#' @author Carson Sievert
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # ----------------------------------------------------------------------
#' # This function is always safe to use when rendering a single 
#' # plotly graph. In this case, we get a 3x file reduction.
#' # ----------------------------------------------------------------------
#'
#' \dontrun{
#' library(plotly)
#' p <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
#' save_widget <- function(p, f) {
#'   owd <- setwd(dirname(f))
#'   on.exit(setwd(owd))
#'   htmlwidgets::saveWidget(p, f)
#'   mb <- round(file.info(f)$size / 1e6, 3)
#'   message("File is: ", mb," MB")
#' }
#' f1 <- tempfile(fileext = ".html")
#' f2 <- tempfile(fileext = ".html")
#' save_widget(p, f1)
#' save_widget(partial_bundle(p), f2)
#' 
#' # ----------------------------------------------------------------------
#' # But, since plotly.js bundles override one another, 
#' # be careful when putting multiple graphs in a larger document!
#' # Note how the surface (part of the gl3d bundle) renders, but the 
#' # heatmap (part of the cartesian bundle) doesn't...
#' # ----------------------------------------------------------------------
#' 
#' library(htmltools)
#' p1 <- plot_ly(z = ~volcano) %>% 
#'   add_heatmap() %>%
#'   partial_bundle()
#' p2 <- plot_ly(z = ~volcano) %>% 
#'   add_surface() %>%
#'   partial_bundle()
#' browsable(tagList(p1, p2))
#' }
#'


partial_bundle <- function(p, type = "auto", local = TRUE, minified = TRUE) {
  deprecated("partial_bundle", "plotly.js is loaded once per page by plotly.core; the plot is returned unchanged.")
  p
}
