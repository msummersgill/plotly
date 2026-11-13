# Deprecated and defunct functions of plotly 4.x. Chart Studio (api_*(),
# signup(), plotly_POST(), ...) and orca were retired; partial bundles and the
# typed array polyfill are no longer needed with plotly.js 3+.

lp_defunct <- function(fn, instead) {
  stop("`", fn, "()` is no longer available: ", instead, call. = FALSE)
}

lp_deprecated <- function(fn, instead) {
  warning("`", fn, "()` is deprecated: ", instead, call. = FALSE)
}

LP_CHART_STUDIO <- "Chart Studio's API has been retired. Use htmlwidgets::saveWidget() to share a plot as HTML, or save_image() for a static image."

#' Defunct Chart Studio functions
#'
#' Chart Studio's API has been retired, so these functions now raise an
#' error. Use [htmlwidgets::saveWidget()] to save a plot as a standalone HTML
#' file, or [save_image()] for a static image.
#'
#' @param ... Ignored.
#' @return None; these functions raise an error.
#' @name chart-studio
#' @keywords internal
NULL

#' @rdname chart-studio
#' @export
api_create <- function(...) lp_defunct("api_create", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
api_download_plot <- function(...) lp_defunct("api_download_plot", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
api_download_grid <- function(...) lp_defunct("api_download_grid", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
api <- function(...) lp_defunct("api", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
signup <- function(...) lp_defunct("signup", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
plotly_POST <- function(...) lp_defunct("plotly_POST", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
get_figure <- function(...) lp_defunct("get_figure", LP_CHART_STUDIO)

#' @rdname chart-studio
#' @export
plotly_IMAGE <- function(...) lp_defunct("plotly_IMAGE", "use save_image() instead.")

#' @rdname chart-studio
#' @export
plotly <- function(...) lp_defunct("plotly", "credentials are no longer used; see plot_ly() and ggplotly().")

#' Deprecated functions
#'
#' These functions are kept for compatibility with plotly 4.x and warn when
#' used:
#' * `orca()` and `export()` save static images with [save_image()] (Kaleido)
#'   instead of orca. `orca_serve()` is defunct.
#' * `partial_bundle()` returns the plot unchanged: plotly.js is bundled
#'   with plotly.core, which loads it once per page.
#' * `remove_typedarray_polyfill()` returns the plot unchanged: there is no
#'   polyfill any more.
#' * `offline()`, `as.widget()` return the plot (offline plots have long been
#'   the default; plots are already htmlwidgets).
#'
#' @param p,x A plot.
#' @param file Output file.
#' @param format Image format (by default, from `file`'s extension).
#' @param scale,width,height Image scale and size, passed to [save_image()].
#' @param ... Ignored, or (for `orca()`) passed to [save_image()].
#' @return `orca()` and `export()` return `file` invisibly; the others the
#'   plot.
#' @name plotly-deprecated
#' @keywords internal
NULL

#' @rdname plotly-deprecated
#' @export
orca <- function(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL, ...) {
  lp_deprecated("orca", "use save_image() (needs Kaleido) instead.")
  if (!identical(tolower(format), tolower(tools::file_ext(file)))) file <- paste0(tools::file_path_sans_ext(file), ".", format)
  save_image(p, file, width = width, height = height, scale = scale)
}

#' @rdname plotly-deprecated
#' @export
orca_serve <- function(...) lp_defunct("orca_serve", "orca is no longer supported; use save_image() instead.")

#' @rdname plotly-deprecated
#' @export
export <- function(p = last_plot(), file = "plotly.png", ...) {
  lp_deprecated("export", "use save_image() (needs Kaleido) or htmlwidgets::saveWidget() instead.")
  if (identical(tolower(tools::file_ext(file)), "html")) {
    htmlwidgets::saveWidget(as_widget(p), file)
  } else {
    save_image(p, file)
  }
  invisible(file)
}

#' @rdname plotly-deprecated
#' @export
partial_bundle <- function(p, ...) {
  lp_deprecated("partial_bundle", "plotly.js is loaded once per page by plotly.core; the plot is returned unchanged.")
  p
}

#' @rdname plotly-deprecated
#' @export
remove_typedarray_polyfill <- function(p) {
  lp_deprecated("remove_typedarray_polyfill", "there is no polyfill to remove; the plot is returned unchanged.")
  p
}

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

# config() for plot_ly() objects, with plotly 4.x's handling of options
# plotly.js no longer has: `collaborate` and `cloud` warn as before, options
# of removed features (Mapbox, Chart Studio links, global transforms) are
# ignored, and other unknown options are dropped with a warning when the
# plot is built (lp_verify_config()), as plotly.js ignored them.
LP_REMOVED_CONFIG <- c("mapboxAccessToken", "showLink", "linkText", "sendData", "showSources",
                       "showEditInChartStudio", "globalTransforms", "collaborate")

#' @export
config.plotly <- function(p, ..., cloud = FALSE, showSendToCloud = cloud) {
  args <- list(...)
  if ("collaborate" %in% names(args)) warning("The collaborate button is no longer supported", call. = FALSE)
  if (isTRUE(cloud)) warning("The `cloud` argument is deprecated. Use `showSendToCloud` instead.", call. = FALSE)
  args <- args[!names(args) %in% LP_REMOVED_CONFIG]
  if (identical(args$mathjax, "local")) {
    warning("`mathjax = \"local\"` is no longer supported; MathJax is loaded from a CDN", call. = FALSE)
    args$mathjax <- "cdn"
  }
  if (!missing(cloud) || !missing(showSendToCloud)) args$showSendToCloud <- isTRUE(showSendToCloud)
  core_args <- c("locale", "mathjax", names(schema(jsonedit = FALSE)$config))
  unknown <- args[!names(args) %in% core_args]
  args <- args[names(args) %in% core_args]
  p <- do.call(utils::getS3method("config", "default", envir = asNamespace("plotly.core")), c(list(p), args))
  if (length(unknown)) p$x$config <- utils::modifyList(p$x$config %||l% list(), unknown)
  p
}

lp_verify_config <- function(p) {
  bad <- setdiff(names(p$x$config), names(schema(jsonedit = FALSE)$config))
  for (b in bad) {
    warning("'", b, "' is not a valid config attribute and will be ignored (see plotly.core::schema()$config)", call. = FALSE)
    p$x$config[[b]] <- NULL
  }
  p
}
