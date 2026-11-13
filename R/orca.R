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


