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
api_create <- function(...) defunct("api_create", LP_CHART_STUDIO)


#' @rdname chart-studio
#' @export
api_download_plot <- function(...) defunct("api_download_plot", LP_CHART_STUDIO)


#' @rdname chart-studio
#' @export
api_download_grid <- function(...) defunct("api_download_grid", LP_CHART_STUDIO)


#' @rdname chart-studio
#' @export
api <- function(...) defunct("api", LP_CHART_STUDIO)
