#' Modify the layout of a plot
#'
#' For [plot_ly()] objects, layout attributes may be formulas referencing the
#' data (as in the legacy plotly package). For a [go_figure()], this is
#' [update_layout()]. For a matrix, [graphics::layout()] is called.
#'
#' @param p A [plot_ly()] object, [go_figure()] or matrix.
#' @param ... Layout attributes.
#' @param data A data frame for formulas in `...`.
#' @return The modified plot.
#' @export
#' @examples
#' mtcars |> plot_ly(x = ~wt, y = ~mpg) |> layout(title = "Cars", xaxis = list(title = "Weight"))
layout <- function(p, ..., data = NULL) UseMethod("layout")

#' @export
layout.matrix <- function(p, ..., data = NULL) graphics::layout(p, ...)

#' @export
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  for (i in which(vapply(p, is_legacy_plot, logical(1)))) p[[i]] <- layout.plotly(p[[i]], ...)
  p
}

#' @export
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
  if (!is.null(attrs[["height"]]) || !is.null(attrs[["width"]])) {
    warning("Specifying width/height in layout() is now deprecated.\nPlease specify in plot_ly()", call. = FALSE)
  }
  p$x$layoutAttrs <- c(p$x$layoutAttrs %||l% list(), stats_named(list(attrs), p$x$cur_data %||l% ""))
  p
}

#' Add a range slider to the x axis
#'
#' @param p A [plot_ly()] object or [go_figure()].
#' @param start,end Initial range (numbers, Dates or date-times).
#' @param ... Range slider attributes.
#' @return The modified plot.
#' @export
rangeslider <- function(p, start = NULL, end = NULL, ...) {
  rng <- if (!is.null(start) || !is.null(end)) list(lp_to_ms(start), lp_to_ms(end))
  if (inherits(p, "go_figure")) {
    patch <- c(list(rangeslider = c(list(visible = TRUE), list(...))), if (!is.null(rng)) list(range = rng))
    return(update_xaxes(p, patch))
  }
  if (sum(grepl("^xaxis", names(p$x$layout))) > 1) stop("Can only add a rangeslider to a plot with one x-axis", call. = FALSE)
  p$x$layout$xaxis$range <- rng
  p$x$layout$xaxis$rangeslider <- list(visible = TRUE, ...)
  p
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


