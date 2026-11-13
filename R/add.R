#' Add a trace to a plot_ly() object
#'
#' Adds a layer whose attributes (values or formulas referencing the data)
#' are inherited from `plot_ly()` unless `inherit = FALSE`. The other
#' functions are shortcuts for common trace types. [add_trace()] and the
#' `add_<trace type>()` functions (e.g. `add_heatmap()`, `add_pie()`) are
#' generics from plotly.core; their methods for `plot_ly()` objects take
#' trace attributes in `...` (positional arguments map to the trace type's
#' main data attributes, e.g. `x` and `y` for histograms).
#'
#' @param fig,p A [plot_ly()] object.
#' @param ... Trace attributes.
#' @param data A data frame for this layer (defaults to the plot's data).
#' @param inherit Inherit attributes from [plot_ly()]?
#' @param x,y,z,text,xend,yend,ymin,ymax,r,theta,t Positional attributes
#'   (values or formulas).
#' @param fun A function taking and returning a plot, applied with its data
#'   kept separate (`add_fun()`).
#' @return The modified plot.
#' @name add_trace.plotly
#' @examples
#' mtcars |> plot_ly(x = ~wt, y = ~mpg) |>
#'   add_markers(color = ~factor(cyl)) |>
#'   add_lines(data = data.frame(wt = c(2, 5), mpg = c(30, 12)), color = I("black"))
NULL

#' @rdname add_trace.plotly
#' @export
add_trace.plotly <- function(fig, ..., data = NULL, inherit = TRUE) {
  attrs <- list(...)
  attrs$inherit <- inherit
  if (!is.null(attrs[["group"]])) warning("The group argument has been deprecated. Use split instead.", call. = FALSE)
  p <- add_data(fig, data)
  if (inherit) attrs <- modify_list(unclass(p$x$attrs[[1]]), attrs)
  p$x$attrs <- c(p$x$attrs %||l% list(), stats_named(list(attrs), p$x$cur_data))
  p
}

#' @rdname plotly_data
#' @export
add_data <- function(p, data = NULL) {
  if (is.null(data)) return(p)
  if (!is_legacy_plot(p)) stop("Don't know how to add data to an object of class: ", class(p)[[1]], call. = FALSE)
  id <- new_id()
  p$x$visdat[[id]] <- function() data
  p$x$cur_data <- id
  p
}


#' @rdname add_trace.plotly
#' @export
add_markers <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, z = z)
  check_required(v, c("x", "y"), "Must supply `x` and `y` attributes")
  add_trace.plotly(p, x = v$x, y = v$y, z = v$z, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                   mode = "markers", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_text <- function(p, x = NULL, y = NULL, z = NULL, text = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, z = z, text = text)
  check_required(v, c("x", "y", "text"), "Must supply `x`, `y` and `text` attributes")
  add_trace.plotly(p, x = v$x, y = v$y, z = v$z, text = v$text, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                   mode = "text", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_paths <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, z = z)
  check_required(v, c("x", "y"), "Must supply `x` and `y` attributes")
  add_classed(p, "plotly_path", x = v$x, y = v$y, z = v$z, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_lines <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, z = z)
  check_required(v, c("x", "y"), "Must supply `x` and `y` attributes")
  add_classed(p, "plotly_line", x = v$x, y = v$y, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_segments <- function(p, x = NULL, y = NULL, xend = NULL, yend = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, xend = xend, yend = yend)
  check_required(v, c("x", "y", "xend", "yend"), "Must supply `x`/`y`/`xend`/`yend` attributes")
  add_classed(p, "plotly_segment", x = v$x, y = v$y, xend = v$xend, yend = v$yend, type = "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_polygons <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y)
  check_required(v, c("x", "y"), "Must supply `x`/`y` attributes")
  add_classed(p, "plotly_polygon", x = v$x, y = v$y, type = "scatter", fill = "toself", mode = "lines",
                 ..., data = data, inherit = inherit)
}


#' Add a simple features (sf) layer
#'
#' Draws the geometries of an [sf](https://r-spatial.github.io/sf/) object:
#' polygons as filled outlines, lines as paths and points as markers (one
#' trace per geometry kind). Works with [plot_ly()] (cartesian, with an equal
#' aspect ratio), [plot_geo()] and [plot_mapbox()]; `plot_ly()` with sf data
#' and no layers adds one automatically. Requires the sf package.
#'
#' For maps, data are transformed to longitude/latitude (WGS84, EPSG:4326)
#' when they use another coordinate reference system (with a warning); data
#' with none are assumed to be EPSG:4326. The initial view fits the data.
#'
#' @param p A [plot_ly()] object.
#' @param ... Trace attributes (e.g. `color`, `split`, `text`, `hoverinfo`).
#' @param x,y Coordinates (default: the geometries' coordinates).
#' @param data An sf object (default: the plot's data).
#' @param inherit Inherit attributes from [plot_ly()]?
#' @return The plot.
#' @export
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc |> plot_ly()
#' nc |> plot_ly() |> add_sf(color = ~AREA, split = ~NAME, hoverinfo = "text", text = ~NAME)
#' nc |> plot_geo() |> add_sf(color = I("gray50"))
add_sf <- function(p, ..., x = ~x, y = ~y, data = NULL, inherit = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("add_sf() needs the sf package: install.packages(\"sf\")", call. = FALSE)
  }
  dat <- plotly_data(add_data(p, data))
  if (!inherits(dat, "sf")) {
    stop("The `data` for an `add_sf()` layer must be an sf object, not an object of class: ",
         class(dat)[1], call. = FALSE)
  }
  if (is_geo(p)) dat <- sf_crs(dat)
  bbox <- as.list(sf::st_bbox(dat))
  set <- attr(dat, "set")
  d <- sf_basic(dat)
  attrz <- if (inherit) modify_list(unclass(p$x$attrs[[1]]), list(...)) else list(...)
  for (i in seq_along(d)) {
    attrs <- modify_list(sf_default_attrs(d[[i]]), attrz)
    # scatter3d doesn't support fill
    if ("z" %in% names(attrs)) attrs$fill <- NULL
    args <- list(p = p, class = "plotly_sf", x = x, y = y, `_bbox` = bbox, set = set,
                 data = d[[i]], inherit = inherit)
    args <- modify_list(args, attrs)
    p <- do.call(add_classed, args)
  }
  p
}


#' @rdname add_trace.plotly
#' @export
add_ribbons <- function(p, x = NULL, ymin = NULL, ymax = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, ymin = ymin, ymax = ymax)
  check_required(v, c("x", "ymin", "ymax"), "Must supply `x`/`ymin`/`ymax` attributes")
  add_classed(p, c("plotly_ribbon", "plotly_polygon"), x = v$x, ymin = v$ymin, ymax = v$ymax, type = "scatter",
                 mode = "lines", hoveron = "points", fill = "toself", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_area <- function(p, r = NULL, theta = NULL, t = NULL, ..., data = NULL, inherit = TRUE) {
  if (!is.null(t)) message("Since `add_area()` now uses the barpolar trace, the `t` argument is deprecated. Use `theta` instead.")
  v <- inherit(p, inherit, r = r, theta = theta %||l% t)
  check_required(v, c("r", "theta"), "Must supply `r`/`theta` attributes")
  add_classed(p, "plotly_area", r = v$r, theta = v$theta, type = "barpolar", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_bars <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y)
  check_required(v, c("x", "y"), "Must supply `x`/`y` attributes")
  add_classed(p, "plotly_bar", x = v$x, y = v$y, type = "bar", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_boxplot <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y)
  if (is.null(v$x) && is.null(v$y)) stop("Must supply either `x` or `y` attribute", call. = FALSE)
  add_classed(p, "plotly_boxplot", x = v$x, y = v$y, type = "box", ..., data = data, inherit = inherit)
}


#' @rdname add_trace.plotly
#' @export
add_mesh <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- inherit(p, inherit, x = x, y = y, z = z)
  check_required(v, c("x", "y", "z"), "Must supply `x`/`y`/`z` attributes")
  add_classed(p, "plotly_mesh", x = v$x, y = v$y, z = v$z, type = "mesh3d", ..., data = data, inherit = inherit)
}


special_attrs <- function(trace) {
  out <- switch(class(trace)[[1]] %||l% "", plotly_segment = c("xend", "yend"), plotly_ribbon = c("ymin", "ymax"), NULL)
  if (isTRUE(trace[["type"]] %in% c("scattermap", "scattergeo"))) out <- c(out, "x", "y")
  out
}


# #' 
# #' 
# #' @export
# #' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
# #' 
# #' x <- rnorm(10)
# #' plot_ly(x = ~x) %>%
# #'   add_chull()
# add_chull <- function(p, ...) {
#   stop("not yet implemented")
#   ch <- chull(x, y = NULL)
#   # TODO: Should mode='markers+lines'? If so, retrace first points?
#   add_polygons(...)
# }


#' @rdname add_trace.plotly
#' @export
add_fun <- function(p, fun, ...) {
  old <- p$x$cur_data
  p <- fun(p, ...)
  p$x$cur_data <- old
  n <- length(p$x$attrs)
  p$x$attrs[n] <- stats_named(list(p$x$attrs[[n]]), old)
  p
}


#' @rdname add_trace.plotly
#' @export
add_annotations <- function(p, text = NULL, ..., data = NULL, inherit = TRUE) {
  if (inherits(p, "go_figure")) return(add_annotation(p, text = text, ...))
  p <- add_data(p, data)
  attrs <- list(text = text, ...)
  for (k in c("x", "y", "text")) attrs[k] <- list(attrs[[k]] %||l% p$x$attrs[[1]][[k]])
  if (is.null(attrs[["text"]])) stop("Must supply text to annotation", call. = FALSE)
  p$x$layoutAttrs <- c(p$x$layoutAttrs %||l% list(), stats_named(list(list(annotations = attrs)), p$x$cur_data %||l% ""))
  p
}
