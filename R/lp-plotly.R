# The plot_ly() API (plot_ly(), add_*(), layout(), ...). plot_ly() objects
# are htmlwidgets that store attribute mappings lazily; they are built with
# plotly_build() (R/lp-build.R), converted to a validated plotly.core
# go_figure by legacy_figure() and rendered by plotly.core's widget.

#' Create a plot with the plot_ly() interface
#'
#' Compatibility with the legacy plotly R package: `plot_ly()` maps columns of
#' `data` to plotly.js attributes using formulas (e.g. `x = ~wt`), and
#' `add_trace()`/`add_markers()`/`add_lines()` and friends add traces. The
#' mappings are evaluated when the plot is built or printed. `color`,
#' `stroke`, `size`, `span`, `symbol` and `linetype` map data to visual
#' properties, splitting discrete values into separate traces; wrap a value
#' in [I()] to use it as is (e.g. `color = I("red")`).
#'
#' Plots are rendered without a template, as with the legacy package (use
#' `layout(p, template = "plotly")` for one). Use [as_widget()] or
#' [legacy_figure()] to convert to a widget or a [go_figure()].
#'
#' @param data A data frame (optional).
#' @param ... Attributes (see <https://plotly.com/r/reference/>), as values or
#'   formulas referencing `data`.
#' @param type The trace type (e.g. `"scatter"`, `"bar"`). Inferred from the
#'   data if missing.
#' @param name Trace name (legend entry).
#' @param color,stroke Values mapped to fill and stroke color.
#' @param colors,strokes Colors to use: a color vector, an RColorBrewer or
#'   viridis palette name, or a named vector for discrete values.
#' @param alpha,alpha_stroke Opacity of fill and stroke colors (0-1).
#' @param size,span Values mapped to fill size and stroke width.
#' @param sizes,spans Range of sizes/widths to map to.
#' @param symbol,linetype Values mapped to marker symbols and line types.
#' @param symbols,linetypes Symbols (plotly.js names or `pch` values) and line
#'   types (plotly.js names or `lty` values) to use.
#' @param split Values defining separate traces.
#' @param frame Values defining animation frames.
#' @param width,height Size of the plot in pixels.
#' @param source Identifier of the plot for [event_data()].
#' @return A plotly htmlwidget object.
#' @seealso [add_trace()], [layout()], [subplot()], [plotly_build()]
#' @export
#' @examples
#' mtcars |> plot_ly(x = ~wt, y = ~mpg, color = ~factor(cyl), size = ~hp) |>
#'   layout(title = "Motor Trend cars")
#' data.frame(t = 1:20, v = cumsum(rnorm(20))) |>
#'   plot_ly(x = ~t, y = ~v) |>
#'   add_lines()
plot_ly <- function(data = data.frame(), ..., type = NULL, name, color, colors = NULL, alpha = NULL,
                    stroke, strokes = NULL, alpha_stroke = 1, size, sizes = c(10, 100), span, spans = c(1, 20),
                    symbol, symbols = NULL, linetype, linetypes = NULL, split, frame,
                    width = NULL, height = NULL, source = "A") {
  if (!is.data.frame(data) && !lp_is_shared(data)) {
    stop("First argument, `data`, must be a data frame or crosstalk::SharedData object.", call. = FALSE)
  }
  attrs <- list(...)
  for (k in c("group", "inherit")) {
    if (!is.null(attrs[[k]])) {
      warning("The ", k, " argument has been deprecated.", call. = FALSE)
      attrs[[k]] <- NULL
    }
  }
  if (!missing(name)) attrs$name <- name
  if (!missing(color)) attrs$color <- color
  if (!missing(stroke)) attrs$stroke <- stroke
  if (!missing(size)) attrs$size <- size
  if (!missing(span)) attrs$span <- span
  if (!missing(symbol)) attrs$symbol <- symbol
  if (!missing(linetype)) attrs$linetype <- linetype
  if (!missing(split)) attrs$split <- split
  if (!missing(frame)) attrs$frame <- frame
  attrs$colors <- colors
  attrs$strokes <- strokes
  attrs$alpha <- alpha
  attrs$alpha_stroke <- alpha_stroke
  attrs$sizes <- sizes
  attrs$spans <- spans
  attrs$symbols <- symbols
  attrs$linetypes <- linetypes
  attrs$type <- type
  id <- lp_new_id()
  vis <- data
  x <- list(
    visdat = stats_named(list(function() vis), id),
    cur_data = id,
    attrs = stats_named(list(attrs), id),
    layout = list(width = width, height = height, margin = LP_MARGIN),
    source = source
  )
  legacy_widget_lazy(x)
}

# The legacy package's default plot margins (tighter than plotly.js's).
LP_MARGIN <- list(b = 40, l = 60, t = 25, r = 10)

legacy_widget_lazy <- function(x) {
  w <- htmlwidgets::createWidget(
    name = "plotly_core", x = x, width = x$layout$width, height = x$layout$height,
    package = "plotly.core",
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE, defaultWidth = "100%", defaultHeight = 400, padding = 0),
    preRenderHook = function(w) legacy_widget(w)
  )
  class(w) <- c("plotly_core", "plotly", "htmlwidget")
  w
}

#' @rdname plot_ly
#' @param offline Unused (kept for compatibility).
#' @export
plot_geo <- function(data = data.frame(), ..., offline = FALSE) {
  p <- plot_ly(data, ...)
  p$x$layout$mapType <- "geo"
  lp_geo2cartesian(p)
}

# lat/lon given to plot_geo()/plot_mapbox() also serve as x/y, so add_*()
# functions inherit them (as in the legacy package)
lp_geo2cartesian <- function(p) {
  p$x$attrs <- lapply(p$x$attrs, function(tr) {
    tr[["x"]] <- tr[["x"]] %||l% tr[["lat"]]
    tr[["y"]] <- tr[["y"]] %||l% tr[["lon"]]
    tr
  })
  p
}

#' @rdname plot_ly
#' @details `plot_mapbox()` creates tile maps; since plotly.js 3 removed the
#'   Mapbox-based traces, it uses the MapLibre-based `scattermap` and
#'   `choroplethmap` traces (no access token is needed).
#' @export
plot_mapbox <- function(data = data.frame(), ...) {
  p <- plot_ly(data, ...)
  p$x$layout$mapType <- "map"
  lp_geo2cartesian(p)
}

#' @rdname plot_ly
#' @export
plotly_empty <- function(...) {
  ax <- list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
  layout(plot_ly(...), xaxis = ax, yaxis = ax)
}

#' @rdname plotly_data
#' @export
plotly_data <- function(p, id = p$x$cur_data) {
  if (!is_legacy_plot(p)) stop("`plotly_data()` expects a plot_ly() object as its first argument.", call. = FALSE)
  f <- p$x$visdat[[id]]
  if (is.null(f)) return(NULL)
  lp_shared_data(f())
}

#' @rdname plotly_data
#' @export
add_data <- function(p, data = NULL) {
  if (is.null(data)) return(p)
  if (!is_legacy_plot(p)) stop("Don't know how to add data to an object of class: ", class(p)[[1]], call. = FALSE)
  id <- lp_new_id()
  p$x$visdat[[id]] <- function() data
  p$x$cur_data <- id
  p
}

# Traces --------------------------------------------------------------------------------

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
  if (inherit) attrs <- lp_modify_list(unclass(p$x$attrs[[1]]), attrs)
  p$x$attrs <- c(p$x$attrs %||l% list(), stats_named(list(attrs), p$x$cur_data))
  p
}

lp_add_classed <- function(p, class, ...) {
  p <- add_trace.plotly(p, ...)
  n <- length(p$x$attrs)
  p$x$attrs[[n]] <- lp_prefix_class(p$x$attrs[[n]], class)
  p
}

lp_inherit <- function(p, inherit, ...) {
  vals <- list(...)
  if (inherit) for (k in names(vals)) vals[k] <- list(vals[[k]] %||l% p$x$attrs[[1]][[k]])
  vals
}

lp_require <- function(vals, needed, msg) {
  if (any(vapply(vals[needed], is.null, logical(1)))) stop(msg, call. = FALSE)
}

#' @rdname add_trace.plotly
#' @export
add_markers <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, z = z)
  lp_require(v, c("x", "y"), "Must supply `x` and `y` attributes")
  add_trace.plotly(p, x = v$x, y = v$y, z = v$z, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                   mode = "markers", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_text <- function(p, x = NULL, y = NULL, z = NULL, text = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, z = z, text = text)
  lp_require(v, c("x", "y", "text"), "Must supply `x`, `y` and `text` attributes")
  add_trace.plotly(p, x = v$x, y = v$y, z = v$z, text = v$text, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                   mode = "text", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_paths <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, z = z)
  lp_require(v, c("x", "y"), "Must supply `x` and `y` attributes")
  lp_add_classed(p, "plotly_path", x = v$x, y = v$y, z = v$z, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_lines <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, z = z)
  lp_require(v, c("x", "y"), "Must supply `x` and `y` attributes")
  lp_add_classed(p, "plotly_line", x = v$x, y = v$y, type = if (!is.null(v$z)) "scatter3d" else "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_segments <- function(p, x = NULL, y = NULL, xend = NULL, yend = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, xend = xend, yend = yend)
  lp_require(v, c("x", "y", "xend", "yend"), "Must supply `x`/`y`/`xend`/`yend` attributes")
  lp_add_classed(p, "plotly_segment", x = v$x, y = v$y, xend = v$xend, yend = v$yend, type = "scatter",
                 mode = "lines", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_polygons <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y)
  lp_require(v, c("x", "y"), "Must supply `x`/`y` attributes")
  lp_add_classed(p, "plotly_polygon", x = v$x, y = v$y, type = "scatter", fill = "toself", mode = "lines",
                 ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_ribbons <- function(p, x = NULL, ymin = NULL, ymax = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, ymin = ymin, ymax = ymax)
  lp_require(v, c("x", "ymin", "ymax"), "Must supply `x`/`ymin`/`ymax` attributes")
  lp_add_classed(p, c("plotly_ribbon", "plotly_polygon"), x = v$x, ymin = v$ymin, ymax = v$ymax, type = "scatter",
                 mode = "lines", hoveron = "points", fill = "toself", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_area <- function(p, r = NULL, theta = NULL, t = NULL, ..., data = NULL, inherit = TRUE) {
  if (!is.null(t)) message("Since `add_area()` now uses the barpolar trace, the `t` argument is deprecated. Use `theta` instead.")
  v <- lp_inherit(p, inherit, r = r, theta = theta %||l% t)
  lp_require(v, c("r", "theta"), "Must supply `r`/`theta` attributes")
  lp_add_classed(p, "plotly_area", r = v$r, theta = v$theta, type = "barpolar", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_bars <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y)
  lp_require(v, c("x", "y"), "Must supply `x`/`y` attributes")
  lp_add_classed(p, "plotly_bar", x = v$x, y = v$y, type = "bar", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_boxplot <- function(p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y)
  if (is.null(v$x) && is.null(v$y)) stop("Must supply either `x` or `y` attribute", call. = FALSE)
  lp_add_classed(p, "plotly_boxplot", x = v$x, y = v$y, type = "box", ..., data = data, inherit = inherit)
}

#' @rdname add_trace.plotly
#' @export
add_mesh <- function(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE) {
  v <- lp_inherit(p, inherit, x = x, y = y, z = z)
  lp_require(v, c("x", "y", "z"), "Must supply `x`/`y`/`z` attributes")
  lp_add_classed(p, "plotly_mesh", x = v$x, y = v$y, z = v$z, type = "mesh3d", ..., data = data, inherit = inherit)
}

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

# Positional arguments of the add_<type>() methods for the trace types that
# had legacy add_<type>() functions (add_histogram(), add_heatmap(), ...).
LP_POSITIONAL <- list(histogram = c("x", "y"), histogram2d = c("x", "y", "z"), histogram2dcontour = c("x", "y", "z"),
                      heatmap = c("x", "y", "z"), contour = "z", surface = "z", pie = c("values", "labels"),
                      image = c("z", "colormodel"))

# Methods for plotly.core's add_<type>() generics (add_bar(), add_heatmap(),
# ...): unnamed arguments are positional, as in the legacy add_<type>()
# functions, and some types check their data attributes.
lp_add_method <- function(type) {
  force(type)
  function(fig, ...) legacy_add_dispatch(type, fig, list(...))
}

legacy_add_dispatch <- function(type, p, dots) {
  if (length(dots)) {
    nms <- names(dots) %||l% rep("", length(dots))
    unnamed <- which(!nzchar(nms))
    pos <- setdiff(LP_POSITIONAL[[type]] %||l% character(), nms)
    if (length(unnamed) > length(pos)) stop("Too many unnamed arguments", call. = FALSE)
    nms[unnamed] <- pos[seq_along(unnamed)]
    names(dots) <- nms
  }
  args <- dots[!vapply(dots, is.null, logical(1))]
  data <- args$data
  inherit <- args$inherit %||l% TRUE
  args$data <- NULL
  args$inherit <- NULL
  v <- function(k) args[[k]] %||l% if (inherit) p$x$attrs[[1]][[k]]
  switch(type,
    histogram = if (is.null(v("x")) && is.null(v("y"))) stop("Must supply `x` and/or `y` attributes", call. = FALSE),
    histogram2d = , histogram2dcontour = if (is.null(v("z")) && (is.null(v("x")) || is.null(v("y")))) {
      stop("Must supply both `x` and `y` attributes if `z` is NULL", call. = FALSE)
    },
    heatmap = , contour = , surface = if (is.null(v("z"))) stop("Must supply `z` attribute", call. = FALSE),
    pie = if (is.null(v("values"))) stop("Must supply `values`", call. = FALSE),
    NULL
  )
  if (type == "image" && inherits(args$z, "raster")) {
    cols <- grDevices::col2rgb(args$z, alpha = TRUE)
    dims <- c(dim(args$z), 4)
    z <- array(numeric(prod(dims)), dims)
    for (k in 1:4) z[, , k] <- matrix(cols[k, ], nrow = dims[1], ncol = dims[2], byrow = TRUE)
    args$z <- z
    args$colormodel <- "rgba"
  }
  if (type == "table") {
    dat <- plotly_data(add_data(p, data))
    if (is.data.frame(dat)) {
      rn <- !isFALSE(args$rownames)
      hv <- lapply(names(dat), function(x) list(x))
      if (rn) hv <- c(list(""), hv)
      cv <- unname(as.list(dat))
      if (rn) cv <- c(list(row.names(dat)), cv)
      args$header <- lp_modify_list(list(values = hv, align = "right"), args$header)
      args$cells <- lp_modify_list(list(values = cv, align = "right"), args$cells)
    }
    args$rownames <- NULL
  }
  do.call(lp_add_classed, c(list(p = p, class = paste0("plotly_", type), type = type), args,
                            list(data = data, inherit = inherit)))
}

# Layout -------------------------------------------------------------------------------------

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
layout.go_figure <- function(p, ..., data = NULL) update_layout(p, ...)

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

#' @export
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  for (i in which(vapply(p, is_legacy_plot, logical(1)))) p[[i]] <- layout.plotly(p[[i]], ...)
  p
}

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
    for (k in seq_along(paths)) tr <- lp_set_path_value(tr, paths[[k]], vals[[k]])
    tr
  })
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

lp_to_ms <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  if (inherits(x, "POSIXt")) return(as.numeric(x) * 1000)
  x
}

#' Show or hide guides
#'
#' `hide_legend()`, `hide_colorbar()` and `hide_guides()` hide the legend,
#' colorbars or both; `colorbar()` modifies the colorbar of a [plot_ly()]
#' object (with `limits` to clip the color range).
#'
#' @param p A [plot_ly()] object or [go_figure()].
#' @param ... Colorbar attributes.
#' @param limits Numeric range of the color mapping.
#' @param which Which colorbar to modify.
#' @return The modified plot.
#' @export
hide_legend <- function(p) {
  if (inherits(p, "go_figure")) return(update_layout(p, showlegend = FALSE))
  if (inherits(p, c("gg", "ggmatrix"))) p <- plotly_build(p)
  p$x$.hideLegend <- TRUE
  p
}

#' @rdname hide_legend
#' @export
hide_colorbar <- function(p) {
  if (inherits(p, c("gg", "ggmatrix"))) p <- plotly_build(p)
  if (inherits(p, "go_figure")) {
    p <- for_each_trace(p, function(tr) {
      if (lp_has_attr(tr$type, "showscale")) tr$showscale <- FALSE
      if (lp_has_attr(tr$type, "marker.showscale")) tr$marker$showscale <- FALSE
      tr
    })
    return(update_layout(p, coloraxis_showscale = FALSE))
  }
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    type <- p$x$data[[i]]$type
    if (lp_has_attr(type, "showscale")) p$x$data[[i]]$showscale <- lp_default(FALSE)
    if (lp_has_attr(type, "marker.showscale")) p$x$data[[i]]$marker$showscale <- lp_default(FALSE)
  }
  p
}

#' @rdname hide_legend
#' @export
hide_guides <- function(p) hide_legend(hide_colorbar(p))

#' @rdname hide_legend
#' @export
colorbar <- function(p, ..., limits = NULL, which = 1) {
  p <- plotly_build(p)
  is_bar <- vapply(p$x$data, lp_is_colorbar, logical(1))
  if (!any(is_bar)) {
    warning("Didn't find a colorbar to modify.", call. = FALSE)
    return(p)
  }
  for (i in which(is_bar)[which]) {
    tr <- p$x$data[[i]]
    z_color <- inherits(tr, "zcolor")
    if (!is.null(limits)) {
      limits <- sort(limits)
      if (z_color) {
        z <- p$x$data[[i]]$z
        z[z < limits[1] | limits[2] < z] <- NA
        p$x$data[[i]]$z <- z
        p$x$data[[i]]$zmin <- limits[1]
        p$x$data[[i]]$zmax <- limits[2]
      } else {
        p$x$data <- lapply(p$x$data, function(x) {
          # marker and line colors (e.g. scatter3d lines+markers), as plotly 4.x
          for (k in c("marker", "line")) {
            col <- x[[k]][["color"]]
            if (is.numeric(col) && length(col) > 1) {
              x[[k]]$color[col < limits[1] | limits[2] < col] <- NA
              x[[k]]$cmin <- limits[1]
              x[[k]]$cmax <- limits[2]
            }
          }
          x
        })
      }
    }
    if (z_color) p$x$data[[i]]$colorbar <- lp_modify_list(tr$colorbar, list(...))
    else p$x$data[[i]]$marker$colorbar <- lp_modify_list(tr$marker$colorbar, list(...))
  }
  p
}

#' Convert traces to WebGL versions
#'
#' Uses WebGL-based trace types (e.g. `scattergl`) where available, for
#' faster rendering of large data.
#'
#' @param p A [plot_ly()] object.
#' @return The modified plot.
#' @export
toWebGL <- function(p) {
  if (inherits(p, c("gg", "ggmatrix"))) p <- plotly_build(p)
  p$x$.plotlyWebGl <- TRUE
  p
}

# Animation -------------------------------------------------------------------------------------

LP_EASINGS <- c("linear", "quad", "cubic", "sin", "exp", "circle", "elastic", "back", "bounce", "linear-in", "quad-in",
                "cubic-in", "sin-in", "exp-in", "circle-in", "elastic-in", "back-in", "bounce-in", "linear-out",
                "quad-out", "cubic-out", "sin-out", "exp-out", "circle-out", "elastic-out", "back-out", "bounce-out",
                "linear-in-out", "quad-in-out", "cubic-in-out", "sin-in-out", "exp-in-out", "circle-in-out",
                "elastic-in-out", "back-in-out", "bounce-in-out")

#' Animation options
#'
#' Controls animations of [plot_ly()] objects with a `frame` mapping:
#' `animation_opts()` sets timing, `animation_slider()` and
#' `animation_button()` modify (or hide) the slider and play button.
#'
#' @param p A [plot_ly()] object.
#' @param frame Milliseconds per frame (including the transition).
#' @param transition Milliseconds of the transition between frames.
#' @param easing Transition easing (e.g. `"linear"`, `"cubic-in-out"`).
#' @param redraw Redraw the plot at the end of each transition?
#' @param mode `"immediate"`, `"next"` or `"afterall"`.
#' @param hide Remove the slider?
#' @param label Button label.
#' @param ... Slider or button attributes.
#' @return The modified plot.
#' @export
#' @examples
#' df <- data.frame(x = rep(1:5, 3), y = runif(15), t = rep(1:3, each = 5))
#' df |> plot_ly(x = ~x, y = ~y, frame = ~t) |> animation_opts(1000, easing = "elastic")
animation_opts <- function(p, frame = 500, transition = frame, easing = "linear", redraw = TRUE, mode = "immediate") {
  p$animation <- lp_animation_format(frame, transition, easing, redraw, mode)
  p
}

lp_animation_format <- function(frame, transition, easing, redraw, mode) {
  if (frame < 0) stop("frame must be non-negative.", call. = FALSE)
  if (transition < 0) stop("transition must be non-negative.", call. = FALSE)
  if (frame < transition) stop("frame must be a value larger than transition (it includes the transition)", call. = FALSE)
  match.arg(as.character(easing), LP_EASINGS)
  match.arg(as.character(mode), c("immediate", "next", "afterall"))
  list(transition = list(duration = transition, easing = easing), frame = list(duration = frame, redraw = redraw), mode = mode)
}

lp_animation_defaults <- function() {
  lp_animation_format(lp_default(500), lp_default(500), lp_default("linear"), lp_default(TRUE), lp_default("immediate"))
}

#' @rdname animation_opts
#' @export
animation_slider <- function(p, hide = FALSE, ...) {
  p <- plotly_build(p)
  is_slider <- vapply(p$x$layout$sliders, function(s) inherits(s, "aniSlider"), logical(1))
  if (hide) {
    p$x$layout$sliders[is_slider] <- NULL
    return(p)
  }
  i <- which(is_slider)
  p$x$layout$sliders[[i]] <- structure(lp_modify_list(unclass(p$x$layout$sliders[[i]]), list(...)), class = "aniSlider")
  p
}

#' @rdname animation_opts
#' @export
animation_button <- function(p, ..., label) {
  p <- plotly_build(p)
  i <- which(vapply(p$x$layout$updatemenus, function(s) inherits(s, "aniButton"), logical(1)))
  if (!missing(label)) p$x$layout$updatemenus[[i]]$buttons[[1]]$label <- label
  p$x$layout$updatemenus[[i]] <- structure(lp_modify_list(unclass(p$x$layout$updatemenus[[i]]), list(...)), class = "aniButton")
  p
}

lp_animation_button_supply <- function(p) {
  menus <- p$x$layout$updatemenus
  is_btn <- vapply(menus, function(s) inherits(s, "aniButton"), logical(1))
  idx <- if (sum(is_btn) == 1) which(is_btn) else length(menus) + 1
  opts <- p$animation %||l% lp_animation_defaults()
  p$x$layout$updatemenus[[idx]] <- structure(list(
    type = "buttons", direction = "right", showactive = FALSE, y = 0, x = 0, yanchor = "top", xanchor = "right",
    pad = list(t = 60, r = 5),
    buttons = list(list(label = "Play", method = "animate",
                        args = list(NULL, lp_modify_list(list(fromcurrent = TRUE, mode = "immediate"), opts))))
  ), class = "aniButton")
  p
}

lp_animation_slider_supply <- function(p, ...) {
  sliders <- p$x$layout$sliders
  is_slider <- vapply(sliders, function(s) inherits(s, "aniSlider"), logical(1))
  idx <- if (sum(is_slider) == 1) which(is_slider) else length(sliders) + 1
  opts <- p$animation %||l% lp_animation_defaults()
  steps <- lapply(p$x$frames, function(f) list(method = "animate", args = list(list(f$name), opts), label = f$name, value = f$name))
  slider <- lp_modify_list(if (any(is_slider)) unclass(sliders[[which(is_slider)]]) else list(), compact(list(...)))
  slider$steps <- steps
  slider$visible <- slider$visible %||l% TRUE
  slider$pad$t <- slider$pad[["t"]] %||l% 40
  p$x$layout$sliders[[idx]] <- structure(slider, class = "aniSlider")
  p
}

# Conversion to go_figure and rendering --------------------------------------------------------

# Remove the build step's internal classes (defaults, colorway markers, trace
# classes) so values can be validated.
lp_strip <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- unclass(x)
    attr(x, "titles") <- NULL
    x[] <- lapply(x, lp_strip)
    return(x)
  }
  if (!is.null(oldClass(x))) {
    keep <- setdiff(oldClass(x), c("plotly_default", "colorway", "plotly_eval", "aniSlider", "aniButton",
                                   "numeric", "integer", "character", "logical", "double", "list"))
    oldClass(x) <- if (length(keep)) keep else NULL
  }
  x
}

#' Convert a plot_ly() object to a figure
#'
#' Builds a [plot_ly()] object and returns the equivalent validated
#' [go_figure()], so it can be modified with [update_layout()],
#' [update_traces()] and the other figure functions. Attributes that are not
#' valid for plotly.js are dropped with a warning, as the legacy plotly
#' package warns about them.
#'
#' @param p A [plot_ly()] object.
#' @return A [go_figure()] (with the plot's config, event and key settings).
#' @export
#' @examples
#' fig <- mtcars |> plot_ly(x = ~wt, y = ~mpg) |> legacy_figure()
#' fig <- plotly.core::update_traces(fig, marker_size = 10)
legacy_figure <- function(p) {
  b <- plotly_build(p)
  x <- lp_extract_keys(lp_strip(b$x))
  # ggplot2 themes give font families of "" (the default font), which
  # plotly.js ignored
  x[c("data", "layout", "frames")] <- lp_drop_blank_family(x[c("data", "layout", "frames")])
  x[c("data", "frames")] <- lp_matrix_colorscales(x[c("data", "frames")])
  x$layout$mapType <- NULL
  if (is.null(x$layout$template)) x$layout$template <- get_template("none")
  fig <- tryCatch(go_figure(x$data, x$layout, x$frames %||l% NULL), error = function(e) {
    warning(conditionMessage(e), "\n(invalid attributes are ignored)", call. = FALSE)
    go_figure(x$data, x$layout, x$frames %||l% NULL, skip_invalid = TRUE)
  })
  for (k in c("config", "source", "shinyEvents", "mathjax", "keys", "sets", "frameKeys", "highlight", "selectize", "fitBounds")) {
    if (!is.null(x[[k]])) attr(fig, paste0("plotly_", k)) <- x[[k]]
  }
  fig
}

# colorscales given as (value, color) matrices, as lists of pairs
lp_matrix_colorscales <- function(x) {
  if (!is.list(x)) return(x)
  cs <- x[["colorscale"]]
  if (is.matrix(cs) && ncol(cs) == 2) {
    x[["colorscale"]] <- Map(function(v, c) list(v, c), as.numeric(cs[, 1]), as.character(cs[, 2]))
  }
  x[] <- lapply(x, lp_matrix_colorscales)
  x
}

lp_drop_blank_family <- function(x) {
  if (!is.list(x)) return(x)
  if (identical(x[["family"]], "")) x[["family"]] <- NULL
  x[] <- lapply(x, lp_drop_blank_family)
  x
}

legacy_widget <- function(p, ...) {
  w <- plotly_widget(legacy_figure(p), ...)
  w$width <- p$width %||l% w$width
  w$height <- p$height %||l% w$height
  w
}

#' @export
print.plotly <- function(x, ...) {
  print(legacy_widget(x), ...)
  invisible(x)
}

#' @export
as_widget.plotly <- function(x, ...) legacy_widget(x, ...)

#' @export
as_widget.gg <- function(x, ...) as_widget(ggplotly(x), ...)

#' @export
as_widget.ggmatrix <- function(x, ...) as_widget(ggplotly(x), ...)
