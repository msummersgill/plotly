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
  if (!is.data.frame(data) && !is_shared(data)) {
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
  id <- new_id()
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
#' @details `plot_mapbox()` creates tile maps; since plotly.js 3 removed the
#'   Mapbox-based traces, it uses the MapLibre-based `scattermap` and
#'   `choroplethmap` traces (no access token is needed).
#' @export
plot_mapbox <- function(data = data.frame(), ...) {
  p <- plot_ly(data, ...)
  p$x$layout$mapType <- "map"
  geo2cartesian(p)
}

add_classed <- function(p, class, ...) {
  p <- add_trace.plotly(p, ...)
  n <- length(p$x$attrs)
  p$x$attrs[[n]] <- prefix_class(p$x$attrs[[n]], class)
  p
}

inherit <- function(p, inherit, ...) {
  vals <- list(...)
  if (inherit) for (k in names(vals)) vals[k] <- list(vals[[k]] %||l% p$x$attrs[[1]][[k]])
  vals
}

check_required <- function(vals, needed, msg) {
  if (any(vapply(vals[needed], is.null, logical(1)))) stop(msg, call. = FALSE)
}

# Positional arguments of the add_<type>() methods for the trace types that
# had legacy add_<type>() functions (add_histogram(), add_heatmap(), ...).
LP_POSITIONAL <- list(histogram = c("x", "y"), histogram2d = c("x", "y", "z"), histogram2dcontour = c("x", "y", "z"),
                      heatmap = c("x", "y", "z"), contour = "z", surface = "z", pie = c("values", "labels"),
                      image = c("z", "colormodel"))

# Methods for plotly.core's add_<type>() generics (add_bar(), add_heatmap(),
# ...): unnamed arguments are positional, as in the legacy add_<type>()
# functions, and some types check their data attributes.
add_method <- function(type) {
  force(type)
  function(fig, ...) legacy_add_dispatch(type, fig, list(...))
}

# add_<type>() methods for plot_ly() objects, for every trace type
# (plotly.core's add_bar(), add_heatmap(), ... generics).
for (.type in plotly.core::available_trace_types()) {
  assign(paste0("add_", .type, ".plotly"), add_method(.type))
}
rm(.type)

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
      args$header <- modify_list(list(values = hv, align = "right"), args$header)
      args$cells <- modify_list(list(values = cv, align = "right"), args$cells)
    }
    args$rownames <- NULL
  }
  do.call(add_classed, c(list(p = p, class = paste0("plotly_", type), type = type), args,
                            list(data = data, inherit = inherit)))
}

#' @export
layout.go_figure <- function(p, ..., data = NULL) update_layout(p, ...)

to_milliseconds <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  if (inherits(x, "POSIXt")) return(as.numeric(x) * 1000)
  x
}

LP_EASINGS <- c("linear", "quad", "cubic", "sin", "exp", "circle", "elastic", "back", "bounce", "linear-in", "quad-in",
                "cubic-in", "sin-in", "exp-in", "circle-in", "elastic-in", "back-in", "bounce-in", "linear-out",
                "quad-out", "cubic-out", "sin-out", "exp-out", "circle-out", "elastic-out", "back-out", "bounce-out",
                "linear-in-out", "quad-in-out", "cubic-in-out", "sin-in-out", "exp-in-out", "circle-in-out",
                "elastic-in-out", "back-in-out", "bounce-in-out")

animation_format <- function(frame, transition, easing, redraw, mode) {
  if (frame < 0) stop("frame must be non-negative.", call. = FALSE)
  if (transition < 0) stop("transition must be non-negative.", call. = FALSE)
  if (frame < transition) stop("frame must be a value larger than transition (it includes the transition)", call. = FALSE)
  match.arg(as.character(easing), LP_EASINGS)
  match.arg(as.character(mode), c("immediate", "next", "afterall"))
  list(transition = list(duration = transition, easing = easing), frame = list(duration = frame, redraw = redraw), mode = mode)
}

animation_defaults <- function() {
  animation_format(default(500), default(500), default("linear"), default(TRUE), default("immediate"))
}

# Remove the build step's internal classes (defaults, colorway markers, trace
# classes) so values can be validated.
strip <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- unclass(x)
    attr(x, "titles") <- NULL
    x[] <- lapply(x, strip)
    return(x)
  }
  if (!is.null(oldClass(x))) {
    keep <- setdiff(oldClass(x), c("plotly_default", "colorway", "plotly_eval", "aniSlider", "aniButton",
                                   "numeric", "integer", "character", "logical", "double", "list",
                                   # ggplot2's vctrs classes for scaled aesthetics (e.g. bar widths)
                                   "mapped_discrete", "mapped_continuous", "vctrs_vctr"))
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
  x <- extract_keys(strip(b$x))
  # ggplotly()'s internal marker for axis-title annotations (see subplot())
  x$layout$annotations <- lapply(x$layout$annotations, function(a) { a$annotationType <- NULL; a })
  # ggplot2 themes give font families of "" (the default font), which
  # plotly.js ignored
  x[c("data", "layout", "frames")] <- drop_blank_family(x[c("data", "layout", "frames")])
  x[c("data", "frames")] <- matrix_colorscales(x[c("data", "frames")])
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
matrix_colorscales <- function(x) {
  if (!is.list(x)) return(x)
  cs <- x[["colorscale"]]
  if (is.matrix(cs) && ncol(cs) == 2) {
    x[["colorscale"]] <- Map(function(v, c) list(v, c), as.numeric(cs[, 1]), as.character(cs[, 2]))
  }
  x[] <- lapply(x, matrix_colorscales)
  x
}

drop_blank_family <- function(x) {
  if (!is.list(x)) return(x)
  if (identical(x[["family"]], "")) x[["family"]] <- NULL
  x[] <- lapply(x, drop_blank_family)
  x
}

legacy_widget <- function(p, ...) {
  w <- plotly_widget(legacy_figure(p), ...)
  w$width <- p$width %||l% w$width
  w$height <- p$height %||l% w$height
  w
}

#' @export
as_widget.plotly <- function(x, ...) legacy_widget(x, ...)

#' @export
as_widget.gg <- function(x, ...) as_widget(ggplotly(x), ...)

#' @export
as_widget.ggmatrix <- function(x, ...) as_widget(ggplotly(x), ...)

#' @rdname plot_ly
#' @param offline Unused (kept for compatibility).
#' @export
plot_geo <- function(data = data.frame(), ..., offline = FALSE) {
  p <- plot_ly(data, ...)
  p$x$layout$mapType <- "geo"
  geo2cartesian(p)
}

# lat/lon given to plot_geo()/plot_mapbox() also serve as x/y, so add_*()
# functions inherit them (as in the legacy package)
geo2cartesian <- function(p) {
  p$x$attrs <- lapply(p$x$attrs, function(tr) {
    tr[["x"]] <- tr[["x"]] %||l% tr[["lat"]]
    tr[["y"]] <- tr[["y"]] %||l% tr[["lon"]]
    tr
  })
  p
}


#' Plot an interactive dendrogram
#' 
#' This function takes advantage of nested key selections to implement an 
#' interactive dendrogram. Selecting a node selects all the labels (i.e. leafs)
#' under that node.
#' 
#' @param d a dendrogram object
#' @param set defines a crosstalk group
#' @param xmin minimum of the range of the x-scale
#' @param width width
#' @param height height
#' @param ... arguments supplied to [subplot()]
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_mapbox()], [ggplotly()] 
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' hc <- hclust(dist(USArrests), "ave")
#' dend1 <- as.dendrogram(hc)
#' plot_dendro(dend1, height = 600) %>% 
#'   hide_legend() %>% 
#'   highlight(persistent = TRUE, dynamic = TRUE)
#' }
#' 

plot_dendro <- function(d, set = "A", xmin = -50, height = 500, width = 500, ...) {
  # get x/y locations of every node in the tree
  allXY <- get_xy(d)
  # get non-zero heights so we can split on them and find the relevant labels
  non0 <- allXY[["y"]][allXY[["y"]] > 0]
  # splitting on the minimum height would generate all terminal nodes anyway
  split <- non0[min(non0) < non0]
  # label is a list-column since non-zero heights have multiple labels
  # for now, we just have access to terminal node labels
  labs <- labels(d)
  allXY$label <- vector("list", nrow(allXY))
  allXY$label[[1]] <- labs
  allXY$label[allXY$y == 0] <- labs
  
  # collect all the *unique* non-trivial nodes
  nodes <- list()
  for (i in split) {
    dsub <- cut(d, i)$lower
    for (j in seq_along(dsub)) {
      s <- dsub[[j]]
      if (is.leaf(s)) next
      if (any(vapply(nodes, function(x) identical(x, s), logical(1)))) next
      nodes[[length(nodes) + 1]] <- s
    }
  }
  
  heights <- sapply(nodes, function(x) attr(x, "height"))
  labs <- lapply(nodes, labels)
  
  # NOTE: this won't support nodes that have the same height 
  # but that isn't possible, right?
  for (i in seq_along(heights)) {
    allXY$label[[which(allXY$y == heights[i])]] <- labs[[i]]
  }
  
  tidy_segments <- dendextend::as.ggdend(d)$segments
  
  allTXT <- allXY[allXY$y == 0, ]
  
  blank_axis <- list(
    title = "",
    showticklabels = FALSE,
    zeroline = FALSE
  )
  
  allXY$members <- sapply(allXY$label, length)
  allTXT$label <- as.character(allTXT$label)
  
  allXY %>% 
    plot_ly(x = ~y, y = ~x, color = I("black"), hoverinfo = "none",
            height = height, width = width) %>%
    add_segments(
      data = tidy_segments, xend = ~yend, yend = ~xend, showlegend = FALSE
    ) %>%
    add_markers(
      data = allXY[allXY$y > 0, ], key = ~label, set = set, name = "nodes", 
      text = ~paste0("members: ", members), hoverinfo = "text"
    ) %>%
    add_text(
      data = allTXT, x = 0, y = ~x, text = ~label, key = ~label, set = set,
      textposition = "middle left", name = "labels"
    ) %>%
    layout(
      dragmode = "select", 
      xaxis = c(blank_axis, list(range = c(xmin, extendrange(allXY[["y"]])[2]))),
      yaxis = c(blank_axis, list(range = extendrange(allXY[["x"]])))
    )
}

get_xy <- function(node) {
  m <- dendextend::get_nodes_xy(node)
  colnames(m) <- c("x", "y")
  tibble::as_tibble(m)
}


#' @rdname plotly-deprecated
#' @export
remove_typedarray_polyfill <- function(p) {
  deprecated("remove_typedarray_polyfill", "there is no polyfill to remove; the plot is returned unchanged.")
  p
}
