#' Initiate a plotly visualization
#'
#' This function maps R objects to [plotly.js](https://plotly.com/javascript/),
#' an (MIT licensed) web-based interactive charting library. It provides 
#' abstractions for doing common things (e.g. mapping data values to 
#' fill colors (via `color`) or creating [animation]s (via `frame`)) and sets
#' some different defaults to make the interface feel more 'R-like' 
#' (i.e., closer to [plot()] and [ggplot2::qplot()]). 
#'
#' @details Unless `type` is specified, this function just initiates a plotly 
#' object with 'global' attributes that are passed onto downstream uses of
#' [add_trace()] (or similar). A [formula] must always be used when 
#' referencing column name(s) in `data` (e.g. `plot_ly(mtcars, x = ~wt)`).
#' Formulas are optional when supplying values directly, but they do
#' help inform default axis/scale titles
#' (e.g., `plot_ly(x = mtcars$wt)` vs `plot_ly(x = ~mtcars$wt)`)
#'
#' @param data A data frame (optional) or [crosstalk::SharedData] object.
#' @param ... Arguments (i.e., attributes) passed along to the trace `type`.
#' See [schema()] for a list of acceptable attributes for a given trace `type`
#' (by going to `traces` -> `type` -> `attributes`). Note that attributes
#' provided at this level may override other arguments 
#' (e.g. `plot_ly(x = 1:10, y = 1:10, color = I("red"), marker = list(color = "blue"))`).
#' @param type A character string specifying the trace type (e.g. `"scatter"`, `"bar"`, `"box"`, etc).
#' If specified, it *always* creates a trace. Otherwise, a trace type is
#' inferred from the supplied arguments when the plot is built.
#' @param name Values mapped to the trace's name attribute. Since a trace can 
#' only have one name, this argument acts very much like `split` in that it 
#' creates one trace for every unique value.
#' @param color Values mapped to relevant 'fill-color' attribute(s) 
#' (e.g. [fillcolor](https://plotly.com/r/reference/#scatter-fillcolor), 
#' [marker.color](https://plotly.com/r/reference/#scatter-marker-color), 
#' [textfont.color](https://plotly.com/r/reference/#scatter-textfont-color), etc.).
#' The mapping from data values to color codes may be controlled using
#' `colors` and `alpha`, or avoided altogether via [I()] (e.g., `color = I("red")`). 
#' Any color understood by [grDevices::col2rgb()] may be used in this way. 
#' @param colors Either a colorbrewer2.org palette name (e.g. "YlOrRd" or "Blues"), 
#' or a vector of colors to interpolate in hexadecimal "#RRGGBB" format, 
#' or a color interpolation function like `colorRamp()`.
#' @param stroke Similar to `color`, but values are mapped to relevant 'stroke-color' attribute(s)
#' (e.g., [marker.line.color](https://plotly.com/r/reference/#scatter-marker-line-color)
#'  and [line.color](https://plotly.com/r/reference/#scatter-line-color)
#' for filled polygons). If not specified, `stroke` inherits from `color`.
#' @param strokes Similar to `colors`, but controls the `stroke` mapping.
#' @param alpha A number between 0 and 1 specifying the alpha channel applied to `color`.
#' Defaults to 0.5 when mapping to [fillcolor](https://plotly.com/r/reference/#scatter-fillcolor) and 1 otherwise.
#' @param alpha_stroke Similar to `alpha`, but applied to `stroke`.
#' @param symbol (Discrete) values mapped to [marker.symbol](https://plotly.com/r/reference/#scatter-marker-symbol).
#' The mapping from data values to symbols may be controlled using
#' `symbols`, or avoided altogether via [I()] (e.g., `symbol = I("pentagon")`). 
#' Any [pch] value or [symbol name](https://plotly.com/r/reference/#scatter-marker-symbol) may be used in this way.
#' @param symbols A character vector of [pch] values or [symbol names](https://plotly.com/r/reference/#scatter-marker-symbol).
#' @param linetype (Discrete) values mapped to [line.dash](https://plotly.com/r/reference/#scatter-line-dash).
#' The mapping from data values to symbols may be controlled using
#' `linetypes`, or avoided altogether via [I()] (e.g., `linetype = I("dash")`). 
#' Any `lty` (see [par]) value or [dash name](https://plotly.com/r/reference/#scatter-line-dash) may be used in this way.
#' @param linetypes A character vector of `lty` values or [dash names](https://plotly.com/r/reference/#scatter-line-dash)
#' @param size (Numeric) values mapped to relevant 'fill-size' attribute(s) 
#' (e.g., [marker.size](https://plotly.com/r/reference/#scatter-marker-size), 
#' [textfont.size](https://plotly.com/r/reference/#scatter-textfont-size),
#' and [error_x.width](https://plotly.com/r/reference/#scatter-error_x-width)).
#' The mapping from data values to symbols may be controlled using
#' `sizes`, or avoided altogether via [I()] (e.g., `size = I(30)`). 
#' @param sizes A numeric vector of length 2 used to scale `size` to pixels.
#' @param span (Numeric) values mapped to relevant 'stroke-size' attribute(s) 
#' (e.g., 
#' [marker.line.width](https://plotly.com/r/reference/#scatter-marker-line-width),
#' [line.width](https://plotly.com/r/reference/#scatter-line-width) for filled polygons,
#' and [error_x.thickness](https://plotly.com/r/reference/#scatter-error_x-thickness))
#' The mapping from data values to symbols may be controlled using
#' `spans`, or avoided altogether via [I()] (e.g., `span = I(30)`). 
#' @param spans A numeric vector of length 2 used to scale `span` to pixels.
#' @param split (Discrete) values used to create multiple traces (one trace per value).
#' @param frame (Discrete) values used to create animation frames.
#' @param width	Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param source a character string of length 1. Match the value of this string 
#' with the source argument in [event_data()] to retrieve the 
#' event data corresponding to a specific plot (shiny apps can have multiple plots).
#' @author Carson Sievert
#' @references <https://plotly-r.com/overview.html>
#' @seealso \itemize{
#'  \item For initializing a plotly-geo object: [plot_geo()]
#'  \item For initializing a plotly-mapbox object: [plot_mapbox()]
#'  \item For translating a ggplot2 object to a plotly object: [ggplotly()]
#'  \item For modifying any plotly object: [layout()], [add_trace()], [style()]
#'  \item For linked brushing: [highlight()]
#'  \item For arranging multiple plots: [subplot()], [crosstalk::bscols()]
#'  \item For inspecting plotly objects: [plotly_json()]
#'  \item For quick, accurate, and searchable plotly.js reference: [schema()]
#' }
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' \dontrun{
#' 
#' # plot_ly() tries to create a sensible plot based on the information you 
#' # give it. If you don't provide a trace type, plot_ly() will infer one.
#' plot_ly(economics, x = ~pop)
#' plot_ly(economics, x = ~date, y = ~pop)
#' # plot_ly() doesn't require data frame(s), which allows one to take 
#' # advantage of trace type(s) designed specifically for numeric matrices
#' plot_ly(z = ~volcano)
#' plot_ly(z = ~volcano, type = "surface")
#' 
#' # plotly has a functional interface: every plotly function takes a plotly
#' # object as it's first input argument and returns a modified plotly object
#' add_lines(plot_ly(economics, x = ~date, y = ~unemploy/pop))
#' 
#' # To make code more readable, plotly imports the pipe operator from magrittr
#' economics %>% plot_ly(x = ~date, y = ~unemploy/pop) %>% add_lines()
#' 
#' # Attributes defined via plot_ly() set 'global' attributes that 
#' # are carried onto subsequent traces, but those may be over-written
#' plot_ly(economics, x = ~date, color = I("black")) %>%
#'  add_lines(y = ~uempmed) %>%
#'  add_lines(y = ~psavert, color = I("red"))
#' 
#' # Attributes are documented in the figure reference -> https://plotly.com/r/reference
#' # You might notice plot_ly() has named arguments that aren't in this figure
#' # reference. These arguments make it easier to map abstract data values to
#' # visual attributes.
#' p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~body_mass_g)
#' add_markers(p, color = ~bill_depth_mm, size = ~bill_depth_mm)
#' add_markers(p, color = ~species)
#' add_markers(p, color = ~species, colors = "Set1")
#' add_markers(p, symbol = ~species)
#' add_paths(p, linetype = ~species)
#' 
#' }
#' 
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


#' Initiate a plotly-mapbox object
#' 
#' Use this function instead of [plot_ly()] to initialize
#' a plotly-mapbox object. This enforces the entire plot so use
#' the scattermapbox trace type, and enables higher level geometries
#' like [add_polygons()] to work
#' 
#' @param data A data frame (optional).
#' @param ... arguments passed along to [plot_ly()]. They should be
#' valid scattermapbox attributes - \url{https://plotly.com/r/reference/#scattermapbox}.
#' Note that x/y can also be used in place of lat/lon.
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_geo()], [ggplotly()] 
#' 
#' @examples \dontrun{
#' 
#' plot_mapbox(res_mn)
#' plot_mapbox(res_mn, color = ~INDRESNAME)
#' 
#' map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_mapbox(x = ~long, y = ~lat) %>%
#'   add_polygons() %>%
#'   layout(
#'     mapbox = list(
#'       center = list(lat = ~median(lat), lon = ~median(long))
#'     )
#'   )
#' }
#' 
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

#' Initiate a plotly-geo object
#' 
#' Use this function instead of [plot_ly()] to initialize
#' a plotly-geo object. This enforces the entire plot so use
#' the scattergeo trace type, and enables higher level geometries
#' like [add_polygons()] to work
#' 
#' @param data A data frame (optional).
#' @param ... arguments passed along to [plot_ly()].
#' @param offline whether or not to include geo assets so that the map
#' can be viewed with or without an internet connection. The plotlyGeoAssets
#' package is required for this functionality.
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_mapbox()], [ggplotly()] 
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' map_data("world", "canada") %>%
#'   group_by(group) %>%
#'   plot_geo(x = ~long, y = ~lat) %>%
#'   add_markers(size = I(1))
#' 
plot_geo <- function(data = data.frame(), ..., offline = FALSE) {
  p <- plot_ly(data, ...)
  
  if (isTRUE(offline)) {
    if (!is_installed("plotlyGeoAssets")) {
      stop(
        "The plotlyGeoAssets package is required to make 'offline' maps. ",
        "Please install and try again.",
        call. = FALSE
      )
    }
    p$dependencies <- c(
      list(plotlyGeoAssets::geo_assets()),
      p$dependencies
    )
  }
  # not only do we use this for is_geo(), but also setting the layout attr
  # https://plotly.com/r/reference/#layout-geo
  p$x$layout$mapType <- "geo"
  geo2cartesian(p)
}

# rename attrs (unevaluated arguments) from geo locations (lat/lon) to cartesian
geo2cartesian <- function(p) {
  p$x$attrs <- lapply(p$x$attrs, function(tr) {
    tr[["x"]] <- tr[["x"]] %||% tr[["lat"]]
    tr[["y"]] <- tr[["y"]] %||% tr[["lon"]]
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

#' Remove TypedArray polyfill
#'
#' By default, plotly.js' TypedArray polyfill is included as a dependency, so
#' printing "just works" in any context. Many users won't need this polyfill,
#' so this function may be used to remove it and thus reduce the size of the page. 
#' 
#' @details The polyfill seems to be only relevant for those rendering plots 
#' via phantomjs and RStudio on some Windows platforms.
#'
#' @param p a plotly object
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' p1 <- plot_ly()
#' p2 <- remove_typedarray_polyfill(p1)
#' t1 <- tempfile(fileext = ".html")
#' htmlwidgets::saveWidget(p1, t1)
#' file.info(t1)$size
#' htmlwidgets::saveWidget(p2, t1)
#' file.info(t1)$size
#' }

remove_typedarray_polyfill <- function(p) {
  deprecated("remove_typedarray_polyfill", "there is no polyfill to remove; the plot is returned unchanged.")
  p
}
