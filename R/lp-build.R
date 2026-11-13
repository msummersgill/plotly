# Building legacy plot_ly() objects (a port of the legacy plotly package's
# plotly_build()). Evaluates the lazily stored attributes against the data,
# splits and maps traces (color, size, symbol, linetype, ...), registers
# animation frames, and fills p$x$data, p$x$layout and p$x$frames. The built
# lists are converted to a validated go_figure by legacy_figure().

#' Build a figure
#'
#' For [plot_ly()] objects, evaluates the attribute mappings against the data
#' and returns the object with the resulting traces in `p$x$data`, layout in
#' `p$x$layout` and frames in `p$x$frames` (as the legacy plotly package
#' does). For a [go_figure()] it returns the htmlwidget that rendering
#' would produce.
#'
#' @param p A [plot_ly()] object or [go_figure()].
#' @param registerFrames Whether to register animation frames.
#' @return A built object.
#' @export
#' @examples
#' p <- mtcars |> plot_ly(x = ~wt, y = ~mpg, color = ~factor(cyl))
#' length(plotly_build(p)$x$data)
plotly_build <- function(p, registerFrames = TRUE) {
  if (inherits(p, "go_figure")) return(plotly_widget(p))
  if (is.null(p)) return(htmltools::browsable(htmltools::div()))
  if (inherits(p, c("gg", "ggmatrix"))) return(plotly_build(ggplotly(p), registerFrames = registerFrames))
  # a figure given as a list (data, layout, ...), as the legacy package's
  # plotly_build.list()
  if (is.list(p) && !is.object(p) && any(c("data", "layout") %in% names(p))) p <- lp_list_plot(p)
  if (!is_legacy_plot(p)) stop("Don't know how to build an object of class ", class(p)[[1L]], call. = FALSE)
  set_last_plot(p)

  # layout attributes (possibly referencing data via formulas)
  layouts <- Map(function(x, id) {
    d <- plotly_data(p, id)
    x <- lp_eval_attrs(x, d)
    n_ann <- max(lengths(x$annotations) %||l% 0)
    if (!is.null(names(x$annotations))) {
      font <- x$annotations[["font"]]
      ann <- lapply(x$annotations, function(v) as.list(rep(v, length.out = n_ann)))
      x$annotations <- lapply(seq_len(n_ann), function(i) {
        a <- lapply(ann, `[[`, i)
        a$font <- font
        a
      })
    }
    x[lengths(x) > 0]
  }, p$x$layoutAttrs %||l% list(), names(p$x$layoutAttrs) %||l% character())
  # axes given `domain = NULL`: no default domain (plotly.js then lays them
  # out itself, e.g. the axes of a splom trace)
  null_domain <- unique(unlist(lapply(p$x$layoutAttrs, function(l) {
    nms <- grep("^[xyz]axis[0-9]*$", names(l), value = TRUE)
    nms[vapply(nms, function(n) is.list(l[[n]]) && "domain" %in% names(l[[n]]) && is.null(l[[n]][["domain"]]), logical(1))]
  })))
  p$x$layoutAttrs <- NULL
  p$x$nullDomain <- null_domain
  annotations <- c(p$x$layout$annotations, unlist(unname(compact(lapply(layouts, `[[`, "annotations"))), recursive = FALSE))
  for (l in layouts) p$x$layout <- lp_modify_list(p$x$layout, l)
  p$x$layout$annotations <- annotations

  frame_mapping <- unique(unlist(lapply(p$x$attrs, function(x) lp_deparse(x[["frame"]])), use.names = FALSE))
  if (length(frame_mapping) > 1) warning("Only one `frame` variable is allowed", call. = FALSE)

  if (length(p$x$attrs) == 0) p$x$attrs <- NULL
  # one (unevaluated) trace with sf data: add an sf layer
  if (length(p$x$attrs) == 1 && !inherits(p$x$attrs[[1]], "plotly_eval") && inherits(plotly_data(p), "sf")) {
    p <- add_sf(p)
  }
  if (is.null(p$x$attrs[[1]][["type"]]) && length(p$x$attrs) > 1) p$x$attrs[[1]] <- NULL
  is_evaled <- function(x) inherits(x, "plotly_eval")
  to_eval <- p$x$attrs[!vapply(p$x$attrs, is_evaled, logical(1))]
  to_eval <- lapply(to_eval, function(tr) {
    if (!is.null(tr[["type"]])) tr[["type"]] <- lp_upgrade_type(tr[["type"]])
    tr
  })
  if (lp_is_geo(p)) {
    to_eval <- lapply(to_eval, function(tr) {
      tr[["x"]] <- tr[["x"]] %||l% tr[["lat"]]
      tr[["y"]] <- tr[["y"]] %||l% tr[["lon"]]
      type <- tr[["type"]] %||l% "scatter"
      if (!grepl("scatter|choropleth", type)) warning("Can't add a '", type, "' trace to a map object", call. = FALSE)
      tr[["type"]] <- if (identical(p$x$layout$mapType, "map")) {
        if (!is.null(tr[["z"]])) "choroplethmap" else "scattermap"
      } else {
        if (!is.null(tr[["z"]])) "choropleth" else "scattergeo"
      }
      tr
    })
  }

  dats <- Map(function(x, id) lp_build_trace(p, x, id), to_eval, names(to_eval) %||l% rep("", length(to_eval)))
  # axis titles collected while building
  for (d in dats) for (ax in names(attr(d, "titles"))) {
    path <- attr(d, "titles")[[ax]]
    if (length(path) == 2L) {
      p$x$layout[[path[[1]]]][[path[[2]]]]$title <- p$x$layout[[path[[1]]]][[path[[2]]]]$title %||l% lp_default(ax)
    } else {
      p$x$layout[[path]]$title <- p$x$layout[[path]]$title %||l% lp_default(ax)
    }
  }
  p$x$attrs <- lapply(p$x$attrs, function(x) structure(x, class = "plotly_eval"))

  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    attr(d, "titles") <- NULL
    scale_attrs <- names(d) %in% paste0(LP_NPSCALES, "s")
    traces <- c(traces, lp_traceify(d[!scale_attrs], d$.plotlyTraceIndex))
    if (i == 1) traces[[1]] <- c(traces[[1]], d[scale_attrs])
  }
  # (groups were separated with missing values in lp_build_trace())
  traces <- lapply(traces, function(x) {
    for (v in intersect(x$.plotlyVariableMapping, LP_NPSCALES)) {
      if (!is.null(x[[v]])) x[[v]] <- structure(lp_uniq(x[[v]]), class = oldClass(x[[v]]))
    }
    x
  })
  if (length(traces)) {
    traces <- lp_map_symbol(traces)
    traces <- lp_map_linetype(traces)
    traces <- lp_map_size(traces)
    traces <- lp_map_size(traces, stroke = TRUE)
    color_title <- unlist(lapply(p$x$attrs, function(x) lp_deparse(x[["color"]] %||l% x[["z"]])))
    stroke_title <- unlist(lapply(p$x$attrs, function(x) lp_deparse(x[["stroke"]])))
    traces <- lp_map_color(traces, title = paste(color_title, collapse = lp_br()), colorway = lp_colorway(p))
    traces <- lp_map_color(traces, stroke = TRUE, title = paste(stroke_title, collapse = lp_br()), colorway = lp_colorway(p))
  }
  mapping_attrs <- c("alpha", "alpha_stroke", LP_NPSCALES, paste0(LP_NPSCALES, "s"), ".plotlyGroupIndex",
                     ".plotlyMissingIndex", ".plotlyTraceIndex", ".plotlyVariableMapping", "inherit")
  traces <- lapply(traces, function(tr) {
    for (j in mapping_attrs) tr[[j]] <- NULL
    tr
  })
  p$x$data <- unname(c(p$x$data, traces))
  p <- lp_apply_transforms(p)
  p <- lp_upgrade_legacy_polar(p)
  p <- lp_supply_defaults(p)
  p$x$nullDomain <- NULL
  p <- lp_upgrade_removed(p)
  p$x$layout <- lp_dedupe_names(p$x$layout)
  p$x$data <- lp_dedupe_names(p$x$data)
  p <- lp_cartesian2geo(p)
  p <- lp_fit_bounds(p)
  p <- lp_populate_categorical_axes(p)
  p <- lp_translate_linebreaks(p)
  p <- lp_verify_mode(p)
  p <- lp_verify_arrays(p)
  p <- lp_verify_hovermode(p)
  p <- lp_verify_webgl(p)
  p <- lp_supply_highlight_attrs(p)
  p <- lp_verify_showlegend(p)
  p$x$shinyEvents <- p$x$shinyEvents %||l% lp_default_events()
  p <- lp_verify_key_type(p)
  p <- lp_verify_config(p)
  if (registerFrames) p <- lp_register_frames(p, frame_mapping)
  p <- lp_verify_guides(p)
  p <- lp_verify_attr_spec(p)
  p
}

lp_is_geo <- function(p) !is.null(p$x$layout$mapType)

# Evaluate one trace's attributes and train its data (legacy plotly_build's
# per-trace step).
lp_build_trace <- function(p, x, id) {
  dat <- plotly_data(p, id)
  trace <- structure(lp_eval_attrs(x, dat), class = oldClass(x))
  trace <- lp_verify_type(trace)
  trace <- lp_verify_orientation(trace)
  trace <- lp_coerce_attr_defaults(trace, p$x$layout)
  # crosstalk (highlight_key()) data: attach its keys and group
  if (LP_CT_KEY %in% names(dat) && isTRUE(trace[["inherit"]] %||l% TRUE)) {
    trace[["key"]] <- trace[["key"]] %||l% dat[[LP_CT_KEY]]
    trace[["set"]] <- trace[["set"]] %||l% attr(dat, "set")
  }
  grps <- if (lp_has_group(trace)) lp_group_vars(dat)
  if (length(grps) && any(lengths(trace) == NROW(dat))) {
    trace[[".plotlyGroupIndex"]] <- lp_group_index(lapply(grps, function(g) dat[[g]]))
  }
  titles <- list()
  for (ax in c("x", "y", "z")) {
    if (sum(names(trace) %in% ax) == 1) {
      title <- lp_deparse(x[[ax]])
      if (!is.null(title)) {
        nm <- paste0(ax, "axis")
        titles[[title]] <- if (lp_is3d(trace$type) || ax == "z") c("scene", nm) else nm
      }
    }
  }
  if (inherits(trace, c("plotly_surface", "plotly_contour"))) {
    if (!is.matrix(trace[["z"]]) || !is.numeric(trace[["z"]])) stop("`z` must be a numeric matrix", call. = FALSE)
  }
  entry <- get_validator(trace[["type"]])
  array_attrs <- if (lp_is_tidy(trace)) {
    Filter(function(ch) {
      v <- get_validator(paste(trace[["type"]], ch, sep = "."))
      identical(v$superclass, "DataArrayValidator") || isTRUE(v$array_ok)
    }, entry$children)
  }
  all_attrs <- c(array_attrs, lp_special_attrs(trace), LP_NPSCALES, "frame", ".plotlyGroupIndex", "text",
                 "key", "fillcolor", "name", "legendgroup")
  tr <- trace[names(trace) %in% all_attrs]
  # vectors, and unnamed lists with a value per point (e.g. customdata given
  # as a list-column), which are sorted and split into groups with the data
  per_point <- function(v) is.null(dim(v)) && (is.atomic(v) || (is.list(v) && !is.data.frame(v) && is.null(names(v)) && length(v) > 1))
  tr <- tr[vapply(tr, per_point, logical(1))]
  n <- max(c(0L, lengths(tr)))
  tr <- tr[lengths(tr) %in% c(1L, n) | vapply(tr, is.atomic, logical(1))]
  built <- if (n > 0) {
    data.table::as.data.table(lapply(tr, function(v) if (length(v) == 1L) rep(v, n) else v))
  } else data.table::data.table()
  for (k in names(tr)) if (inherits(tr[[k]], "AsIs")) data.table::set(built, j = k, value = I(built[[k]]))
  if (NROW(built) > 0) {
    is_asis <- vapply(built, function(v) inherits(v, "AsIs"), logical(1))
    is_disc <- vapply(built, lp_is_discrete, logical(1))
    is_split <- names(built) %in% c("split", "linetype", "frame", "fillcolor", "name") |
      (!is_asis & is_disc & names(built) %in% c("symbol", "color"))
    if (any(is_split)) {
      paste2 <- function(a, b) if (identical(a, b)) a else paste(a, b, sep = lp_br())
      split_vars <- as.list(built)[is_split]
      data.table::set(built, j = ".plotlyTraceIndex", value = Reduce(paste2, split_vars))
      trace$frameOrder <- which(names(split_vars) %in% "frame")
    }
    built <- lp_arrange(built, ".plotlyTraceIndex")
    pos <- intersect(c("x", "y", "z"), names(built))
    complete <- if (length(pos)) !Reduce(`|`, lapply(pos, function(k) is.na(built[[k]]))) else rep(TRUE, nrow(built))
    if (any(!complete) && !lp_has_group(trace)) warning("Ignoring ", sum(!complete), " observations", call. = FALSE)
    data.table::set(built, j = ".plotlyMissingIndex", value = cumsum(!complete))
    if (!all(complete)) built <- built[complete]
    if (length(grps) && lp_has_group(trace) && isTRUE(trace[["connectgaps"]])) {
      stop("Can't use connectgaps=TRUE when data has group(s).", call. = FALSE)
    }
    data.table::set(built, j = ".plotlyGroupIndex", value = lp_group_index(list(
      built[[".plotlyGroupIndex"]] %||l% rep("", nrow(built)), built[[".plotlyMissingIndex"]])))
    built <- lp_arrange(built, c(".plotlyTraceIndex", ".plotlyGroupIndex", if (inherits(trace, "plotly_line")) "x"))
    built <- lp_train_data(built, trace)
    # Separate groups with missing values for all of the layer's traces at
    # once: nesting in the trace index gives the same result as the legacy
    # package's group2NA() call on each trace after splitting. That call sees
    # traces whose class was dropped by `[`, so only lp_has_group()'s
    # type/mode test applies (no `ordered`, no `retrace.first`).
    if (lp_has_group(unclass(trace))) {
      built <- group2NA(built, ".plotlyGroupIndex",
                        nested = if (".plotlyTraceIndex" %in% names(built)) ".plotlyTraceIndex")
    }
    trace[[".plotlyVariableMapping"]] <- names(built)
    for (k in names(built)) trace[[k]] <- built[[k]]
  }
  trace[c("ymin", "ymax", "yend", "xend")] <- NULL
  out <- trace[lengths(trace) > 0]
  oldClass(out) <- oldClass(trace)
  attr(out, "titles") <- titles
  out
}

# dplyr::arrange() replacement for a data.table, by reference (stable; factors
# by level, strings in C locale, missing values last).
lp_arrange <- function(d, vars) {
  vars <- vars[vars %in% names(d)]
  if (length(vars)) data.table::setorderv(d, cols = vars, na.last = TRUE)
  d
}

# Group ids ordered and grouped like interaction(...) codes (the first
# variable varies fastest; factors by level, other values as factor() sorts
# them), without building interaction()'s levels, which multiply.
lp_group_index <- function(vars) {
  codes <- lapply(vars, function(v) {
    if (is.factor(v)) as.integer(v) else match(v, sort(unique(v)))
  })
  data.table::frankv(rev(codes), ties.method = "dense", na.last = "keep")
}

lp_special_attrs <- function(trace) {
  out <- switch(class(trace)[[1]] %||l% "", plotly_segment = c("xend", "yend"), plotly_ribbon = c("ymin", "ymax"), NULL)
  if (isTRUE(trace[["type"]] %in% c("scattermap", "scattergeo"))) out <- c(out, "x", "y")
  out
}

lp_is3d <- function(type) (type %||l% "scatter") %in% c("mesh3d", "scatter3d", "surface")

lp_train_data <- function(data, trace) {
  if (inherits(trace, "plotly_ribbon")) data <- lp_ribbon_dat(data)
  if (inherits(trace, "plotly_segment")) {
    data.table::set(data, j = ".plotlyGroupIndex", value = seq_len(NROW(data)))
    idx <- rep(seq_len(NROW(data)), each = 2)
    keep <- setdiff(names(data), c("xend", "yend"))
    dat <- data[idx, keep, with = FALSE]
    idx2 <- seq.int(2, NROW(dat), by = 2)
    data.table::set(dat, i = idx2, j = "x", value = data[["xend"]])
    data.table::set(dat, i = idx2, j = "y", value = data[["yend"]])
    data <- dat
  }
  data
}

# The lower edge (ymin) left to right, then the upper edge (ymax) right to
# left, as one closed path.
lp_ribbon_dat <- function(dat) {
  n <- nrow(dat)
  if (n == 0) return(dat)
  o <- order(dat[["x"]])
  o2 <- order(dat[["x"]], decreasing = TRUE)
  others <- setdiff(names(dat), c("x", "ymin", "ymax", "y"))
  rows <- c(o, o[n], o2)
  out <- data.table::data.table(x = dat[["x"]][rows], y = c(dat[["ymin"]][c(o, o[n])], dat[["ymax"]][o2]))
  for (k in others) data.table::set(out, j = k, value = dat[[k]][rows])
  out
}

lp_traceify <- function(dat, x = NULL) {
  if (length(x) == 0) return(list(dat))
  lvls <- if (is.factor(x)) levels(x) else unique(x)
  lvls <- lvls[lvls %in% x]
  if (is.ordered(x)) lvls <- rev(lvls)
  n <- length(x)
  recurse <- function(z, idx) if (is.list(z) && !is.data.frame(z)) lapply(z, recurse, idx) else if (length(z) == n) z[idx] else z
  # row indices of each level, in one pass
  rows <- split(seq_len(n), factor(match(x, lvls), levels = seq_along(lvls)))
  Map(function(l, idx) {
    d <- lapply(dat, recurse, idx = idx)
    oldClass(d) <- oldClass(dat)
    d$name <- d$name %||l% as.character(l)
    d
  }, lvls, rows)
}

lp_verify_type <- function(trace) {
  if (is.null(trace$type)) {
    attrs <- names(trace)
    trace$type <- if (all(c("x", "y", "z") %in% attrs)) {
      if (all(c("i", "j", "k") %in% attrs)) "mesh3d" else "scatter3d"
    } else if (all(c("x", "y") %in% attrs)) {
      xn <- !lp_is_discrete(trace[["x"]])
      yn <- !lp_is_discrete(trace[["y"]])
      if (xn && yn) "scatter" else if (xn || yn) "bar" else "histogram2d"
    } else if ("y" %in% attrs || "x" %in% attrs) {
      "histogram"
    } else if ("z" %in% attrs) {
      "heatmap"
    } else {
      warning("No trace type specified and no positional attributes specified", call. = FALSE)
      "scatter"
    }
    message("No trace type specified:\n  Based on info supplied, a '", trace$type,
            "' trace seems appropriate.\n  Read more about this trace type -> https://plotly.com/r/reference/#", trace$type)
  }
  if (!is.character(trace$type) || length(trace$type) != 1) stop("The trace type must be a character vector of length 1.\n", call. = FALSE)
  if (is.null(get_validator(trace$type)$trace_type)) {
    stop("Trace type must be one of the following: \n'", paste(available_trace_types(), collapse = "', '"), "'", call. = FALSE)
  }
  if (grepl("scatter", trace$type) && is.null(trace$mode)) {
    message("No ", trace$type, " mode specifed:\n  Setting the mode to markers\n  Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode")
    trace$mode <- "markers"
  }
  trace
}

lp_verify_orientation <- function(trace) {
  xn <- !lp_is_discrete(trace[["x"]]) && !is.null(trace[["x"]])
  yn <- !lp_is_discrete(trace[["y"]]) && !is.null(trace[["y"]])
  if (xn && !yn && any(c("bar", "box") %in% trace[["type"]])) trace$orientation <- "h"
  if (yn && "histogram" %in% trace[["type"]]) trace$orientation <- "h"
  trace
}

lp_coerce_attr_defaults <- function(trace, layout) {
  if (length(trace[["stroke"]]) && !lp_is_default(trace[["stroke"]])) trace$span <- trace[["span"]] %||l% lp_default(I(1))
  if (trace[["type"]] %in% c("sunburst", "pie", "treemap")) {
    trace$stroke <- trace[["stroke"]] %||l% lp_default(I(layout$paper_bgcolor %||l% "#ffffff"))
  }
  trace
}

# Mappings ----------------------------------------------------------------------------------

lp_supply_palette <- function(default, user) {
  for (i in seq_along(user)) {
    idx <- names(user)[i] %||l% i
    default[idx] <- user[i]
  }
  default
}

lp_map_symbol <- function(traces) {
  lst <- lapply(traces, `[[`, "symbol")
  n <- lengths(lst)
  if (all(n == 0)) return(traces)
  lvls <- lp_get_levels(unlist(compact(lst)))
  shapes <- c(16, 17, 15, 3, 7, 8)
  if (length(lvls) > length(shapes)) warning("The shape palette can deal with a maximum of 6 discrete values", call. = FALSE)
  pal <- stats_named(shapes[seq_along(lvls)], lvls)
  pal <- lp_supply_palette(pal, traces[[1]][["symbols"]])
  valid <- c(as.character(get_validator("scatter.marker.symbol")$values$chr), as.character(get_validator("scatter.marker.symbol")$values$num))
  for (i in which(n > 0)) {
    s <- lst[[i]]
    syms <- lp_lookup(if (inherits(s, "AsIs")) s else as.character(pal[as.character(s)]), LP_PCH)
    bad <- setdiff(syms, valid)
    if (length(bad)) warning("The following are not valid symbol codes:\n'", paste(bad, collapse = "', '"), "'", call. = FALSE)
    traces[[i]][["marker"]] <- lp_modify_list(list(symbol = lp_default(syms)), traces[[i]][["marker"]])
    if (!grepl("markers", traces[[i]]$mode %||l% "")) {
      message("Adding markers to mode; otherwise symbol would have no effect.")
      traces[[i]]$mode <- paste0(traces[[i]]$mode, "+markers")
    }
  }
  traces
}

lp_map_linetype <- function(traces) {
  lst <- lapply(traces, `[[`, "linetype")
  n <- lengths(lst)
  if (all(n == 0)) return(traces)
  lvls <- lp_get_levels(unlist(compact(lst)))
  ltys <- c("solid", "22", "42", "44", "13", "1343")
  pal <- stats_named(ltys[seq_along(lvls)], lvls)
  pal <- lp_supply_palette(pal, traces[[1]][["linetypes"]])
  if (length(pal) > 6) warning("plotly.js only supports 6 different linetypes", call. = FALSE)
  for (i in which(n > 0)) {
    l <- lst[[i]]
    dashes <- lp_lookup(if (inherits(l, "AsIs")) l else as.character(pal[as.character(l)]), LP_LTY)
    valid <- c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
    illegal <- setdiff(dashes, valid)
    if (length(illegal)) {
      warning("The following are not valid linetype codes:\n'", paste(illegal, collapse = "', '"), "'\n",
              "Valid linetypes include:\n'", paste(valid, collapse = "', '"), "'", call. = FALSE)
    }
    traces[[i]][["line"]] <- lp_modify_list(list(dash = lp_default(dashes)), traces[[i]][["line"]])
    if (!grepl("lines", traces[[i]]$mode %||l% "")) {
      message("Adding lines to mode; otherwise linetype would have no effect.")
      traces[[i]][["mode"]] <- paste0(traces[[i]][["mode"]], "+lines")
    }
  }
  traces
}

lp_map_size <- function(traces, stroke = FALSE) {
  key <- if (stroke) "span" else "size"
  lst <- lapply(traces, `[[`, key)
  n <- lengths(lst)
  if (all(n == 0)) return(traces)
  all_size <- unlist(compact(lst))
  if (!is.null(all_size) && lp_is_discrete(all_size)) stop("`size`/`width` values must be numeric .", call. = FALSE)
  rng <- range(all_size, na.rm = TRUE)
  map_fn <- function(trace, sizes) {
    type <- trace$type %||l% "scatter"
    u <- lp_uniq(sizes)
    single <- length(u) == 1
    pick <- function(path) if (single) u else if (lp_array_ok(type, path)) sizes
    if (stroke) {
      if (lp_has_attr(type, "marker.line")) trace$marker$line <- lp_modify_list(list(width = lp_default(pick(c("marker", "line", "width")))), trace$marker$line)
      for (a in c("error_y", "error_x")) {
        if (lp_has_attr(type, a)) trace[[a]] <- lp_modify_list(list(thickness = lp_default(pick(c(a, "thickness")))), trace[[a]])
      }
      if (lp_has_fill(trace) && lp_has_attr(type, "line")) {
        s <- if (single) u else if (lp_array_ok(type, c("line", "width"))) sizes else NA
        if (identical(s, NA)) warning("`line.width` does not currently support multiple values.", call. = FALSE)
        else trace$line <- lp_modify_list(list(width = lp_default(s)), trace$line)
      }
    } else {
      if (lp_has_attr(type, "marker") && lp_has_attr(type, "marker.size")) {
        trace$marker <- lp_modify_list(list(size = lp_default(pick(c("marker", "size"))), sizemode = lp_default("area")), trace$marker)
      }
      if (lp_has_attr(type, "textfont")) trace$textfont <- lp_modify_list(list(size = lp_default(pick(c("textfont", "size")))), trace$textfont)
      for (a in c("error_y", "error_x")) {
        if (lp_has_attr(type, a)) trace[[a]] <- lp_modify_list(list(width = lp_default(pick(c(a, "width")))), trace[[a]])
      }
      if (!lp_has_fill(trace) && lp_has_attr(type, "line")) {
        s <- if (single) u else if (lp_array_ok(type, c("line", "width"))) sizes else NA
        if (identical(s, NA)) warning("`line.width` does not currently support multiple values.", call. = FALSE)
        else trace$line <- lp_modify_list(list(width = lp_default(s)), trace$line)
      }
    }
    trace
  }
  for (i in which(n > 0)) {
    s <- lst[[i]]
    size_i <- if (inherits(s, "AsIs")) lp_remove_class(s, "AsIs") else {
      lp_rescale(s, to = if (stroke) traces[[1]][["spans"]] else traces[[1]][["sizes"]], from = rng)
    }
    traces[[i]] <- map_fn(traces[[i]], size_i)
  }
  traces
}

lp_color_fill <- function(trace, rgba, is_colorway = FALSE, stroke = FALSE) {
  type <- trace$type %||l% "scatter"
  u <- lp_uniq(rgba)
  single <- length(u) == 1
  dflt <- if (is_colorway) function(x) lp_prefix_class(lp_default(x), "colorway") else lp_default
  if (stroke) {
    if (lp_has_attr(type, "marker.line")) {
      col <- if (single) u else if (lp_array_ok(type, c("marker", "line", "color"))) rgba
      trace$marker$line <- lp_modify_list(list(color = dflt(col)), trace$marker$line)
    }
    if (lp_has_fill(trace)) {
      col <- if (single) u else if (lp_array_ok(type, c("line", "color"))) rgba
      if (is.null(col)) warning("`line.color` does not currently support multiple values.", call. = FALSE)
      else trace$line <- lp_modify_list(list(color = dflt(col)), trace$line)
    }
    return(trace)
  }
  for (a in c("marker", "textfont", "error_y", "error_x")) {
    if (!lp_has_attr(type, a)) next
    if (is_colorway && a == "textfont") next
    col <- if (single) u else if (lp_array_ok(type, c(a, "color"))) rgba else NA
    if (identical(col, NA)) warning("`", a, ".color` does not currently support multiple values.", call. = FALSE)
    else trace[[a]] <- lp_modify_list(list(color = dflt(col)), trace[[a]])
  }
  if (lp_has_fill(trace)) {
    if (!single) warning("Only one fillcolor per trace allowed", call. = FALSE)
    if (is.null(trace[["alpha"]])) u <- toRGB(u, 0.5)
    if (single) trace <- lp_modify_list(list(fillcolor = dflt(u)), trace)
  } else if (lp_has_attr(type, "line")) {
    col <- if (single) u else if (lp_array_ok(type, c("line", "color"))) rgba else NA
    if (identical(col, NA)) warning("`line.color` does not currently support multiple values.", call. = FALSE)
    else trace$line <- lp_modify_list(list(color = dflt(col)), trace$line)
  }
  trace
}

# Colorscales given in a trace (as a name, list of [value, color] pairs,
# two-column matrix or data frame, or list of values and colors) are kept: the
# color mapping only supplies a default one.
lp_map_color <- function(traces, ...) {
  user <- lapply(traces, function(tr) {
    list(cs = lp_colorscale_json(tr[["colorscale"]]),
         mcs = if (is.list(tr[["marker"]])) lp_colorscale_json(tr[["marker"]][["colorscale"]]))
  })
  traces <- lp_map_color_scale(traces, ...)
  # (the mapping may append a colorbar trace, which has no user colorscale)
  for (i in seq_along(user)) {
    u <- user[[i]]
    if (!is.null(u$cs) && !lp_is_default(u$cs)) traces[[i]][["colorscale"]] <- u$cs
    if (!is.null(u$mcs) && !lp_is_default(u$mcs) && is.list(traces[[i]][["marker"]])) traces[[i]][["marker"]][["colorscale"]] <- u$mcs
  }
  # an appended colorbar trace shows the (first) user colorscale
  given <- Filter(function(cs) !is.null(cs) && !lp_is_default(cs), unlist(lapply(user, function(u) list(u$mcs, u$cs)), recursive = FALSE))
  for (i in seq_along(traces)[-seq_along(user)]) {
    if (length(given) && inherits(traces[[i]], "plotly_colorbar") && is.list(traces[[i]][["marker"]])) {
      traces[[i]][["marker"]][["colorscale"]] <- given[[1]]
    }
  }
  traces
}

# The legacy package's colorscale_json(): a colorscale as a name or a list of
# [value, color] pairs.
lp_colorscale_json <- function(x) {
  if (!length(x) || lp_is_default(x)) return(x)
  if (is.character(x) && !is.matrix(x)) return(x)
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("A colorscale matrix requires two columns", call. = FALSE)
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x[[1]] <- as.numeric(x[[1]])
  }
  if (is.data.frame(x)) return(lp_df_rows(x))
  if (is.list(x) && length(x) == 2 && length(x[[1]]) > 1 && length(x[[1]]) == length(x[[2]]) &&
      !is.list(x[[1]]) && !anyNA(suppressWarnings(as.numeric(x[[1]][!is.na(x[[1]])])))) {
    return(Map(function(v, c) list(v, c), as.numeric(x[[1]]), as.character(x[[2]])))
  }
  x
}

lp_map_color_scale <- function(traces, stroke = FALSE, title = "", colorway, na.color = "transparent") {
  color <- if (stroke) {
    lapply(traces, function(x) x[["stroke"]] %||l% x[["color"]])
  } else {
    lapply(traces, function(x) {
      x[["color"]] %||l% if (grepl("histogram2d", x[["type"]])) c(0, 1) else if (lp_has_attr(x[["type"]], "colorscale")) x[["surfacecolor"]] %||l% x[["z"]]
    })
  }
  alphas <- vapply(traces, function(x) (if (stroke) x$alpha_stroke else x[["alpha"]]) %||l% 1, numeric(1))
  is_const <- vapply(color, function(x) inherits(x, "AsIs") || is.null(x), logical(1))
  is_num <- vapply(color, is.numeric, logical(1)) & !is_const
  is_disc <- vapply(color, lp_is_discrete, logical(1)) & !is_const
  if (any(is_num & is_disc)) stop("Can't have both discrete and numeric color mappings", call. = FALSE)
  single <- lengths(lapply(color, lp_uniq)) == 1
  types <- vapply(traces, function(tr) tr$type %||l% "scatter", character(1))
  modes <- vapply(traces, function(tr) tr$mode %||l% "lines", character(1))
  is_scatter <- grepl("scatter", types)
  has_line <- ifelse(is_scatter, grepl("line", modes), lp_has_attr(types, "line"))
  has_line_color <- vapply(types, lp_array_ok, logical(1), path = c("line", "color"))
  has_text <- ifelse(is_scatter, grepl("text", modes), lp_has_attr(types, "textfont"))
  has_text_color <- vapply(types, lp_array_ok, logical(1), path = c("textfont", "color"))
  has_z <- lp_has_attr(types, "colorscale") & !stroke &
    any(vapply(traces, function(tr) !is.null(tr[["z"]]) || grepl("histogram2d", tr[["type"]]), logical(1)))

  if (any(is_const)) {
    codes <- Map(function(a, b) a %||l% b, color, rep(colorway, length.out = length(traces)))
    codes <- Map(toRGB, codes[is_const], alphas[is_const])
    is_cw <- lengths(color[is_const]) == 0
    traces[is_const] <- Map(lp_color_fill, traces[is_const], codes, is_cw, MoreArgs = list(stroke = stroke))
  }
  palette <- if (stroke) traces[[1]][["strokes"]] %||l% traces[[1]][["colors"]] else traces[[1]][["colors"]]
  if (any(is_disc)) {
    ordered <- all(vapply(color[is_disc], is.ordered, logical(1)))
    lvls <- lp_get_levels(unlist(color[is_disc]))
    N <- length(lvls)
    pal <- palette %||l% if (ordered) substr(viridisLite::viridis(N), 1, 7) else RColorBrewer::brewer.pal(max(N, 3L), "Set2")
    scale <- lp_col_factor(pal, names(pal) %||l% lvls, na.color = na.color)
    codes <- Map(function(x, a) toRGB(scale(as.character(x)), a), color[is_disc], alphas[is_disc])
    traces[is_disc] <- Map(lp_color_fill, traces[is_disc], codes, MoreArgs = list(stroke = stroke))
  }
  if (any(is_num)) {
    pal <- palette %||l% substr(viridisLite::viridis(10), 1, 7)
    rng <- range(unlist(color[is_num]), na.rm = TRUE)
    scale <- lp_col_numeric(pal, rng, na.color = na.color)
    vals <- if (diff(rng) > 0) seq(rng[1], rng[2], length.out = 25) else c(0, 1)
    # a two-column (value, color) matrix, as in plotly 4.x (legacy_figure() makes it a list of pairs)
    colorscale <- cbind(lp_rescale(vals), toRGB(scale(vals), alphas[[1]]))
    obj <- list(colorbar = lapply(list(title = as.character(title), ticklen = 2), lp_default),
                cmin = lp_default(rng[1]), cmax = lp_default(rng[2]),
                colorscale = lp_default(colorscale), showscale = lp_default(FALSE))
    for (i in which(is_num)) {
      if (has_z[[i]]) {
        o <- obj
        o[c("cmin", "cmax")] <- NULL
        o$showscale <- lp_default(TRUE)
        traces[[i]] <- structure(lp_modify_list(o, traces[[i]]), class = c("plotly_colorbar", "zcolor"))
        next
      }
      if (single[[i]]) {
        traces[[i]] <- lp_color_fill(traces[[i]], toRGB(scale(lp_uniq(color[[i]])), alphas[[i]]), stroke = stroke)
      } else {
        o <- obj
        o$color <- lp_default(color[[i]])
        if (stroke) {
          # numeric line colors only where marker.line has a colorscale (plotly.js
          # ignored them elsewhere, e.g. the stroke of a choropleth)
          if (lp_has_attr(traces[[i]]$type %||l% "scatter", "marker.line.colorscale")) traces[[i]]$marker$line <- lp_modify_list(o, traces[[i]]$marker$line)
        } else {
          traces[[i]]$marker <- lp_modify_list(o, traces[[i]]$marker)
        }
        if (has_line[[i]]) {
          if (has_line_color[[i]]) traces[[i]]$line <- lp_modify_list(o, traces[[i]]$line)
          else warning("line.color doesn't (yet) support data arrays", call. = FALSE)
        }
        if (has_text[[i]]) {
          if (has_text_color[[i]]) traces[[i]]$textfont <- lp_modify_list(o, traces[[i]]$textfont)
          else warning("textfont.color doesn't (yet) support data arrays", call. = FALSE)
        }
        if (lp_has_fill(traces[[i]])) {
          warning("Only one fillcolor per trace allowed", call. = FALSE)
          col <- toRGB(scale(mean(color[[i]], na.rm = TRUE)), alphas[[i]])
          if (is.null(traces[[i]][["alpha"]])) col <- toRGB(col, 0.5)
          traces[[i]] <- lp_modify_list(list(fillcolor = col), traces[[i]])
        }
      }
    }
    if (any(has_z)) return(traces)
    if (stroke && sum(lengths(lapply(traces, `[[`, "stroke"))) == 0) return(traces)
    obj$color <- rng
    obj$showscale <- lp_default(TRUE)
    bar <- list(x = lp_range(lapply(traces, `[[`, "x")),
                y = lp_range(lapply(traces, `[[`, "y")),
                type = if (any(types %in% LP_GL_TYPES)) "scattergl" else "scatter",
                mode = "markers", opacity = 0, hoverinfo = "none", showlegend = FALSE, marker = obj)
    if ("scatter3d" %in% types) {
      bar$type <- "scatter3d"
      bar$z <- lp_range(lapply(traces, `[[`, "z"))
    }
    if (length(geo <- intersect(c("scattergeo", "scattermap"), types))) {
      bar$type <- geo[[1]]
      bar$lat <- bar$y
      bar$lon <- bar$x
      bar$x <- bar$y <- NULL
    }
    traces[[length(traces) + 1]] <- structure(bar, class = "plotly_colorbar")
  }
  traces
}

# Layout and trace checks ---------------------------------------------------------------------

lp_supply_defaults <- function(p) {
  if (isTRUE(p$x$subplot)) return(p)
  anchors <- if (identical(p$x$layout$mapType, "geo")) c(geo = "geo") else if (identical(p$x$layout$mapType, "map")) c(subplot = "map") else c(xaxis = "x", yaxis = "y")
  p$x$data <- lapply(p$x$data, function(tr) {
    for (i in seq_along(anchors)) {
      key <- names(anchors)[[i]]
      if (!lp_has_attr(tr[["type"]] %||l% "scatter", key)) next
      tr[[key]] <- sub("^y1$", "y", sub("^x1$", "x", tr[[key]][1])) %||l% anchors[[i]]
    }
    tr
  })
  if (lp_is_type(p, "parcoords")) p$x$layout$margin$t <- NULL
  if (lp_is_geo(p)) {
    p$x$layout[grepl("^[x-y]axis", names(p$x$layout))] <- NULL
    p$x$layout[[p$x$layout$mapType]] <- lp_modify_list(list(domain = list(x = c(0, 1), y = c(0, 1))), p$x$layout[[p$x$layout$mapType]])
  } else if (!length(p$x$layout[["grid"]])) {
    types <- vapply(p$x$data, function(tr) tr[["type"]] %||l% "scatter", character(1))
    axes <- unique(unlist(lapply(types, function(t) grep("^[a-z]axis$", get_validator(t)$children, value = TRUE))))
    for (ax in axes) {
      dflt <- if (ax %in% p$x$nullDomain) list(automargin = TRUE) else list(domain = c(0, 1), automargin = TRUE)
      p$x$layout[[ax]] <- lp_modify_list(dflt, p$x$layout[[ax]])
    }
  }
  p
}

lp_is_type <- function(p, type) {
  all(vapply(p$x$data, function(tr) tr[["type"]] %||l% "scatter", character(1)) %in% type)
}

lp_cartesian2geo <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    if (isTRUE(tr[["type"]] %in% c("scattermap", "scattergeo"))) {
      tr[["lat"]] <- tr[["lat"]] %||l% tr[["y"]]
      tr[["lon"]] <- tr[["lon"]] %||l% tr[["x"]]
      tr[c("x", "y")] <- NULL
    }
    tr
  })
  p
}

lp_populate_categorical_axes <- function(p) {
  axes <- p$x$layout[grepl("^xaxis|^yaxis", names(p$x$layout))]
  if (!length(axes)) axes <- list(xaxis = NULL, yaxis = NULL)
  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    name <- names(axes)[[i]]
    letter <- substr(name, 1, 1)
    if (!is.null(axis$ticktext) || !is.null(axis$tickvals)) next
    on_axis <- function(tr) {
      is.null(tr[["geo"]]) && sub("axis", "", name) %in% (tr[[sub("[0-9]+", "", name)]] %||l% letter) && !is.matrix(tr[["z"]])
    }
    d <- lapply(p$x$data, `[[`, letter)[vapply(p$x$data, on_axis, logical(1))]
    if (!length(d)) next
    disc <- vapply(d, lp_is_discrete, logical(1))
    if (0 < sum(disc) && sum(disc) < length(d)) {
      warning("Can't display both discrete & non-discrete data on same axis", call. = FALSE)
      next
    }
    if (sum(disc) == 0) next
    cats <- unique(unlist(lapply(d, lp_get_levels)))
    if (any(!vapply(d, is.factor, logical(1)))) cats <- sort(cats)
    p$x$layout[[name]]$type <- p$x$layout[[name]]$type %||l% "category"
    p$x$layout[[name]]$categoryorder <- p$x$layout[[name]]$categoryorder %||l% "array"
    p$x$layout[[name]]$categoryarray <- p$x$layout[[name]]$categoryarray %||l% cats
  }
  p
}

lp_translate_linebreaks <- function(p) {
  recurse <- function(a) {
    if (is.list(a) && !is.data.frame(a)) {
      a[] <- lapply(a, recurse)
    } else if (is.factor(a)) {
      levels(a) <- gsub("\n", lp_br(), levels(a), fixed = TRUE)
    } else if (is.character(a)) {
      at <- attributes(a)
      a <- gsub("\n", lp_br(), a, fixed = TRUE)
      attributes(a) <- at
    }
    a
  }
  p$x$data <- recurse(p$x$data)
  p$x$layout <- recurse(p$x$layout)
  p
}

# Does obj contain a value that isn't a default? (Default-marked lists, such
# as colorscales, count as defaults as a whole.)
lp_user_specified <- function(obj) {
  if (!length(obj)) return(FALSE)
  user <- function(v) if (lp_is_default(v)) FALSE else if (is.list(v)) any(vapply(v, user, logical(1))) else TRUE
  user(obj)
}

lp_verify_mode <- function(p) {
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (!grepl("scatter", tr$type %||l% "scatter")) next
    if (lp_user_specified(tr$marker) && !grepl("markers", tr$mode %||l% "")) {
      message("A marker object has been specified, but markers is not in the mode\nAdding markers to the mode...")
      p$x$data[[i]]$mode <- paste0(p$x$data[[i]]$mode, "+markers")
    }
    if (lp_user_specified(tr$line) && !grepl("lines", tr$mode %||l% "")) {
      message("A line object has been specified, but lines is not in the mode\nAdding lines to the mode...")
      p$x$data[[i]]$mode <- paste0(p$x$data[[i]]$mode, "+lines")
    }
    if (lp_user_specified(tr$textfont) && !grepl("text", tr$mode %||l% "")) {
      warning("A textfont object has been specified, but text is not in the mode\nAdding text to the mode...", call. = FALSE)
      p$x$data[[i]]$mode <- paste0(p$x$data[[i]]$mode, "+text")
    }
  }
  p
}

lp_verify_arrays <- function(p) {
  for (k in c("annotations", "shapes", "images")) {
    thing <- p$x$layout[[k]]
    if (is.list(thing) && !is.null(names(thing))) p$x$layout[[k]] <- list(thing)
  }
  p
}

# The legacy package sets hovermode = "closest" for every plot (its
# highlight defaults include on = "plotly_click").
lp_verify_hovermode <- function(p) {
  if (is.null(p$x$layout$hovermode)) p$x$layout$hovermode <- "closest"
  p
}

lp_verify_webgl <- function(p) {
  if (!isTRUE(p$x$.plotlyWebGl)) return(p)
  types <- vapply(p$x$data, function(x) x[["type"]][1] %||l% "scatter", character(1))
  can_gl <- vapply(paste0(types, "gl"), function(t) !is.null(get_validator(t)$trace_type), logical(1))
  already <- grepl("gl$", types)
  if (any(!can_gl & !already)) warning("The following traces don't have a WebGL equivalent: ", paste(which(!can_gl & !already), collapse = ", "), call. = FALSE)
  for (i in which(can_gl)) p$x$data[[i]]$type <- paste0(p$x$data[[i]]$type, "gl")
  p
}

lp_verify_showlegend <- function(p) {
  if (isTRUE(p$x$.hideLegend)) p$x$layout$showlegend <- FALSE
  show <- vapply(p$x$data, function(x) isTRUE(lp_remove_class(x$showlegend %||l% TRUE, "plotly_default")), logical(1))
  single_pie <- identical("pie", unlist(lapply(p$x$data, function(tr) tr$type)))
  p$x$layout$showlegend <- p$x$layout$showlegend %|D|% lp_default(sum(show) > 1 || single_pie)
  p
}

# Keys (event-data `key`, crosstalk keys) as character, as in plotly 4.x: a
# trace whose key has one unique value (possibly a vector, e.g. the keys of
# a fitted line's group) gets that value and `_isSimpleKey` (the whole trace
# matches); list keys are nested (`_isNestedKey`). They stay in the traces
# (with the crosstalk `set`) until legacy_figure() moves them to widget
# settings (lp_extract_keys()).
lp_verify_key_type <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    k <- tr[["key"]]
    if (is.null(k)) return(tr)
    if ("select" %in% p$x$layout$clickmode && "plotly_click" %in% p$x$highlight$on) {
      warning("`layout.clickmode` = 'select' is not designed to work well with ",
              "the R package's linking framework (i.e. crosstalk support).", call. = FALSE)
    }
    u <- lp_uniq(k)
    if (length(u) == 1) {
      tr$key <- u[[1]]
      tr[["_isSimpleKey"]] <- TRUE
      tr[["_isNestedKey"]] <- FALSE
    }
    tr[["_isNestedKey"]] <- tr[["_isNestedKey"]] %||l% !is.atomic(k)
    tr$key <- if (isTRUE(tr[["_isNestedKey"]])) unname(lapply(tr$key, function(x) I(as.character(x))))
              else I(as.character(tr$key))
    tr
  })
  p
}

lp_is_colorbar <- function(tr) inherits(tr, "plotly_colorbar")

lp_has_legend <- function(p) {
  any(vapply(p$x$data, function(tr) !isFALSE(lp_remove_class(tr$showlegend %||l% TRUE, "plotly_default")), logical(1))) &&
    isTRUE(lp_remove_class(p$x$layout$showlegend %|D|% TRUE, "plotly_default"))
}

lp_verify_guides <- function(p) {
  visible_bar <- function(tr) lp_is_colorbar(tr) && !isFALSE(lp_remove_class(tr$showscale %||l% TRUE, "plotly_default"))
  is_bar <- vapply(p$x$data, visible_bar, logical(1))
  if (any(is_bar) && lp_has_legend(p) && length(p$x$data) <= 2) p$x$layout$showlegend <- lp_default(FALSE)
  n_guides <- sum(is_bar) + lp_has_legend(p)
  if (n_guides > 1) {
    yanchor <- lp_default("top")
    p$x$layout$legend$yanchor <- p$x$layout$legend$yanchor %|D|% yanchor
    p$x$layout$legend$y <- p$x$layout$legend[["y"]] %|D|% lp_default(1 - ((n_guides - 1) / n_guides))
    idx <- which(is_bar)
    for (i in seq_along(idx)) {
      j <- idx[[i]]
      tr <- p$x$data[[j]]
      vals <- list(len = lp_default(1 / n_guides), lenmode = lp_default("fraction"),
                   y = lp_default(1 - ((i - 1) / n_guides)), yanchor = yanchor)
      if (inherits(tr, "zcolor")) {
        for (k in names(vals)) p$x$data[[j]]$colorbar[[k]] <- tr$colorbar[[k]] %|D|% vals[[k]]
      } else {
        for (k in names(vals)) p$x$data[[j]]$marker$colorbar[[k]] <- tr$marker$colorbar[[k]] %|D|% vals[[k]]
      }
    }
  }
  p
}

# Non-array attributes are reduced to their unique value; data arrays are
# kept as arrays (legacy verify_attr_spec()).
lp_verify_attr_spec <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    type <- tr$type %||l% "scatter"
    lp_verify_attr(tr, type)
  })
  p
}

# Validator of attribute `a` of the object at `key` (subplot ids like xaxis2
# fall back to xaxis), memoised in LP_SCHEMA_CACHE.
lp_attr_entry <- function(key, a) {
  path <- paste0(key, ".", a)
  e <- LP_SCHEMA_CACHE[[path]]
  if (is.null(e)) {
    e <- get_validator(path) %||l% get_validator(paste0(key, ".", sub("[0-9]+$", "", a))) %||% FALSE
    assign(path, e, envir = LP_SCHEMA_CACHE)
  }
  if (isFALSE(e)) NULL else e
}

lp_verify_attr <- function(obj, key) {
  for (a in names(obj)) {
    entry <- lp_attr_entry(key, a)
    if (is.null(entry)) {
      # values the color/size mappings set that the trace type doesn't have
      # (e.g. marker.color of a pie): plotly.js ignored them
      parent <- get_validator(key)
      if (!is.null(parent$children) && !a %in% parent$children && lp_all_generated(obj[[a]])) obj[[a]] <- NULL
      next
    }
    if (lp_is_default(obj[[a]]) && !length(obj[[a]])) {
      obj[[a]] <- NULL
      next
    }
    is_data <- identical(entry$superclass, "DataArrayValidator")
    is_compound <- entry$superclass %in% c("CompoundValidator", "TitleValidator", "CompoundArrayValidator")
    if (!is_data && !isTRUE(entry$array_ok) && !is_compound && is.atomic(obj[[a]])) {
      cls <- oldClass(obj[[a]])
      v <- lp_uniq(obj[[a]])
      oldClass(v) <- cls
      obj[[a]] <- v
    }
    if (identical(obj[["hoveron"]], "fills") && !is.null(obj[["text"]])) obj[["text"]] <- paste(lp_uniq(obj[["text"]]), collapse = "\n")
    if (is_data && is.atomic(obj[[a]])) obj[[a]] <- lp_i(obj[[a]])
    if (a == "marker" && lp_is_default(obj$marker$size) && length(obj$marker$size) == 1) {
      n <- length(obj[["x"]] %||l% obj[["y"]] %||l% obj[["lat"]] %||l% obj[["lon"]])
      if (n > 1) obj$marker$size <- lp_default(rep(obj$marker$size, n))
    }
    if (identical(entry$superclass, "CompoundValidator") && is.list(obj[[a]])) {
      obj[[a]] <- lp_verify_attr(obj[[a]], paste(key, a, sep = "."))
    }
  }
  obj
}

# Animation ------------------------------------------------------------------------------------

lp_register_frames <- function(p, frame_mapping = NULL) {
  p$x$data <- lapply(p$x$data, function(tr) {
    tr[["frame"]] <- tr[["frame"]][[1]] %||l% NA
    tr
  })
  frame_attrs <- unlist(lapply(p$x$data, `[[`, "frame"))
  frame_names <- lp_get_levels(frame_attrs)
  frame_names <- frame_names[!is.na(frame_names)]
  p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- as.character(tr$frame); tr })
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (length(tr[["name"]]) != 1) {
      p$x$data[[i]]$frameOrder <- NULL
      next
    }
    nms <- strsplit(as.character(tr[["name"]]), lp_br(), fixed = TRUE)[[1]]
    idx <- setdiff(seq_along(nms), tr$frameOrder %||l% 0)
    p$x$data[[i]]["name"] <- list(if (length(idx)) paste(nms[idx], collapse = lp_br()) else NULL)
    p$x$data[[i]]$frameOrder <- NULL
  }
  if (length(frame_names) < 2) {
    p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- NULL; tr })
    return(p)
  }
  for (ax in c("x", "y")) {
    v <- unlist(lapply(p$x$data, function(tr) tr[[ax]]))
    if (is.numeric(v)) {
      rng <- range(v, na.rm = TRUE)
      name <- paste0(ax, "axis")
      if (identical(p$x$layout[[name]]$type, "log")) {
        rng <- log10(rng)
        rng[is.nan(rng)] <- 0
      }
      p$x$layout[[name]]$range <- p$x$layout[[name]]$range %||l% grDevices::extendrange(rng)
    }
  }
  frame_trace_names <- unique(unlist(lapply(p$x$data[!is.na(frame_attrs)], `[[`, "name")))
  p$x$frames <- lapply(seq_along(frame_names), function(i) {
    nm <- frame_names[[i]]
    d <- p$x$data[vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% nm), logical(1))]
    d <- lapply(d, function(tr) { tr$visible <- tr$visible %||l% TRUE; tr })
    for (j in setdiff(frame_trace_names, unlist(lapply(d, `[[`, "name")))) {
      idx <- which(vapply(p$x$data, function(tr) isTRUE(tr[["name"]] == j), logical(1)))
      if (length(idx)) d <- c(d, list(lp_modify_list(p$x$data[[idx[[1]]]], list(visible = FALSE))))
    }
    list(name = as.character(format(nm)), data = lapply(d, function(tr) lp_verify_attr(tr, tr$type %||l% "scatter")))
  })
  first <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frame_names[[1]]), logical(1))
  p$x$data[first] <- p$x$frames[[1]]$data
  rest <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frame_names[-1]), logical(1))
  p$x$data[rest] <- NULL
  traces_idx <- which(!is.na(vapply(p$x$data, function(tr) tr[["frame"]] %||l% NA_character_, "")))
  p$x$frames <- lapply(p$x$frames, function(f) { f$traces <- I(traces_idx - 1L); f })
  cw <- lp_colorway(p)
  p$x$data <- lp_colorway_retrain(p$x$data, rep(cw, length.out = length(p$x$data)))
  p$x$frames <- lapply(p$x$frames, function(f) {
    f$data <- lp_colorway_retrain(f$data, rep(cw, length.out = max(f$traces) + 1L)[f$traces + 1L])
    f
  })
  p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- NULL; tr })
  p$x$frames <- lapply(p$x$frames, function(f) { f$data <- lapply(f$data, function(tr) { tr$frame <- NULL; tr }); f })

  current <- if (length(frame_mapping) == 1) {
    list(prefix = paste0(frame_mapping, ": "), xanchor = "right", font = list(size = 16, color = toRGB("gray80")))
  }
  p$animation <- p$animation %||l% lp_animation_defaults()
  types <- unique(unlist(lapply(p$x$frames, function(f) vapply(f$data, function(tr) tr$type %||l% "scatter", ""))))
  if (identical(types, "scatter") && lp_is_default(p$animation$frame$redraw)) p$animation$frame$redraw <- lp_default(FALSE)
  lp_animation_button_supply(lp_animation_slider_supply(p, currentvalue = current))
}

# plotly.js 3 removed the Mapbox-based traces and layout.mapbox (replaced by
# the MapLibre-based "map" ones, which need no access token) and heatmapgl
# (use heatmap). Legacy code using them is upgraded.
LP_REMOVED_TYPES <- c(scattermapbox = "scattermap", choroplethmapbox = "choroplethmap",
                      densitymapbox = "densitymap", heatmapgl = "heatmap")
# Mapbox styles that need a Mapbox token, or are gone (Stamen), as their
# closest MapLibre style
LP_MAPBOX_STYLES <- c("stamen-terrain" = "outdoors", "stamen-toner" = "light",
                      "stamen-watercolor" = "outdoors", "satellite-streets" = "satellite-streets")

lp_upgrade_type <- function(type) {
  if (is.character(type) && length(type) == 1 && type %in% names(LP_REMOVED_TYPES)) LP_REMOVED_TYPES[[type]] else type
}

lp_upgrade_trace <- function(tr) {
  tr[["type"]] <- lp_upgrade_type(tr[["type"]])
  sp <- tr[["subplot"]]
  if (is.character(sp) && length(sp) == 1) tr[["subplot"]] <- sub("^mapbox", "map", sp)
  tr
}

lp_upgrade_map_layout <- function(layout) {
  nms <- names(layout)
  if (is.null(nms)) return(layout)
  i <- grep("^mapbox[0-9]*$", nms)
  for (j in i) {
    m <- layout[[j]]
    if (is.list(m)) {
      m$accesstoken <- NULL
      st <- m$style
      if (is.character(st) && length(st) == 1) {
        if (st %in% names(LP_MAPBOX_STYLES)) m$style <- LP_MAPBOX_STYLES[[st]]
        else if (grepl("^mapbox://", st)) m$style <- "basic"
      }
      layout[[j]] <- m
    }
    new <- sub("^mapbox", "map", nms[[j]])
    if (is.null(layout[[new]])) {
      names(layout)[j] <- new
    } else {
      # settings of both: the ones given as mapbox win
      layout[[new]] <- lp_modify_list(layout[[new]], if (is.list(m)) m else list())
      layout[j] <- list(NULL)
    }
  }
  layout[!vapply(layout, is.null, logical(1))]
}

lp_upgrade_removed <- function(p) {
  # as plotly 4.x, warn about a top-level titlefont (which is then upgraded)
  if ("titlefont" %in% c(names(p$x$layout), unlist(lapply(p$x$data, names)))) {
    warning("The titlefont attribute is deprecated. Use title = list(font = ...) instead.", call. = FALSE)
  }
  p$x$data <- lp_upgrade_titles(lapply(p$x$data, lp_upgrade_trace))
  p$x$layout <- lp_upgrade_titles(lp_upgrade_map_layout(p$x$layout))
  if (length(p$x$frames)) {
    p$x$frames <- lapply(p$x$frames, function(f) {
      if (is.list(f) && length(f$data)) f$data <- lp_upgrade_titles(lapply(f$data, lp_upgrade_trace))
      if (is.list(f) && length(f$layout)) f$layout <- lp_upgrade_titles(lp_upgrade_map_layout(f$layout))
      f
    })
  }
  p$x$config$mapboxAccessToken <- NULL
  p
}

# Lists with repeated names (e.g. list(showland = TRUE, ..., showland = FALSE))
# keep the last value, as plotly.js reading the JSON did.
# Data frames in attribute values (e.g. a colorscale given as a data frame
# of values and colors) become lists of rows, as the legacy package's JSON
# (jsonlite's dataframe = "values") wrote them.
lp_dedupe_names <- function(x) {
  if (is.data.frame(x)) return(lp_df_rows(x))
  if (!is.list(x)) return(x)
  nms <- names(x)
  if (!is.null(nms)) {
    named <- nzchar(nms)
    dup <- named & duplicated(nms, fromLast = TRUE)
    if (any(dup)) {
      attrs <- attributes(x)
      x <- x[!dup]
      for (a in setdiff(names(attrs), c("names", "row.names"))) attr(x, a) <- attrs[[a]]
    }
  }
  if (length(x)) x[] <- lapply(x, lp_dedupe_names)
  x
}

lp_list_plot <- function(fig) {
  p <- plot_ly()
  p$x$attrs <- list()
  p$x$data <- unname(fig$data)
  # plotly.js's default margins, as the legacy package's as_widget() of a list
  p$x$layout$margin <- NULL
  p$x$layout <- lp_modify_list(p$x$layout, fig$layout)
  if (!is.null(fig$frames)) p$x$frames <- fig$frames
  if (!is.null(fig$config)) p$x$config <- lp_modify_list(p$x$config, fig$config)
  p
}

lp_df_rows <- function(df) {
  cols <- lapply(unname(as.list(df)), function(v) if (is.factor(v)) as.character(v) else v)
  if (!length(cols) || !nrow(df)) return(list())
  do.call(Map, c(list(f = function(...) list(...)), cols))
}

# Legacy polar charts (scatter traces with r and t, removed in plotly.js 2)
# as scatterpolar traces: angles t in degrees running clockwise from 0 at the
# right, the whole chart turned by layout.orientation, and plot_bgcolor as
# the background.
lp_upgrade_legacy_polar <- function(p) {
  polar <- FALSE
  p$x$data <- lapply(p$x$data, function(tr) {
    type <- tr[["type"]] %||l% "scatter"
    if (type %in% c("scatter", "scattergl") && !is.null(tr[["r"]]) && !is.null(tr[["t"]])) {
      polar <<- TRUE
      tr[["type"]] <- "scatterpolar"
      tr[["theta"]] <- tr[["t"]]
      tr[c("t", "x", "y", "xaxis", "yaxis")] <- NULL
    }
    tr
  })
  if (!polar) return(p)
  l <- p$x$layout
  pol <- l$polar %||l% list()
  pol$angularaxis <- lp_modify_list(list(direction = "clockwise", rotation = -(l$orientation %||l% 0)),
                                    lp_modify_list(l$angularaxis, pol$angularaxis))
  pol$radialaxis <- lp_modify_list(l$radialaxis, pol$radialaxis)
  if (!length(pol$radialaxis)) pol$radialaxis <- NULL
  if (!is.null(l$plot_bgcolor) && is.null(pol$bgcolor)) pol$bgcolor <- l$plot_bgcolor
  l[c("orientation", "angularaxis", "radialaxis")] <- NULL
  l$polar <- pol
  p$x$layout <- l
  p
}

# titlefont, titleside and titleposition (aliases plotly.js 3 removed) as
# title.font, title.side and title.position, in any object (axes, colorbars,
# the layout, legends).
LP_TITLE_ALIASES <- c(titlefont = "font", titleside = "side", titleposition = "position")

lp_upgrade_titles <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  nms <- names(x)
  if (!is.null(nms) && any(names(LP_TITLE_ALIASES) %in% nms)) {
    t <- x[["title"]]
    t <- if (is.null(t)) list() else if (is.list(t)) t else list(text = t)
    for (a in intersect(names(LP_TITLE_ALIASES), nms)) {
      k <- LP_TITLE_ALIASES[[a]]
      t[[k]] <- if (is.list(x[[a]]) && is.list(t[[k]])) lp_modify_list(x[[a]], t[[k]]) else t[[k]] %||l% x[[a]]
      x[[a]] <- NULL
    }
    x[["title"]] <- t
  }
  if (length(x)) x[] <- lapply(x, lp_upgrade_titles)
  x
}

# A value set by the plot_ly() mappings rather than given (defaults and
# colorway colors, at any depth).
lp_all_generated <- function(x) {
  if (lp_is_default(x) || inherits(x, "colorway")) return(TRUE)
  is.list(x) && length(x) > 0 && all(vapply(x, lp_all_generated, logical(1)))
}
