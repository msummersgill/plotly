#' 'Build' (i.e., evaluate) a plotly object
#'
#' This generic function creates the list object sent to plotly.js
#' for rendering. Using this function can be useful for overriding defaults
#' provided by `ggplotly`/`plot_ly` or for debugging rendering
#' errors.
#'
#' @param p a ggplot object, or a plotly object, or a list.
#' @param registerFrames should a frame trace attribute be interpreted as frames in an animation?
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#'
#' p <- plot_ly(economics, x = ~date, y = ~pce)
#' # the unevaluated plotly object
#' str(p)
#' # the evaluated data
#' str(plotly_build(p)$x$data)
#'
plotly_build <- function(p, registerFrames = TRUE) {
  if (inherits(p, "go_figure")) return(plotly_widget(p))
  if (is.null(p)) return(htmltools::browsable(htmltools::div()))
  if (inherits(p, c("gg", "ggmatrix"))) return(plotly_build(ggplotly(p), registerFrames = registerFrames))
  # a figure given as a list (data, layout, ...), as the legacy package's
  # plotly_build.list()
  if (is.list(p) && !is.object(p) && any(c("data", "layout") %in% names(p))) p <- list_plot(p)
  if (!is_legacy_plot(p)) stop("Don't know how to build an object of class ", class(p)[[1L]], call. = FALSE)
  # make this plot retrievable
  set_last_plot(p)

  # layout attributes (possibly referencing data via formulas)
  layouts <- Map(function(x, id) {
    d <- plotly_data(p, id)
    x <- eval_attrs(x, d)
    n_ann <- max(lengths(x$annotations) %||l% 0)
    if (!is.null(names(x$annotations))) {
      # font is the only list object, so store it, and attach after transposing
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
  # get rid of the data -> layout mapping
  p$x$layoutAttrs <- NULL
  
  # accumulate, rather than override, annotations.
  annotations <- Reduce(c, c(
    list(p$x$layout$annotations),
    setNames(compact(lapply(layouts, "[[", "annotations")), NULL)
  ))
  
  # merge layouts into a single layout (more recent layouts will override older ones)
  p$x$layout <- modify_list(p$x$layout, Reduce(modify_list, layouts))
  p$x$layout$annotations <- annotations
  
  # keep frame mapping for populating layout.slider.currentvalue in animations
  frameMapping <- unique(unlist(
    lapply(p$x$attrs, function(x) deparse2(x[["frame"]])),
    use.names = FALSE
  ))
  if (length(frameMapping) > 1) {
    warning("Only one `frame` variable is allowed", call. = FALSE)
  }
  
  # Attributes should be NULL if none exist (rather than an empty list)
  if (length(p$x$attrs) == 0) p$x$attrs <- NULL
  # If there is just one (unevaluated) trace, and the data is sf, add an sf layer
  if (length(p$x$attrs) == 1 && !inherits(p$x$attrs[[1]], "plotly_eval") && inherits(plotly_data(p), "sf")) {
    p <- add_sf(p)
  }
  
  # If type was not specified in plot_ly(), it doesn't create a trace unless
  # there are no other traces
  if (is.null(p$x$attrs[[1]][["type"]]) && length(p$x$attrs) > 1) {
    p$x$attrs[[1]] <- NULL
  }
  
  # have the attributes already been evaluated?
  is.evaled <- function(x) inherits(x, "plotly_eval")
  attrsToEval <- p$x$attrs[!vapply(p$x$attrs, is.evaled, logical(1))]
  attrsToEval <- lapply(attrsToEval, function(tr) {
    if (!is.null(tr[["type"]])) tr[["type"]] <- upgrade_type(tr[["type"]])
    tr
  })
  if (is_geo(p)) {
    attrsToEval <- lapply(attrsToEval, function(tr) {
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

  dats <- Map(function(x, id) build_trace(p, x, id), attrsToEval, names(attrsToEval) %||l% rep("", length(attrsToEval)))
  # axis titles collected while building
  for (d in dats) for (ax in names(attr(d, "titles"))) {
    path <- attr(d, "titles")[[ax]]
    if (length(path) == 2L) {
      p$x$layout[[path[[1]]]][[path[[2]]]]$title <- p$x$layout[[path[[1]]]][[path[[2]]]]$title %||l% default(ax)
    } else {
      p$x$layout[[path]]$title <- p$x$layout[[path]]$title %||l% default(ax)
    }
  }
  p$x$attrs <- lapply(p$x$attrs, function(x) structure(x, class = "plotly_eval"))
  
  # traceify by the interaction of discrete variables
  traces <- list()
  for (i in seq_along(dats)) {
    d <- dats[[i]]
    scaleAttrs <- names(d) %in% paste0(npscales(), "s")
    traces <- c(traces, traceify(d[!scaleAttrs], d$.plotlyTraceIndex))
    if (i == 1) traces[[1]] <- c(traces[[1]], d[scaleAttrs])
  }
  
  # insert NAs to differentiate groups
  traces <- lapply(traces, function(x) {
    for (v in intersect(x$.plotlyVariableMapping, LP_NPSCALES)) {
      if (!is.null(x[[v]])) x[[v]] <- structure(uniq(x[[v]]), class = oldClass(x[[v]]))
    }
    x
  })
  
  # Map special plot_ly() arguments to plotly.js trace attributes.
  # Note that symbol/linetype can modify the mode, so those are applied first
  # TODO: use 'legends 2.0' to create legends for these discrete mappings
  # https://github.com/plotly/plotly.js/issues/1668
  if (length(traces)) {
    traces <- map_symbol(traces)
    traces <- map_linetype(traces)
    traces <- map_size(traces)
    traces <- map_size(traces, stroke = TRUE) #i.e., span
    colorTitle <- unlist(lapply(p$x$attrs, function(x) { deparse2(x[["color"]] %||% x[["z"]]) }))
    strokeTitle <- unlist(lapply(p$x$attrs, function(x) deparse2(x[["stroke"]])))
    traces <- map_color(traces, title = paste(colorTitle, collapse = br()), colorway = colorway(p))
    traces <- map_color(traces, stroke = TRUE, title = paste(strokeTitle, collapse = br()), colorway = colorway(p))
  }
  mapping_attrs <- c("alpha", "alpha_stroke", LP_NPSCALES, paste0(LP_NPSCALES, "s"), ".plotlyGroupIndex",
                     ".plotlyMissingIndex", ".plotlyTraceIndex", ".plotlyVariableMapping", "inherit")
  traces <- lapply(traces, function(tr) {
    for (j in mapping_attrs) tr[[j]] <- NULL
    tr
  })
  p$x$data <- unname(c(p$x$data, traces))
  p <- apply_transforms(p)
  p <- upgrade_legacy_polar(p)
  # supply trace anchor and domain information
  p <- supply_defaults(p)
  p$x$nullDomain <- NULL
  p <- upgrade_removed(p)
  p$x$layout <- dedupe_names(p$x$layout)
  p$x$data <- dedupe_names(p$x$data)
  # attribute naming corrections for "geo-like" traces
  p <- cartesian2geo(p)
  
  # Compute sensible bounding boxes for each mapbox/geo subplot
  p <- fit_bounds(p)
  
  # polar charts don't like null width/height keys
  if (is.null(p$x$layout[["height"]])) p$x$layout[["height"]] <- NULL
  if (is.null(p$x$layout[["width"]])) p$x$layout[["width"]] <- NULL
  
  # ensure we get the order of categories correct
  # (plotly.js uses the order in which categories appear by default)
  p <- populate_categorical_axes(p)
  # translate '\n' to '<br />' in text strings
  p <- translate_linebreaks(p)
  # if it makes sense, add markers/lines/text to mode
  p <- verify_mode(p)
  # annotations & shapes must be an array of objects
  # TODO: should we add anything else to this?
  p <- verify_arrays(p)
  # set a sensible hovermode if it hasn't been specified already
  p <- verify_hovermode(p)
  # try to convert to webgl if toWebGl was used
  p <- verify_webgl(p)
  p <- supply_highlight_attrs(p)
  # crosstalk dynamically adds traces, meaning that a legend could be dynamically
  # added, which is confusing. So here we populate a sensible default.
  p <- verify_showlegend(p)
  p$x$shinyEvents <- p$x$shinyEvents %||l% default_events()
  # NOTE: this needs to occur *before* registering frames so simple/nested key
  # flags get passed onto frame data.
  p <- verify_key_type(p)
  p <- verify_config(p)
  if (registerFrames) {
    p <- registerFrames(p, frameMapping = frameMapping)
  }
  p <- verify_guides(p)
  # box up 'data_array' attributes where appropriate
  p <- verify_attr_spec(p)
  p
}

is_geo <- function(p) !is.null(p$x$layout$mapType)

# Evaluate one trace's attributes and train its data (legacy plotly_build's
# per-trace step).
build_trace <- function(p, x, id) {
  
  # grab the data for this trace
  dat <- plotly_data(p, id)
  
  # formula/symbol/attribute evaluation
  trace <- structure(eval_attrs(x, dat), class = oldClass(x))
  
  # determine trace type (if not specified, can depend on the # of data points)
  # note that this should also determine a sensible mode, if appropriate
  trace <- verify_type(trace)
  
  # verify orientation of boxes/bars
  trace <- verify_orientation(trace)
  
  # supply sensible defaults based on trace type
  trace <- coerce_attr_defaults(trace, p$x$layout)
  
  # attach crosstalk info, if necessary
  if (LP_CT_KEY %in% names(dat) && isTRUE(trace[["inherit"]] %||l% TRUE)) {
    trace[["key"]] <- trace[["key"]] %||l% dat[[LP_CT_KEY]]
    trace[["set"]] <- trace[["set"]] %||l% attr(dat, "set")
  }
  
  # if appropriate, tack on a group index
  grps <- if (has_group(trace)) data_groups(dat)
  if (length(grps) && any(lengths(trace) == NROW(dat))) {
    trace[[".plotlyGroupIndex"]] <- group_index(lapply(grps, function(g) dat[[g]]))
  }
  
  # add sensible axis names to layout
  titles <- list()
  for (ax in c("x", "y", "z")) {
    if (sum(names(trace) %in% ax) == 1) {
      title <- deparse2(x[[ax]])
      if (!is.null(title)) {
        nm <- paste0(ax, "axis")
        titles[[title]] <- if (is3d(trace$type) || ax == "z") c("scene", nm) else nm
      }
    }
  }
  
  if (inherits(trace, c("plotly_surface", "plotly_contour"))) {
    # TODO: generate matrix for users?
    # (1) if z is vector, and x/y are null throw error
    # (2) if x/y/z are vectors and length(x) * length(y) == length(z), convert z to matrix
    if (!is.matrix(trace[["z"]]) || !is.numeric(trace[["z"]])) {
      stop("`z` must be a numeric matrix", call. = FALSE)
    }
  }
  
  entry <- get_validator(trace[["type"]])
  array_attrs <- if (is_tidy(trace)) {
    Filter(function(ch) {
      v <- get_validator(paste(trace[["type"]], ch, sep = "."))
      identical(v$superclass, "DataArrayValidator") || isTRUE(v$array_ok)
    }, entry$children)
  }
  
  # collect non-positional scales, plotly.js data_arrays, and "special"
  # array attributes for "data training"
  all_attrs <- c(array_attrs, special_attrs(trace), LP_NPSCALES, "frame", ".plotlyGroupIndex", "text",
                 "key", "fillcolor", "name", "legendgroup")
  tr <- trace[names(trace) %in% all_attrs]
  # vectors, and unnamed lists with a value per point (e.g. customdata given
  # as a list-column), which are sorted and split into groups with the data
  per_point <- function(v) is.null(dim(v)) && (is.atomic(v) || (is.list(v) && !is.data.frame(v) && is.null(names(v)) && length(v) > 1))
  # TODO: does it make sense to "train" matrices/2D-tables (e.g. z)?
  tr <- tr[vapply(tr, per_point, logical(1))]
  n <- max(c(0L, lengths(tr)))
  tr <- tr[lengths(tr) %in% c(1L, n) | vapply(tr, is.atomic, logical(1))]
  built <- if (n > 0) {
    data.table::as.data.table(lapply(tr, function(v) if (length(v) == 1L) rep(v, n) else v))
  } else data.table::data.table()
  # avoid clobbering I() (i.e., variables that shouldn't be scaled)
  for (k in names(tr)) if (inherits(tr[[k]], "AsIs")) data.table::set(built, j = k, value = I(built[[k]]))
  if (NROW(built) > 0) {
    # Build the index used to split one "trace" into multiple traces
    is_asis <- vapply(built, function(v) inherits(v, "AsIs"), logical(1))
    is_disc <- vapply(built, is_discrete, logical(1))
    # note: can only have one linetype per trace
    is_split <- names(built) %in% c("split", "linetype", "frame", "fillcolor", "name") |
      (!is_asis & is_disc & names(built) %in% c("symbol", "color"))
    if (any(is_split)) {
      paste2 <- function(a, b) if (identical(a, b)) a else paste(a, b, sep = br())
      split_vars <- as.list(built)[is_split]
      data.table::set(built, j = ".plotlyTraceIndex", value = Reduce(paste2, split_vars))
      # in registerFrames() we need to strip the frame from .plotlyTraceIndex
      # so keep track of which variable it is...
      trace$frameOrder <- which(names(split_vars) %in% "frame")
    }
    # Build the index used to determine grouping (later on, NAs are inserted
    # via group2NA() to create the groups). This is done in 3 parts:
    # 1. Sort data by the trace index since groups are nested within traces.
    # 2. Translate missing values on positional scales to a grouping variable.
    #    If grouping isn't relevant for this trace, a warning is thrown since
    #    NAs are removed.
    # 3. The grouping from (2) and any groups detected via dplyr::groups()
    #    are combined into a single grouping variable, .plotlyGroupIndex
    built <- arrange_safe(built, ".plotlyTraceIndex")
    pos <- intersect(c("x", "y", "z"), names(built))
    complete <- if (length(pos)) !Reduce(`|`, lapply(pos, function(k) is.na(built[[k]]))) else rep(TRUE, nrow(built))
    # warn about missing values if groups aren't relevant for this trace type
    if (any(!complete) && !has_group(trace)) {
      warning("Ignoring ", sum(!complete), " observations", call. = FALSE)
    }
    data.table::set(built, j = ".plotlyMissingIndex", value = cumsum(!complete))
    if (!all(complete)) built <- built[complete]
    if (length(grps) && has_group(trace) && isTRUE(trace[["connectgaps"]])) {
      stop("Can't use connectgaps=TRUE when data has group(s).", call. = FALSE)
    }
    data.table::set(built, j = ".plotlyGroupIndex", value = group_index(list(
      built[[".plotlyGroupIndex"]] %||l% rep("", nrow(built)), built[[".plotlyMissingIndex"]])))
    built <- arrange_safe(built, c(".plotlyTraceIndex", ".plotlyGroupIndex", if (inherits(trace, "plotly_line")) "x"))
    built <- train_data(built, trace)
    # Separate groups with missing values for all of the layer's traces at
    # once: nesting in the trace index gives the same result as the legacy
    # package's group2NA() call on each trace after splitting. That call sees
    # traces whose class was dropped by `[`, so only has_group()'s
    # type/mode test applies (no `ordered`, no `retrace.first`).
    if (has_group(unclass(trace))) {
      built <- group2NA(built, ".plotlyGroupIndex",
                        nested = if (".plotlyTraceIndex" %in% names(built)) ".plotlyTraceIndex")
    }
    trace[[".plotlyVariableMapping"]] <- names(built)
    # copy over to the trace data
    for (k in names(built)) {
      trace[[k]] <- built[[k]]
    }
  }
  # TODO: provide a better way to clean up "high-level" attrs
  trace[c("ymin", "ymax", "yend", "xend")] <- NULL
  out <- trace[lengths(trace) > 0]
  oldClass(out) <- oldClass(trace)
  attr(out, "titles") <- titles
  out
}

# dplyr::arrange() replacement for a data.table, by reference (stable; factors
# by level, strings in C locale, missing values last).
arrange_safe <- function(d, vars) {
  vars <- vars[vars %in% names(d)]
  if (length(vars)) data.table::setorderv(d, cols = vars, na.last = TRUE)
  d
}

# Group ids ordered and grouped like interaction(...) codes (the first
# variable varies fastest; factors by level, other values as factor() sorts
# them), without building interaction()'s levels, which multiply.
group_index <- function(vars) {
  codes <- lapply(vars, function(v) {
    if (is.factor(v)) as.integer(v) else match(v, sort(unique(v)))
  })
  data.table::frankv(rev(codes), ties.method = "dense", na.last = "keep")
}

# is a given trace type 3d?
is3d <- function(type = NULL) {
  type <- type %||% "scatter"
  type %in% c("mesh3d", "scatter3d", "surface")
}

# ----------------------------------------------------------------
# Functions used solely within plotly_build
# ----------------------------------------------------------------

registerFrames <- function(p, frameMapping = NULL) {
  p$x$data <- lapply(p$x$data, function(tr) {
    tr[["frame"]] <- tr[["frame"]][[1]] %||l% NA
    tr
  })
  frameAttrs <- unlist(lapply(p$x$data, `[[`, "frame"))
  # NOTE: getLevels() should drop NAs
  frameNames <- getLevels(frameAttrs)
  frameNames <- frameNames[!is.na(frameNames)]
  p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- as.character(tr$frame); tr })
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]
    if (length(tr[["name"]]) != 1) {
      p$x$data[[i]]$frameOrder <- NULL
      next
    } 
    nms <- strsplit(as.character(tr[["name"]]), br(), fixed = TRUE)[[1]]
    idx <- setdiff(seq_along(nms), tr$frameOrder %||l% 0)
    p$x$data[[i]]["name"] <- list(if (length(idx)) paste(nms[idx], collapse = br()) else NULL)
    p$x$data[[i]]$frameOrder <- NULL
  }
  if (length(frameNames) < 2) {
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
  frame_trace_names <- unique(unlist(lapply(p$x$data[!is.na(frameAttrs)], `[[`, "name")))
  p$x$frames <- lapply(seq_along(frameNames), function(i) {
    nm <- frameNames[[i]]
    d <- p$x$data[vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% nm), logical(1))]
    d <- lapply(d, function(tr) { tr$visible <- tr$visible %||l% TRUE; tr })
    for (j in setdiff(frame_trace_names, unlist(lapply(d, `[[`, "name")))) {
      idx <- which(vapply(p$x$data, function(tr) isTRUE(tr[["name"]] == j), logical(1)))
      if (length(idx)) d <- c(d, list(modify_list(p$x$data[[idx[[1]]]], list(visible = FALSE))))
    }
    list(name = as.character(format(nm)), data = lapply(d, function(tr) verify_attr(tr, tr$type %||l% "scatter")))
  })
  first <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frameNames[[1]]), logical(1))
  p$x$data[first] <- p$x$frames[[1]]$data
  rest <- vapply(p$x$data, function(tr) isTRUE(tr[["frame"]] %in% frameNames[-1]), logical(1))
  p$x$data[rest] <- NULL
  traces_idx <- which(!is.na(vapply(p$x$data, function(tr) tr[["frame"]] %||l% NA_character_, "")))
  p$x$frames <- lapply(p$x$frames, function(f) { f$traces <- I(traces_idx - 1L); f })
  cw <- colorway(p)
  p$x$data <- colorway_retrain(p$x$data, rep(cw, length.out = length(p$x$data)))
  p$x$frames <- lapply(p$x$frames, function(f) {
    f$data <- colorway_retrain(f$data, rep(cw, length.out = max(f$traces) + 1L)[f$traces + 1L])
    f
  })
  p$x$data <- lapply(p$x$data, function(tr) { tr$frame <- NULL; tr })
  p$x$frames <- lapply(p$x$frames, function(f) { f$data <- lapply(f$data, function(tr) { tr$frame <- NULL; tr }); f })

  defaultvalue <- if (length(frameMapping) == 1) {
    list(prefix = paste0(frameMapping, ": "), xanchor = "right", font = list(size = 16, color = toRGB("gray80")))
  }
  p$animation <- p$animation %||l% animation_opts_defaults()
  types <- unique(unlist(lapply(p$x$frames, function(f) vapply(f$data, function(tr) tr$type %||l% "scatter", ""))))
  if (identical(types, "scatter") && is_default(p$animation$frame$redraw)) p$animation$frame$redraw <- default(FALSE)
  # _always_ display an animation button and slider by default
  animation_button_supply(
    animation_slider_supply(p, currentvalue = defaultvalue)
  )
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

upgrade_type <- function(type) {
  if (is.character(type) && length(type) == 1 && type %in% names(LP_REMOVED_TYPES)) LP_REMOVED_TYPES[[type]] else type
}

upgrade_trace <- function(tr) {
  tr[["type"]] <- upgrade_type(tr[["type"]])
  sp <- tr[["subplot"]]
  if (is.character(sp) && length(sp) == 1) tr[["subplot"]] <- sub("^mapbox", "map", sp)
  tr
}

upgrade_map_layout <- function(layout) {
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
      layout[[new]] <- modify_list(layout[[new]], if (is.list(m)) m else list())
      layout[j] <- list(NULL)
    }
  }
  layout[!vapply(layout, is.null, logical(1))]
}

upgrade_removed <- function(p) {
  # as plotly 4.x, warn about a top-level titlefont (which is then upgraded)
  if ("titlefont" %in% c(names(p$x$layout), unlist(lapply(p$x$data, names)))) {
    warning("The titlefont attribute is deprecated. Use title = list(font = ...) instead.", call. = FALSE)
  }
  p$x$data <- upgrade_titles(lapply(p$x$data, upgrade_trace))
  p$x$layout <- upgrade_titles(upgrade_map_layout(p$x$layout))
  if (length(p$x$frames)) {
    p$x$frames <- lapply(p$x$frames, function(f) {
      if (is.list(f) && length(f$data)) f$data <- upgrade_titles(lapply(f$data, upgrade_trace))
      if (is.list(f) && length(f$layout)) f$layout <- upgrade_titles(upgrade_map_layout(f$layout))
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
dedupe_names <- function(x) {
  if (is.data.frame(x)) return(df_rows(x))
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
  if (length(x)) x[] <- lapply(x, dedupe_names)
  x
}

list_plot <- function(fig) {
  p <- plot_ly()
  p$x$attrs <- list()
  p$x$data <- unname(fig$data)
  # plotly.js's default margins, as the legacy package's as_widget() of a list
  p$x$layout$margin <- NULL
  p$x$layout <- modify_list(p$x$layout, fig$layout)
  if (!is.null(fig$frames)) p$x$frames <- fig$frames
  if (!is.null(fig$config)) p$x$config <- modify_list(p$x$config, fig$config)
  p
}

df_rows <- function(df) {
  cols <- lapply(unname(as.list(df)), function(v) if (is.factor(v)) as.character(v) else v)
  if (!length(cols) || !nrow(df)) return(list())
  do.call(Map, c(list(f = function(...) list(...)), cols))
}

# Legacy polar charts (scatter traces with r and t, removed in plotly.js 2)
# as scatterpolar traces: angles t in degrees running clockwise from 0 at the
# right, the whole chart turned by layout.orientation, and plot_bgcolor as
# the background.
upgrade_legacy_polar <- function(p) {
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
  pol$angularaxis <- modify_list(list(direction = "clockwise", rotation = -(l$orientation %||l% 0)),
                                    modify_list(l$angularaxis, pol$angularaxis))
  pol$radialaxis <- modify_list(l$radialaxis, pol$radialaxis)
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

upgrade_titles <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  nms <- names(x)
  if (!is.null(nms) && any(names(LP_TITLE_ALIASES) %in% nms)) {
    t <- x[["title"]]
    t <- if (is.null(t)) list() else if (is.list(t)) t else list(text = t)
    for (a in intersect(names(LP_TITLE_ALIASES), nms)) {
      k <- LP_TITLE_ALIASES[[a]]
      t[[k]] <- if (is.list(x[[a]]) && is.list(t[[k]])) modify_list(x[[a]], t[[k]]) else t[[k]] %||l% x[[a]]
      x[[a]] <- NULL
    }
    x[["title"]] <- t
  }
  if (length(x)) x[] <- lapply(x, upgrade_titles)
  x
}

# A value set by the plot_ly() mappings rather than given (defaults and
# colorway colors, at any depth).
all_generated <- function(x) {
  if (is_default(x) || inherits(x, "colorway")) return(TRUE)
  is.list(x) && length(x) > 0 && all(vapply(x, all_generated, logical(1)))
}


train_data <- function(data, trace) {
  if (inherits(trace, "plotly_ribbon")) data <- data.table::as.data.table(ribbon_dat(as.data.frame(data)))
  if (inherits(trace, "plotly_segment")) {
    # TODO: this could be faster, more efficient
    data$.plotlyGroupIndex <- seq_len(NROW(data))
    idx <- rep(seq_len(NROW(data)), each = 2)
    keep <- setdiff(names(data), c("xend", "yend"))
    dat <- data[idx, keep, with = FALSE]
    idx2 <- seq.int(2, NROW(dat), by = 2)
    data.table::set(dat, i = idx2, j = "x", value = data[["xend"]])
    data.table::set(dat, i = idx2, j = "y", value = data[["yend"]])
    data <- dat
  }
  # TODO: a lot more geoms!!!
  data
}


map_size <- function(traces, stroke = FALSE) {
  sizeList <- lapply(traces, "[[", if (stroke) "span" else "size")
  nSizes <- lengths(sizeList)
  # if no "top-level" size is present, return traces untouched
  if (all(nSizes == 0)) return(traces)
  
  allSize <- unlist(compact(sizeList))
  if (!is.null(allSize) && is_discrete(allSize)) {
    stop("`size`/`width` values must be numeric .", call. = FALSE)
  }
  sizeRange <- range(allSize, na.rm = TRUE)
  map_fn <- function(trace, sizes) {
    type <- trace$type %||l% "scatter"
    u <- uniq(sizes)
    single <- length(u) == 1
    pick <- function(path) if (single) u else if (array_ok(type, path)) sizes
    if (stroke) {
      if (has_attr(type, "marker.line")) trace$marker$line <- modify_list(list(width = default(pick(c("marker", "line", "width")))), trace$marker$line)
      for (a in c("error_y", "error_x")) {
        if (has_attr(type, a)) trace[[a]] <- modify_list(list(thickness = default(pick(c(a, "thickness")))), trace[[a]])
      }
      
      # When fill exists, `span` controls line.width
      if (has_fill(trace) && has_attr(type, "line")) {
        s <- if (single) u else if (array_ok(type, c("line", "width"))) sizes else NA
        if (identical(s, NA)) warning("`line.width` does not currently support multiple values.", call. = FALSE)
        else trace$line <- modify_list(list(width = default(s)), trace$line)
      }
    } else {
      if (has_attr(type, "marker") && has_attr(type, "marker.size")) {
        trace$marker <- modify_list(list(size = default(pick(c("marker", "size"))), sizemode = default("area")), trace$marker)
      }
      if (has_attr(type, "textfont")) trace$textfont <- modify_list(list(size = default(pick(c("textfont", "size")))), trace$textfont)
      for (a in c("error_y", "error_x")) {
        if (has_attr(type, a)) trace[[a]] <- modify_list(list(width = default(pick(c(a, "width")))), trace[[a]])
      }
      
      # if fill does not exist, `size` controls line.width
      if (!has_fill(trace) && has_attr(type, "line")) {
        s <- if (single) u else if (array_ok(type, c("line", "width"))) sizes else NA
        if (identical(s, NA)) warning("`line.width` does not currently support multiple values.", call. = FALSE)
        else trace$line <- modify_list(list(width = default(s)), trace$line)
      }
      trace
    }
    trace
  }
  for (i in which(nSizes > 0)) {
    s <- sizeList[[i]]
    size_i <- if (inherits(s, "AsIs")) remove_class(s, "AsIs") else {
      rescale(s, to = if (stroke) traces[[1]][["spans"]] else traces[[1]][["sizes"]], from = sizeRange)
    }
    traces[[i]] <- map_fn(traces[[i]], size_i)
  }
  traces
}

color_fill <- function(trace, rgba, is_colorway = FALSE, stroke = FALSE) {
  type <- trace$type %||l% "scatter"
  u <- uniq(rgba)
  single <- length(u) == 1
  dflt <- if (is_colorway) function(x) prefix_class(default(x), "colorway") else default
  if (stroke) {
    if (has_attr(type, "marker.line")) {
      col <- if (single) u else if (array_ok(type, c("marker", "line", "color"))) rgba
      trace$marker$line <- modify_list(list(color = dflt(col)), trace$marker$line)
    }
    if (has_fill(trace)) {
      col <- if (single) u else if (array_ok(type, c("line", "color"))) rgba
      if (is.null(col)) warning("`line.color` does not currently support multiple values.", call. = FALSE)
      else trace$line <- modify_list(list(color = dflt(col)), trace$line)
    }
    return(trace)
  }
  for (a in c("marker", "textfont", "error_y", "error_x")) {
    if (!has_attr(type, a)) next
    if (is_colorway && a == "textfont") next
    col <- if (single) u else if (array_ok(type, c(a, "color"))) rgba else NA
    if (identical(col, NA)) warning("`", a, ".color` does not currently support multiple values.", call. = FALSE)
    else trace[[a]] <- modify_list(list(color = dflt(col)), trace[[a]])
  }
  if (has_fill(trace)) {
    if (!single) warning("Only one fillcolor per trace allowed", call. = FALSE)
    if (is.null(trace[["alpha"]])) u <- toRGB(u, 0.5)
    if (single) trace <- modify_list(list(fillcolor = dflt(u)), trace)
  } else if (has_attr(type, "line")) {
    col <- if (single) u else if (array_ok(type, c("line", "color"))) rgba else NA
    if (identical(col, NA)) warning("`line.color` does not currently support multiple values.", call. = FALSE)
    else trace$line <- modify_list(list(color = dflt(col)), trace$line)
  }
  trace
}

# Colorscales given in a trace (as a name, list of [value, color] pairs,
# two-column matrix or data frame, or list of values and colors) are kept: the
# color mapping only supplies a default one.
map_color <- function(traces, ...) {
  user <- lapply(traces, function(tr) {
    list(cs = colorscale_json(tr[["colorscale"]]),
         mcs = if (is.list(tr[["marker"]])) colorscale_json(tr[["marker"]][["colorscale"]]))
  })
  traces <- map_color_scale(traces, ...)
  # (the mapping may append a colorbar trace, which has no user colorscale)
  for (i in seq_along(user)) {
    u <- user[[i]]
    if (!is.null(u$cs) && !is_default(u$cs)) traces[[i]][["colorscale"]] <- u$cs
    if (!is.null(u$mcs) && !is_default(u$mcs) && is.list(traces[[i]][["marker"]])) traces[[i]][["marker"]][["colorscale"]] <- u$mcs
  }
  # an appended colorbar trace shows the (first) user colorscale
  given <- Filter(function(cs) !is.null(cs) && !is_default(cs), unlist(lapply(user, function(u) list(u$mcs, u$cs)), recursive = FALSE))
  for (i in seq_along(traces)[-seq_along(user)]) {
    if (length(given) && inherits(traces[[i]], "plotly_colorbar") && is.list(traces[[i]][["marker"]])) {
      traces[[i]][["marker"]][["colorscale"]] <- given[[1]]
    }
  }
  traces
}

# Coerce `x` into a data structure that can map to a colorscale attribute.
# Note that colorscales can either be the name of a scale (e.g., 'Rainbow') or 
# a 2D array (e.g., [[0, 'rgb(0,0,255)'], [1, 'rgb(255,0,0)']])
colorscale_json <- function(x) {
  if (!length(x)) return(x)
  if (is.character(x)) return(x)
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("A colorscale matrix requires two columns")
    x <- as.data.frame(x)
    x[, 1] <- as.numeric(x[, 1])
  }
  # ensure a list like this: list(list(0, 0.5, 1), list("red", "white", "blue"))
  # converts to the correct dimensions: [[0, 'red'], [0.5, 'white'], [1, 'blue']]
  if (is.list(x) && length(x) == 2) {
    n1 <- length(x[[1]])
    n2 <- length(x[[2]])
    if (n1 != n2 || n1 == 0 || n2 == 0) {
      warning("A colorscale list must of elements of the same (non-zero) length")
    } else if (!is.data.frame(x) && can_be_numeric(x[[1]])) {
      x <- data.frame(
        val = as.numeric(x[[1]]),
        col = as.character(x[[2]]),
        stringsAsFactors = FALSE
      )
      x <- setNames(x, NULL)
    }
  }
  x
}

can_be_numeric <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  sum(is.na(x)) == sum(is.na(xnum))
}

map_color_scale <- function(traces, stroke = FALSE, title = "", colorway, na.color = "transparent") {
  color <- if (stroke) {
    lapply(traces, function(x) x[["stroke"]] %||l% x[["color"]])
  } else {
    lapply(traces, function(x) {
      x[["color"]] %||l% if (grepl("histogram2d", x[["type"]])) c(0, 1) else if (has_attr(x[["type"]], "colorscale")) x[["surfacecolor"]] %||l% x[["z"]]
    })
  }
  alphas <- vapply(traces, function(x) (if (stroke) x$alpha_stroke else x[["alpha"]]) %||l% 1, numeric(1))
  is_const <- vapply(color, function(x) inherits(x, "AsIs") || is.null(x), logical(1))
  is_num <- vapply(color, is.numeric, logical(1)) & !is_const
  is_disc <- vapply(color, is_discrete, logical(1)) & !is_const
  if (any(is_num & is_disc)) stop("Can't have both discrete and numeric color mappings", call. = FALSE)
  single <- lengths(lapply(color, uniq)) == 1
  types <- vapply(traces, function(tr) tr$type %||l% "scatter", character(1))
  modes <- vapply(traces, function(tr) tr$mode %||l% "lines", character(1))
  is_scatter <- grepl("scatter", types)
  has_line <- ifelse(is_scatter, grepl("line", modes), has_attr(types, "line"))
  has_line_color <- vapply(types, array_ok, logical(1), path = c("line", "color"))
  has_text <- ifelse(is_scatter, grepl("text", modes), has_attr(types, "textfont"))
  has_text_color <- vapply(types, array_ok, logical(1), path = c("textfont", "color"))
  has_z <- has_attr(types, "colorscale") & !stroke &
    any(vapply(traces, function(tr) !is.null(tr[["z"]]) || grepl("histogram2d", tr[["type"]]), logical(1)))

  if (any(is_const)) {
    codes <- Map(function(a, b) a %||l% b, color, rep(colorway, length.out = length(traces)))
    codes <- Map(toRGB, codes[is_const], alphas[is_const])
    is_cw <- lengths(color[is_const]) == 0
    traces[is_const] <- Map(color_fill, traces[is_const], codes, is_cw, MoreArgs = list(stroke = stroke))
  }
  palette <- if (stroke) traces[[1]][["strokes"]] %||l% traces[[1]][["colors"]] else traces[[1]][["colors"]]
  if (any(is_disc)) {
    ordered <- all(vapply(color[is_disc], is.ordered, logical(1)))
    lvls <- getLevels(unlist(color[is_disc]))
    N <- length(lvls)
    pal <- palette %||l% if (ordered) substr(viridisLite::viridis(N), 1, 7) else RColorBrewer::brewer.pal(max(N, 3L), "Set2")
    scale <- col_factor(pal, names(pal) %||l% lvls, na.color = na.color)
    codes <- Map(function(x, a) toRGB(scale(as.character(x)), a), color[is_disc], alphas[is_disc])
    traces[is_disc] <- Map(color_fill, traces[is_disc], codes, MoreArgs = list(stroke = stroke))
  }
  if (any(is_num)) {
    pal <- palette %||l% substr(viridisLite::viridis(10), 1, 7)
    rng <- range(unlist(color[is_num]), na.rm = TRUE)
    scale <- col_numeric(pal, rng, na.color = na.color)
    vals <- if (diff(rng) > 0) seq(rng[1], rng[2], length.out = 25) else c(0, 1)
    # a two-column (value, color) matrix, as in plotly 4.x (legacy_figure() makes it a list of pairs)
    colorscale <- cbind(rescale(vals), toRGB(scale(vals), alphas[[1]]))
    obj <- list(colorbar = lapply(list(title = as.character(title), ticklen = 2), default),
                cmin = default(rng[1]), cmax = default(rng[2]),
                colorscale = default(colorscale), showscale = default(FALSE))
    for (i in which(is_num)) {
      if (has_z[[i]]) {
        o <- obj
        o[c("cmin", "cmax")] <- NULL
        o$showscale <- default(TRUE)
        traces[[i]] <- structure(modify_list(o, traces[[i]]), class = c("plotly_colorbar", "zcolor"))
        next
      }
      if (single[[i]]) {
        traces[[i]] <- color_fill(traces[[i]], toRGB(scale(uniq(color[[i]])), alphas[[i]]), stroke = stroke)
      } else {
        o <- obj
        o$color <- default(color[[i]])
        if (stroke) {
          # numeric line colors only where marker.line has a colorscale (plotly.js
          # ignored them elsewhere, e.g. the stroke of a choropleth)
          if (has_attr(traces[[i]]$type %||l% "scatter", "marker.line.colorscale")) traces[[i]]$marker$line <- modify_list(o, traces[[i]]$marker$line)
        } else {
          traces[[i]]$marker <- modify_list(o, traces[[i]]$marker)
        }
        if (has_line[[i]]) {
          if (has_line_color[[i]]) traces[[i]]$line <- modify_list(o, traces[[i]]$line)
          else warning("line.color doesn't (yet) support data arrays", call. = FALSE)
        }
        if (has_text[[i]]) {
          if (has_text_color[[i]]) traces[[i]]$textfont <- modify_list(o, traces[[i]]$textfont)
          else warning("textfont.color doesn't (yet) support data arrays", call. = FALSE)
        }
        
        # TODO: how to make the summary stat (mean) customizable?
        if (has_fill(traces[[i]])) {
          warning("Only one fillcolor per trace allowed", call. = FALSE)
          col <- toRGB(scale(mean(color[[i]], na.rm = TRUE)), alphas[[i]])
          if (is.null(traces[[i]][["alpha"]])) col <- toRGB(col, 0.5)
          traces[[i]] <- modify_list(list(fillcolor = col), traces[[i]])
        }
        
      }
    }
    if (any(has_z)) return(traces)
    if (stroke && sum(lengths(lapply(traces, `[[`, "stroke"))) == 0) return(traces)
    
    # add an "empty" trace with the colorbar
    obj$color <- rng
    obj$showscale <- default(TRUE)
    bar <- list(x = range_or_null(lapply(traces, `[[`, "x")),
                y = range_or_null(lapply(traces, `[[`, "y")),
                type = if (any(types %in% LP_GL_TYPES)) "scattergl" else "scatter",
                mode = "markers", opacity = 0, hoverinfo = "none", showlegend = FALSE, marker = obj)
    if ("scatter3d" %in% types) {
      bar$type <- "scatter3d"
      bar$z <- range_or_null(lapply(traces, `[[`, "z"))
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

supply_defaults <- function(p) {
  if (isTRUE(p$x$subplot)) return(p)
  anchors <- if (identical(p$x$layout$mapType, "geo")) c(geo = "geo") else if (identical(p$x$layout$mapType, "map")) c(subplot = "map") else c(xaxis = "x", yaxis = "y")
  p$x$data <- lapply(p$x$data, function(tr) {
    for (i in seq_along(anchors)) {
      key <- names(anchors)[[i]]
      if (!has_attr(tr[["type"]] %||l% "scatter", key)) next
      tr[[key]] <- sub("^y1$", "y", sub("^x1$", "x", tr[[key]][1])) %||l% anchors[[i]]
    }
    tr
  })
  # hack to avoid https://github.com/ropensci/plotly/issues/945
  if (is_type(p, "parcoords")) p$x$layout$margin$t <- NULL
  if (is_geo(p)) {
    p$x$layout[grepl("^[x-y]axis", names(p$x$layout))] <- NULL
    p$x$layout[[p$x$layout$mapType]] <- modify_list(list(domain = list(x = c(0, 1), y = c(0, 1))), p$x$layout[[p$x$layout$mapType]])
  } else if (!length(p$x$layout[["grid"]])) {
    types <- vapply(p$x$data, function(tr) tr[["type"]] %||l% "scatter", character(1))
    axes <- unique(unlist(lapply(types, function(t) grep("^[a-z]axis$", get_validator(t)$children, value = TRUE))))
    for (ax in axes) {
      dflt <- if (ax %in% p$x$nullDomain) list(automargin = TRUE) else list(domain = c(0, 1), automargin = TRUE)
      p$x$layout[[ax]] <- modify_list(dflt, p$x$layout[[ax]])
    }
  }
  p
}

is_type <- function(p, type) {
  types <- vapply(p$x$data, function(tr) tr[["type"]] %||% "scatter", character(1))
  all(types %in% type)
}

cartesian2geo <- function(p) {
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

# populate categorical axes using categoryorder="array" & categoryarray=[]
populate_categorical_axes <- function(p) {
  axes <- p$x$layout[grepl("^xaxis|^yaxis", names(p$x$layout))] %||%
    list(xaxis = NULL, yaxis = NULL)
  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    axisName <- names(axes)[[i]]
    axisType <- substr(axisName, 0, 1)
    # ggplotly() populates these attributes...don't want to clobber that
    if (!is.null(axis$ticktext) || !is.null(axis$tickvals)) next
    # collect all the data that goes on this axis
    d <- lapply(p$x$data, "[[", axisType)
    isOnThisAxis <- function(tr) {
      is.null(tr[["geo"]]) && sub("axis", "", axisName) %in% 
        (tr[[sub("[0-9]+", "", axisName)]] %||% axisType) &&
        # avoid reordering matrices (see #863)
        !is.matrix(tr[["z"]])
    }
    d <- d[vapply(p$x$data, isOnThisAxis, logical(1))]
    if (length(d) == 0) next
    isDiscrete <- vapply(d, is.discrete, logical(1))
    if (0 < sum(isDiscrete) & sum(isDiscrete) < length(d)) {
      warning(
        "Can't display both discrete & non-discrete data on same axis", 
        call. = FALSE
      )
      next
    }
    if (sum(isDiscrete) == 0) next
    categories <- lapply(d, getLevels)
    categories <- unique(unlist(categories))
    if (any(!vapply(d, is.factor, logical(1)))) categories <- sort(categories)
    p$x$layout[[axisName]]$type <- 
      p$x$layout[[axisName]]$type %||% "category"
    p$x$layout[[axisName]]$categoryorder <- 
      p$x$layout[[axisName]]$categoryorder %||% "array"
    p$x$layout[[axisName]]$categoryarray <- 
      p$x$layout[[axisName]]$categoryarray %||% categories
  }
  p
}

# Searches a list for character strings and translates R linebreaks to HTML 
# linebreaks (i.e., '\n' -> '<br />'). JavaScript function definitions created 
# via `htmlwidgets::JS()` are ignored
translate_linebreaks <- function(p) {
  recurse <- function(a) {
    typ <- typeof(a)
    if (typ == "list") {
      # retain the class of list elements 
      # which is important for many things, such as colorbars
      a[] <- lapply(a, recurse)
    } else if (typ == "character" && !inherits(a, "JS_EVAL")) {
      attrs <- attributes(a)
      a <- gsub("\n", br(), a, fixed = TRUE)
      attributes(a) <- attrs
    } else if (is.factor(a)) {
      levels(a) <- gsub("\n", br(), levels(a), fixed = TRUE)
    }
    a
  }
  p$x[] <- lapply(p$x, recurse)
  p
}

# if an object (e.g. trace.marker) contains a non-default attribute, it has been user-specified
user_specified <- function(obj = NULL) {
  if (!length(obj)) return(FALSE)
  !all(rapply(obj, is.default))
}

verify_mode <- function(p) {
  for (tr in seq_along(p$x$data)) {
    trace <- p$x$data[[tr]]
    if (grepl("scatter", trace$type %||% "scatter")) {
      if (user_specified(trace$marker) && !grepl("markers", trace$mode %||% "")) {
        message(
          "A marker object has been specified, but markers is not in the mode\n",
          "Adding markers to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+markers")
      }
      if (user_specified(trace$line) && !grepl("lines", trace$mode %||% "")) {
        message(
          "A line object has been specified, but lines is not in the mode\n",
          "Adding lines to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+lines")
      }
      if (user_specified(trace$textfont) && !grepl("text", trace$mode %||% "")) {
        warning(
          "A textfont object has been specified, but text is not in the mode\n",
          "Adding text to the mode..."
        )
        p$x$data[[tr]]$mode <- paste0(p$x$data[[tr]]$mode, "+text")
      }
    }
  }
  p
}

verify_arrays <- function(p) {
  for (i in c("annotations", "shapes", "images")) {
    thing <- p$x$layout[[i]]
    if (is.list(thing) && !is.null(names(thing))) {
      p$x$layout[[i]] <- list(thing)
    }
  }
  p
}

# The legacy package sets hovermode = "closest" for every plot (its
# highlight defaults include on = "plotly_click").
verify_hovermode <- function(p) {
  if (is.null(p$x$layout$hovermode)) p$x$layout$hovermode <- "closest"
  p
}

verify_webgl <- function(p) {
  # see toWebGL
  if (!isTRUE(p$x$.plotlyWebGl)) {
    return(p)
  }
  types <- vapply(p$x$data, function(x) x[["type"]][1] %||l% "scatter", character(1))
  can_gl <- vapply(paste0(types, "gl"), function(t) !is.null(get_validator(t)$trace_type), logical(1))
  already_gl <- grepl("gl$", types)
  if (any(!can_gl & !already_gl)) warning("The following traces don't have a WebGL equivalent: ", paste(which(!can_gl & !already_gl), collapse = ", "), call. = FALSE)
  for (i in which(can_gl)) {
    p$x$data[[i]]$type <- paste0(p$x$data[[i]]$type, "gl")
  }
  p
}

verify_showlegend <- function(p) {
  # this attribute should be set in hide_legend()
  # it ensures that "legend titles" go away in addition to showlegend = FALSE
  if (isTRUE(p$x$.hideLegend)) {
    p$x$layout$showlegend <- FALSE 
  }
  show <- vapply(p$x$data, function(x) x$showlegend %||% TRUE, logical(1))
  # respect only _user-specified_ defaults 
  isSinglePie <- identical("pie", unlist(lapply(p$x$data, function(tr) tr$type))) 
  p$x$layout$showlegend <- p$x$layout$showlegend %|D|%
    default(sum(show) > 1 || isTRUE(p$x$highlight$showInLegend) || isSinglePie)
  p
}

verify_key_type <- function(p) {
  keys <- lapply(p$x$data, "[[", "key")
  for (i in seq_along(keys)) {
    k <- keys[[i]]
    if (is.null(k)) next
    if ("select" %in% p$x$layout$clickmode && "plotly_click" %in% p$x$highlight$on) {
      warning(
        "`layout.clickmode` = 'select' is not designed to work well with ",
        "the R package's linking framework (i.e. crosstalk support).",
        call. = FALSE
      )
    }
    # does it *ever* make sense to have a missing key value?
    uk <- uniq(k)
    if (length(uk) == 1) {
      # i.e., the key for this trace has one value. In this case, 
      # we don't have iterate through the entire key, so instead, 
      # we provide a flag to inform client side logic to match the _entire_
      # trace if this one key value is a match
      p$x$data[[i]]$key <- uk[[1]]
      p$x$data[[i]]$`_isSimpleKey` <- TRUE
      p$x$data[[i]]$`_isNestedKey` <- FALSE
    }
    p$x$data[[i]]$`_isNestedKey` <- p$x$data[[i]]$`_isNestedKey` %||% !rlang::is_atomic(k)
    # key values should always be strings
    if (p$x$data[[i]]$`_isNestedKey`) {
      p$x$data[[i]]$key <- lapply(p$x$data[[i]]$key, function(x) I(as.character(x)))
      p$x$data[[i]]$key <- setNames(p$x$data[[i]]$key, NULL)
    } else {
      p$x$data[[i]]$key <- I(as.character(p$x$data[[i]]$key))
    }
  }
  p 
}

is.colorbar <- function(tr) {
  inherits(tr, "plotly_colorbar")
}

has_legend <- function(p) {
  showLegend <- function(tr) {
    tr$showlegend %||% TRUE
  }
  any(vapply(p$x$data, showLegend, logical(1))) && 
    isTRUE(p$x$layout$showlegend %|D|% TRUE)
}

verify_guides <- function(p) {
  
  # since colorbars are implemented as "invisible" traces, prevent a "trivial" legend
  if (has_colorbar(p) && has_legend(p) && length(p$x$data) <= 2) {
    p$x$layout$showlegend <- default(FALSE)
  }
  
  isVisibleBar <- function(tr) {
    is.colorbar(tr) && (tr$showscale %||% TRUE)
  }
  isBar <- vapply(p$x$data, isVisibleBar, logical(1))
  nGuides <- sum(isBar) + has_legend(p)
  
  if (nGuides > 1) {
    
    # place legend at bottom since its scrolly
    yanchor <- default("top")
    y <- default(1 - ((nGuides - 1) / nGuides))
    p$x$layout$legend$yanchor <- p$x$layout$legend$yanchor %|D|% yanchor
    p$x$layout$legend$y <- p$x$layout$legend[["y"]] %|D|% y
    
    # shrink/position colorbars
    idx <- which(isBar)
    for (i in seq_along(idx)) {
      len     <- default(1 / nGuides)
      lenmode <- default("fraction")
      y       <- default(1 - ((i - 1) / nGuides))
      
      j <- idx[[i]]
      tr <- p$x$data[[j]]
      if (inherits(tr, "zcolor")) {
        p$x$data[[j]]$colorbar$len <- tr$colorbar$len %|D|% len
        p$x$data[[j]]$colorbar$lenmode <- tr$colorbar$lenmode %|D|% lenmode
        p$x$data[[j]]$colorbar$y <- tr$colorbar$y %|D|% y
        p$x$data[[j]]$colorbar$yanchor <- tr$colorbar$yanchor %|D|% yanchor
      } else {
        p$x$data[[j]]$marker$colorbar$len <- tr$marker$colorbar$len %|D|% len
        p$x$data[[j]]$marker$colorbar$lenmode <- tr$marker$colorbar$lenmode %|D|% lenmode
        p$x$data[[j]]$marker$colorbar$y <- tr$marker$colorbar$y %|D|% y
        p$x$data[[j]]$marker$colorbar$yanchor <- tr$marker$colorbar$yanchor %|D|% yanchor
      }
    }
    
  }
  
  p
}

has_colorbar <- function(p) {
  isVisibleBar <- function(tr) {
    is.colorbar(tr) && isTRUE(tr$showscale %||% TRUE)
  }
  any(vapply(p$x$data, isVisibleBar, logical(1)))
}

# Non-array attributes are reduced to their unique value; data arrays are
# kept as arrays (legacy verify_attr_spec()).
verify_attr_spec <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    type <- tr$type %||l% "scatter"
    verify_attr(tr, type)
  })
  p
}

# Validator of attribute `a` of the object at `key` (subplot ids like xaxis2
# fall back to xaxis), memoised in LP_SCHEMA_CACHE.
attr_entry <- function(key, a) {
  path <- paste0(key, ".", a)
  e <- LP_SCHEMA_CACHE[[path]]
  if (is.null(e)) {
    e <- get_validator(path) %||l% get_validator(paste0(key, ".", sub("[0-9]+$", "", a))) %||% FALSE
    assign(path, e, envir = LP_SCHEMA_CACHE)
  }
  if (isFALSE(e)) NULL else e
}

verify_attr <- function(obj, key) {
  for (a in names(obj)) {
    entry <- attr_entry(key, a)
    if (is.null(entry)) {
      # values the color/size mappings set that the trace type doesn't have
      # (e.g. marker.color of a pie): plotly.js ignored them
      parent <- get_validator(key)
      if (!is.null(parent$children) && !a %in% parent$children && all_generated(obj[[a]])) obj[[a]] <- NULL
      next
    }
    if (is_default(obj[[a]]) && !length(obj[[a]])) {
      obj[[a]] <- NULL
      next
    }
    is_data <- identical(entry$superclass, "DataArrayValidator")
    is_compound <- entry$superclass %in% c("CompoundValidator", "TitleValidator", "CompoundArrayValidator")
    if (!is_data && !isTRUE(entry$array_ok) && !is_compound && is.atomic(obj[[a]])) {
      cls <- oldClass(obj[[a]])
      v <- uniq(obj[[a]])
      oldClass(v) <- cls
      obj[[a]] <- v
    }
    if (identical(obj[["hoveron"]], "fills") && !is.null(obj[["text"]])) obj[["text"]] <- paste(uniq(obj[["text"]]), collapse = "\n")
    if (is_data && is.atomic(obj[[a]])) obj[[a]] <- i(obj[[a]])
    if (a == "marker" && is_default(obj$marker$size) && length(obj$marker$size) == 1) {
      n <- length(obj[["x"]] %||l% obj[["y"]] %||l% obj[["lat"]] %||l% obj[["lon"]])
      if (n > 1) obj$marker$size <- default(rep(obj$marker$size, n))
    }
    if (identical(entry$superclass, "CompoundValidator") && is.list(obj[[a]])) {
      obj[[a]] <- verify_attr(obj[[a]], paste(key, a, sep = "."))
    }
  }
  obj
}

map_symbol <- function(traces) {
  symbolList <- lapply(traces, `[[`, "symbol")
  nSymbols <- lengths(symbolList)
  # if no "top-level" symbol is present, return traces untouched
  if (all(nSymbols == 0)) {
    return(traces)
  }
  lvls <- getLevels(unlist(compact(symbolList)))
  shapes <- c(16, 17, 15, 3, 7, 8)
  if (length(lvls) > length(shapes)) warning("The shape palette can deal with a maximum of 6 discrete values", call. = FALSE)
  pal <- stats_named(shapes[seq_along(lvls)], lvls)
  pal <- supply_palette(pal, traces[[1]][["symbols"]])
  valid <- c(as.character(get_validator("scatter.marker.symbol")$values$chr), as.character(get_validator("scatter.marker.symbol")$values$num))
  for (i in which(nSymbols > 0)) {
    s <- symbolList[[i]]
    syms <- lookup(if (inherits(s, "AsIs")) s else as.character(pal[as.character(s)]), LP_PCH)
    bad <- setdiff(syms, valid)
    if (length(bad)) warning("The following are not valid symbol codes:\n'", paste(bad, collapse = "', '"), "'", call. = FALSE)
    traces[[i]][["marker"]] <- modify_list(list(symbol = default(syms)), traces[[i]][["marker"]])
    if (!grepl("markers", traces[[i]]$mode %||l% "")) {
      message("Adding markers to mode; otherwise symbol would have no effect.")
      traces[[i]]$mode <- paste0(traces[[i]]$mode, "+markers")
    }
  }
  traces
}

map_linetype <- function(traces) {
  linetypeList <- lapply(traces, "[[", "linetype")
  nLinetypes <- lengths(linetypeList)
  # if no "top-level" linetype is present, return traces untouched
  if (all(nLinetypes == 0)) return(traces)
  lvls <- getLevels(unlist(compact(linetypeList)))
  ltys <- c("solid", "22", "42", "44", "13", "1343")
  pal <- stats_named(ltys[seq_along(lvls)], lvls)
  pal <- supply_palette(pal, traces[[1]][["linetypes"]])
  if (length(pal) > 6) warning("plotly.js only supports 6 different linetypes", call. = FALSE)
  for (i in which(nLinetypes > 0)) {
    l <- linetypeList[[i]]
    dashes <- lookup(if (inherits(l, "AsIs")) l else as.character(pal[as.character(l)]), LP_LTY)
    valid <- c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
    illegal <- setdiff(dashes, valid)
    if (length(illegal)) {
      warning("The following are not valid linetype codes:\n'", paste(illegal, collapse = "', '"), "'\n",
              "Valid linetypes include:\n'", paste(valid, collapse = "', '"), "'", call. = FALSE)
    }
    traces[[i]][["line"]] <- modify_list(
      list(dash = default(dashes)), traces[[i]][["line"]]
    )
    # ensure the mode is set so that the linetype is relevant
    if (!grepl("lines", traces[[i]]$mode %||l% "")) {
      message("Adding lines to mode; otherwise linetype would have no effect.")
      traces[[i]][["mode"]] <- paste0(traces[[i]][["mode"]], "+lines")
    }
  }
  traces
}


# break up a single trace into multiple traces according to values stored
# a particular key name
traceify <- function(dat, x = NULL) {
  if (length(x) == 0) return(list(dat))
  lvls <- if (is.factor(x)) levels(x) else unique(x)
  lvls <- lvls[lvls %in% x]
  # the order of lvls determines the order in which traces are drawn
  # for ordered factors at least, it makes sense to draw the highest level first
  # since that _should_ be the darkest color in a sequential pallette
  if (is.ordered(x)) lvls <- rev(lvls)
  n <- length(x)
  # recursively search for a non-list of appropriate length (if it is, subset it)
  recurse <- function(z, n, idx) {
    if (is.list(z)) lapply(z, recurse, n, idx) else if (length(z) == n) z[idx] else z
  }
  new_dat <- list()
  for (j in seq_along(lvls)) {
    new_dat[[j]] <- lapply(dat, function(y) recurse(y, n, x %in% lvls[j]))
    new_dat[[j]]$name <- new_dat[[j]]$name %||% lvls[j]
  }
  return(new_dat)
}

verify_type <- function(trace) {
  if (is.null(trace$type)) {
    attrs <- names(trace)
    trace$type <- if (all(c("x", "y", "z") %in% attrs)) {
      if (all(c("i", "j", "k") %in% attrs)) "mesh3d" else "scatter3d"
    } else if (all(c("x", "y") %in% attrs)) {
      xn <- !is_discrete(trace[["x"]])
      yn <- !is_discrete(trace[["y"]])
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
  if (!is.character(trace$type) || length(trace$type) != 1) {
    stop("The trace type must be a character vector of length 1.\n", 
         call. = FALSE)
  }
  if (is.null(get_validator(trace$type)$trace_type)) {
    stop("Trace type must be one of the following: \n'", paste(available_trace_types(), collapse = "', '"), "'", call. = FALSE)
  }
  if (grepl("scatter", trace$type) && is.null(trace$mode)) {
    message("No ", trace$type, " mode specifed:\n  Setting the mode to markers\n  Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode")
    trace$mode <- "markers"
  }
  trace
}

verify_orientation <- function(trace) {
  xNumeric <- !is.discrete(trace[["x"]]) && !is.null(trace[["x"]] %||% NULL)
  yNumeric <- !is.discrete(trace[["y"]]) && !is.null(trace[["y"]] %||% NULL)
  if (xNumeric && !yNumeric) {
    if (any(c("bar", "box") %in% trace[["type"]])) {
      trace$orientation <- "h"
    }
  }
  if (yNumeric && "histogram" %in% trace[["type"]]) {
    trace$orientation <- "h"
  }
  trace
}


eval_attr <- function(x, data = NULL) {
  if (rlang::is_formula(x)) rlang::eval_tidy(rlang::f_rhs(x), data = data,
                                             env = rlang::f_env(x)) else x
}


# helper functions
array_ok <- function(type, path) {
  isTRUE(entry(paste(c(type, path), collapse = "."))$array_ok)
}
has_fill <- function(trace) {
  trace_type <- trace[["type"]] %||% "scatter"
  # if trace type has fillcolor, but no fill attribute, then fill is always relevant
  has_fillcolor <- has_attr(trace_type, "fillcolor")
  has_fill <- has_attr(trace_type, "fill")
  if (has_fillcolor && !has_fill) return(TRUE)
  fill <- trace[["fill"]] %||% "none"
  if (has_fillcolor && isTRUE(fill != "none")) return(TRUE)
  FALSE
}

# ensure we've set a sensible trace defaults
# based on the trace type
coerce_attr_defaults <- function(trace, layout) {
  # if user has specified stroke, make sure the span 
  # defaults to something greater than 0 (so they can see the stroke!)
  if (length(trace[["stroke"]]) && !is.default(trace[["stroke"]])) {
    trace$span <- trace[["span"]] %||% default(I(1))
  }
  if (trace[["type"]] %in% c("sunburst", "pie", "treemap")) {
    # As of v1.46.1, paper_bgcolor defaults to '#fff' which
    # col2rgb() can't parse, but expands to '#ffffff'
    # https://stackoverflow.com/a/2899224/1583084
    bgcolor <- layout$paper_bgcolor %||% "#ffffff"
    trace$stroke <- trace[["stroke"]] %||% default(I(bgcolor))
  }
  trace
}

supply_palette <- function(default, user) {
  for (i in seq_along(user)) {
    idx <- names(user)[i] %||l% i
    default[idx] <- user[i]
  }
  default
}
