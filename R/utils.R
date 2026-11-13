# @staticimports pkg:staticimports
#  is_installed get_package_version system_file

is.plotly <- function(x) {
  inherits(x, "plotly")
}

# just like ggplot2:::is.discrete()
is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

"%||%" <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) x else y
}

"%()%" <- function(x, y) {
  if (is.function(x)) return(x())
  y
}

# kind of like %||%, but only respects user-defined defaults
# (instead of defaults provided in the build step)
"%|D|%" <- function(x, y) {
  if (!is.default(x)) x %||% y else y
}

# standard way to specify a line break
br <- function() "<br />"

is.default <- function(x) {
  inherits(x, "plotly_default")
}

is_default <- function(x) inherits(x, "plotly_default")

# The legacy package's %||%: falls back for NULL *and* zero-length values.
`%||l%` <- function(x, y) if (length(x) > 0) x else y

default <- function(x) {
  prefix_class(x %||% list(), "plotly_default")
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}

is_discrete <- function(x) is.factor(x) || is.character(x) || is.logical(x)

getLevels <- function(x) if (is.factor(x)) levels(x) else sort(unique(x))

deparse2 <- function(x) {
  if (is.null(x) || !is.language(x)) return(NULL)
  sub("^~", "", paste(deparse(x, 500L), collapse = ""))
}

modify_list <- function(x, y, ...) {
  modifyList(x %||% list(), y %||% list(), ...)
}

new_id <- function() {
  basename(tempfile(""))
}

tryNULL <- function(expr) tryCatch(expr, error = function(e) NULL)

is_tidy <- function(trace) {
  !(trace[["type"]] %||l% "scatter") %in% c("mesh3d", "heatmap", "histogram2d", "isosurface",
                                         "histogram2dcontour", "contour", "surface")
}

LP_NPSCALES <- c("color", "stroke", "symbol", "linetype", "size", "span", "split")

LP_GL_TYPES <- c("scattergl", "scatter3d", "mesh3d", "heatmapgl", "pointcloud", "parcoords", "surface")

# plotly.js's default colorway (the legacy package doesn't apply templates).
LP_COLORWAY <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
                 "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

has_group <- function(trace) {
  inherits(trace, paste0("plotly_", c("segment", "path", "line", "polygon"))) ||
    (grepl("scatter", trace[["type"]] %||l% "") && grepl("lines", trace[["mode"]] %||l% ""))
}

# currently implemented non-positional scales in plot_ly()
npscales <- function() {
  c("color", "stroke", "symbol", "linetype", "size", "span", "split")
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  if (diff(from) == 0) return(ifelse(is.na(x), NA, mean(to)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

# A palette given as RColorBrewer/viridis names or a vector of colors, as a
# function of n (scales' toPaletteFunc()).
palette_fun <- function(pal) {
  if (is.character(pal) && length(pal) == 1 && pal %in% rownames(RColorBrewer::brewer.pal.info)) {
    maxn <- RColorBrewer::brewer.pal.info[pal, "maxcolors"]
    cols <- RColorBrewer::brewer.pal(maxn, pal)
    return(function(n) if (n <= maxn) RColorBrewer::brewer.pal(max(n, 3L), pal)[seq_len(n)] else ramp(cols)(seq(0, 1, length.out = n)))
  }
  if (is.character(pal) && length(pal) == 1 && pal %in% c("viridis", "magma", "inferno", "plasma", "cividis")) {
    return(function(n) substr(viridisLite::viridis(n, option = pal), 1, 7))
  }
  ramp <- ramp(pal)
  function(n) if (n == 1) ramp(0) else ramp(seq(0, 1, length.out = n))
}

# Interpolate colors in CIE Lab space (like scales::colour_ramp()); values
# that fall exactly on a palette color return it unchanged.
ramp <- function(colors) {
  # a color interpolation function such as colorRamp() (RGB matrix, 0-255)
  # or one returning colors, of values in [0, 1] (as scales' palettes)
  if (is.function(colors)) {
    return(function(x) {
      out <- rep(NA_character_, length(x))
      ok <- !is.na(x)
      if (any(ok)) {
        res <- colors(pmin(pmax(x[ok], 0), 1))
        if (is.matrix(res)) res <- grDevices::rgb(res[, 1:3, drop = FALSE], maxColorValue = 255)
        out[ok] <- res
      }
      out
    })
  }
  f <- grDevices::colorRamp(colors, space = "Lab", interpolate = "linear")
  hex <- substr(grDevices::rgb(t(grDevices::col2rgb(colors)), maxColorValue = 255), 1, 7)
  m <- length(colors)
  function(x) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    if (any(ok)) {
      xv <- pmin(pmax(x[ok], 0), 1)
      rgbm <- f(xv)
      res <- grDevices::rgb(round(rgbm[, 1]), round(rgbm[, 2]), round(rgbm[, 3]), maxColorValue = 255)
      pos <- xv * (m - 1)
      exact <- abs(pos - round(pos)) < 1e-9
      res[exact] <- hex[round(pos[exact]) + 1]
      out[ok] <- res
    }
    out
  }
}

col_factor <- function(pal, levels, na.color = "transparent") {
  if (!is.null(names(pal))) {
    return(function(x) {
      out <- unname(pal[as.character(x)])
      out[is.na(out)] <- na.color
      out
    })
  }
  cols <- palette_fun(pal)(length(levels))
  function(x) {
    out <- cols[match(as.character(x), levels)]
    out[is.na(out)] <- na.color
    out
  }
}

col_numeric <- function(pal, domain, na.color = "transparent") {
  ramp <- if (is.character(pal) && length(pal) == 1) ramp(palette_fun(pal)(256)) else ramp(pal)
  function(x) {
    out <- ramp(rescale(x, from = domain))
    out[is.na(out)] <- na.color
    out
  }
}

# R plotting symbols/line types -> plotly.js names (legacy pch2symbol/lty2dash).
LP_PCH <- c("0" = "square-open", "1" = "circle-open", "2" = "triangle-up-open", "3" = "cross-thin-open",
            "4" = "x-thin-open", "5" = "diamond-open", "6" = "triangle-down-open", "7" = "square-x-open",
            "8" = "asterisk-open", "9" = "diamond-x-open", "10" = "circle-cross-open", "11" = "hexagram-open",
            "12" = "square-cross-open", "13" = "circle-x-open", "14" = "square-open-dot", "15" = "square",
            "16" = "circle", "17" = "triangle-up", "18" = "diamond", "19" = "circle", "20" = "circle",
            "21" = "circle", "22" = "square", "23" = "diamond", "24" = "triangle-up", "25" = "triangle-down",
            "32" = "circle", "35" = "hash-open", "42" = "asterisk-open", "43" = "cross-thin-open",
            "45" = "line-ew-open", "47" = "line-ne-open", "48" = "circle-open", "79" = "circle-open",
            "88" = "x-thin-open", "92" = "line-nw-open", "95" = "line-ew-open", "111" = "circle-open",
            "o" = "circle-open", "O" = "circle-open", "+" = "cross-thin-open")

LP_LTY <- c("0" = "none", "1" = "solid", "2" = "dash", "3" = "dot", "4" = "dashdot", "5" = "longdash",
            "6" = "longdashdot", "blank" = "none", "solid" = "solid", "dashed" = "dash", "dotted" = "dot",
            "dotdash" = "dashdot", "longdash" = "longdash", "twodash" = "longdashdot", "22" = "dash",
            "42" = "dot", "44" = "dashdot", "13" = "longdash", "1343" = "longdashdot", "73" = "dash",
            "2262" = "dashdot", "12223242" = "dashdot", "F282" = "dash", "F4448444" = "dash",
            "224282F2" = "dash", "F1" = "dash")

lookup <- function(x, table) {
  x <- as.character(x)
  idx <- x %in% names(table)
  x[idx] <- table[x[idx]]
  x
}

# A figure (or part) as plain lists and vectors.
plain_value <- function(x) {
  if (is.list(x)) {
    x <- unclass(x)
    x[] <- lapply(x, plain_value)
  } else if (inherits(x, "AsIs")) {
    # drop only AsIs: I(<POSIXct>) must stay a POSIXct
    oldClass(x) <- setdiff(oldClass(x), "AsIs")
  }
  x
}

# Set a value at a path of names, creating lists along the way (a NULL value
# is stored, not dropped).
set_path_value <- function(x, path, value) {
  if (length(path) == 1L) {
    x[path] <- list(value)
    return(x)
  }
  x[[path[[1L]]]] <- set_path_value(x[[path[[1L]]]] %||l% list(), path[-1L], value)
  x
}

stats_named <- function(x, nm) {
  names(x) <- nm
  x
}

is_legacy_plot <- function(p) inherits(p, "plotly") && inherits(p, "htmlwidget")

# Range of the values (NULL if there are none, e.g. a splom trace's x).
range_or_null <- function(v) {
  v <- unlist(v)
  if (!length(v) || all(is.na(v))) return(NULL)
  range(v, na.rm = TRUE)
}

# The events plotly.core's widget sends to Shiny by default.
default_events <- function() unclass(plotly.core::plotly_widget(list())$x$shinyEvents)

# data.table symbols used by group2NA()
utils::globalVariables(c(".I", ".N"))

colorway <- function(p = NULL) {
  lapply(as.list(p$x$layout$colorway %||l% LP_COLORWAY), function(x) structure(x, class = "colorway"))
}

# column name for crosstalk key
# TODO: make this more unique?
crosstalk_key <- function() ".crossTalkKey"

# Replace elements of a nested list
# 
# @param x a named list
# @param indicies a vector of indices. 
# A 1D list may be used to specify both numeric and non-numeric inidices
# @param val the value used to 
# @examples 
# 
# x <- list(a = 1)
# # equivalent to `x$a <- 2`
# re_place(x, "a", 2)
# 
# y <- list(a = list(list(b = 2)))
# 
# # equivalent to `y$a[[1]]$b <- 2`
# y <- re_place(y, list("a", 1, "b"), 3)
# y

has_line <- function(types, modes) {
  is_scatter <- grepl("scatter", types)
  ifelse(is_scatter, grepl("line", modes), has_attr(types, "line"))
}

has_text <- function(types, modes) {
  is_scatter <- grepl("scatter", types)
  ifelse(is_scatter, grepl("text", modes), has_attr(types, "textfont"))
}

has_attr <- function(types, attr = "marker") {
  vapply(types, function(t) !is.null(entry(paste0(t %||l% "scatter", ".", attr))), logical(1))
}

# Evaluate formulas anywhere in a (nested) attribute list.
eval_attrs <- function(x, data) {
  if (inherits(x, "formula")) return(eval_attr(x, data))
  if (is.list(x) && !is.data.frame(x)) {
    cls <- oldClass(x)
    out <- lapply(x, eval_attrs, data = data)
    oldClass(out) <- cls
    return(out)
  }
  x
}

# Grouping variables of a dplyr grouped_df, read without depending on dplyr.
data_groups <- function(d) {
  # add_sf() data marks its group column with this attribute
  if (!is.null(attr(d, "plotly_groups"))) return(attr(d, "plotly_groups"))
  if (!inherits(d, "grouped_df")) return(character())
  setdiff(names(attr(d, "groups")), ".rows")
}

# Schema lookups made for every trace are memoised here (the schema doesn't
# change within a session); FALSE marks a path without a validator.
LP_SCHEMA_CACHE <- new.env(parent = emptyenv())

entry <- function(path) {
  e <- LP_SCHEMA_CACHE[[path]]
  if (is.null(e)) {
    e <- get_validator(path) %||% FALSE
    assign(path, e, envir = LP_SCHEMA_CACHE)
  }
  if (isFALSE(e)) NULL else e
}

i <- function(x) {
  if (is.null(x)) {
    return(NULL)
  } else if (length(x) == 1) {
    return(I(x))
  } else{
    return(x)
  }
}

rm_asis <- function(x) {
  # jsonlite converts NULL to {} and NA to null (plotly prefers null to {})
  # https://github.com/jeroenooms/jsonlite/issues/29
  if (is.null(x)) return(NA)
  if (is.data.frame(x)) return(x)
  if (is.list(x)) lapply(x, rm_asis) 
  # strip any existing 'AsIs' list elements of their 'AsIs' status.
  # this is necessary since ggplot_build(qplot(1:10, fill = I("red"))) 
  # returns list element with their 'AsIs' class, 
  # which conflicts with our JSON unboxing strategy.
  else if (inherits(x, "AsIs")) class(x) <- setdiff(class(x), "AsIs")
  else x
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}

remove_class <- function(x, y) {
  oldClass(x) <- setdiff(oldClass(x), y)
  x
}

# check that suggested packages are installed
try_library <- function(pkg, fun = NULL) {
  if (is_installed(pkg)) {
    return(invisible())
  }
  stop("Package `", pkg, "` required",  if (!is.null(fun)) paste0(" for `", fun, "`"), ".\n", 
       "Please install and try again.", call. = FALSE)
}

# similar logic to rstudioapi::isAvailable()
is_rstudio <- function() {
  identical(.Platform$GUI, "RStudio")
}

# nchar() needs a non-empty character vector; sometimes x will be a
# factor, or an empty vector.
robust_nchar <- function(x, ...) {
  if (length(x)) nchar(as.character(x), ...)
  else 0
}

# Extract longest element, or blank if none
longest_element <- function(x) {
  if (length(x))
    x[which.max(robust_nchar(x))]
  else
    ""
}

# A dplyr::group_by wrapper for the add argument
group_by_add <- function(..., add = TRUE) {
  if (get_package_version('dplyr') >= '1.0') {
    dplyr::group_by(...,  .add = add)
  } else {
    dplyr::group_by(...,  add = add)
  } 
}
