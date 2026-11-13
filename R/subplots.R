#' View multiple plots in a single view
#' 
#' @param ... One of the following 
#' \itemize{
#'  \item any number of plotly/ggplot2 objects.
#'  \item a list of plotly/ggplot2 objects.
#'  \item a tibble with one list-column of plotly/ggplot2 objects.
#' }
#' @param nrows number of rows for laying out plots in a grid-like structure.
#' Only used if no domain is already specified.
#' @param widths relative width of each column on a 0-1 scale. By default all
#' columns have an equal relative width.
#' @param heights relative height of each row on a 0-1 scale. By default all
#' rows have an equal relative height.
#' @param margin either a single value or four values (all between 0 and 1).
#' If four values are provided, the first is used as the left margin, the second
#' is used as the right margin, the third is used as the top margin, and the
#' fourth is used as the bottom margin.
#' If a single value is provided, it will be used as all four margins. 
#' @param shareX should the x-axis be shared amongst the subplots?
#' @param shareY should the y-axis be shared amongst the subplots?
#' @param titleX should x-axis titles be retained?
#' @param titleY should y-axis titles be retained?
#' @param which_layout adopt the layout of which plot? If the default value of 
#' "merge" is used, layout options found later in the sequence of plots will 
#' override options found earlier in the sequence. This argument also accepts a 
#' numeric vector specifying which plots to consider when merging.
#' @return A plotly object
#' @export
#' @author Carson Sievert
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # pass any number of plotly objects to subplot()
#' p1 <- plot_ly(economics, x = ~date, y = ~uempmed)
#' p2 <- plot_ly(economics, x = ~date, y = ~unemploy)
#' subplot(p1, p2, p1, p2, nrows = 2, margin = 0.05)
#' 
#' #'  # anchor multiple traces on the same legend entry
#'  p1 <- add_lines(p1, color = I("black"), name = "1st", legendgroup = "1st")
#'  p2 <- add_lines(p2, color = I("red"), name = "2nd", legendgroup = "2nd")
#'  
#'  subplot(
#'    p1, style(p1, showlegend = FALSE),
#'    p2, style(p2, showlegend = FALSE),
#'    nrows = 2, margin = 0.05
#'  )
#' 
#' # or pass a list
#' economics_long %>%
#'   split(.$variable) %>%
#'   lapply(function(d) plot_ly(d, x = ~date, y = ~value)) %>%
#'   subplot(nrows = NROW(.), shareX = TRUE)
#'   
#' # or pass a tibble with a list-column of plotly objects
#' economics_long %>%
#'   group_by(variable) %>%
#'   do(p = plot_ly(., x = ~date, y = ~value)) %>%
#'   subplot(nrows = NROW(.), shareX = TRUE)
#'   
#' # learn more at https://plotly.com/r/subplots/
#' 

subplot <- function(..., nrows = 1, widths = NULL, heights = NULL, margin = 0.02, shareX = FALSE,
                    shareY = FALSE, titleX = shareX, titleY = shareY, which_layout = "merge") {
  plots <- list(...)
  if (length(plots) == 1 && is.list(plots[[1]]) && !is_legacy_plot(plots[[1]]) && !inherits(plots[[1]], "go_figure")) {
    plots <- plots[[1]]
  }
  # a data frame (e.g. from dplyr's do()) with one column holding the plots
  if (is.data.frame(plots)) {
    is_plot <- function(p) is_legacy_plot(p) || inherits(p, "go_figure")
    idx <- which(vapply(plots, function(col) is.list(col) && length(col) && is_plot(col[[1]]), logical(1)))
    if (length(idx) != 1) {
      stop("A data frame given to subplot() must have one column holding the plots", call. = FALSE)
    }
    plots <- plots[[idx]]
  }
  plots <- lapply(plots, function(p) {
    if (inherits(p, "go_figure")) {
      p <- plain_value(unclass(p)[c("data", "layout")])
      # the result is a plot_ly() object: use its default margins, not plotly.js's
      p$layout$margin <- p$layout$margin %||l% LP_MARGIN
      # go traces leave their default axes out; name them so they are renumbered
      for (i in seq_along(p$data)) {
        tr <- p$data[[i]]
        if (has_attr(tr[["type"]] %||l% "scatter", "xaxis")) {
          p$data[[i]][["xaxis"]] <- tr[["xaxis"]] %||l% "x"
          p$data[[i]][["yaxis"]] <- tr[["yaxis"]] %||l% "y"
        }
      }
      return(p)
    }
    b <- plotly_build(p, registerFrames = FALSE)$x
    # drop empty traces with no subplot (e.g. from ggplot() + geom_blank())
    empty <- vapply(b$data, function(tr) {
      is.null(tr[["type"]]) && all(is.na(c(tr[["xaxis"]], tr[["yaxis"]]))) && all(is.na(unlist(tr[c("x", "y")])))
    }, logical(1))
    b$data <- b$data[!empty]
    b
  })
  # plots with traces on several axes are split so each panel has one pair
  expanded <- list()
  for (p in plots) {
    layout_attrs <- c(names(p$layout), "map", "geo", "xaxis", "yaxis")
    # a trace's subplot reference (an NA one, e.g. from an empty ggplot, is the default)
    ref <- function(tr, k, default) {
      v <- tr[["subplot"]] %||l% tr[["geo"]] %||l% tr[[k]]
      if (!length(v) || all(is.na(v))) default else v[[1]]
    }
    x_attrs <- sub("^x", "xaxis", vapply(p$data, ref, "", "xaxis", "x"))
    y_attrs <- sub("^y", "yaxis", vapply(p$data, ref, "", "yaxis", "y"))
    missing_attrs <- setdiff(c(x_attrs, y_attrs), layout_attrs)
    if (!length(missing_attrs)) {
      expanded <- c(expanded, list(p))
      next
    }
    missing_tr <- x_attrs %in% missing_attrs | y_attrs %in% missing_attrs
    base <- p
    base$data[missing_tr] <- NULL
    expanded <- c(expanded, list(base))
    for (j in missing_attrs) {
      np <- list(data = p$data[x_attrs %in% j | y_attrs %in% j], layout = p$layout)
      np$data <- lapply(np$data, function(tr) {
        for (k in c("map", "geo", "xaxis", "yaxis")) tr[k] <- list(if (!is.null(tr[[k]])) sub("[0-9]+", "", tr[[k]]))
        tr
      })
      expanded <- c(expanded, list(np))
    }
  }
  plots <- expanded
  traces <- lapply(plots, `[[`, "data")
  layouts <- lapply(plots, `[[`, "layout")
  shapes <- lapply(layouts, `[[`, "shapes")
  images <- lapply(layouts, `[[`, "images")
  annotations <- lapply(layouts, function(l) l$annotations)
  pick_axes <- function(lay, letter) {
    keys <- grep(paste0("^geo[0-9]*$|^map[0-9]*$|^", letter, "axis"), names(lay), value = TRUE)
    if (!length(keys)) keys <- paste0(letter, "axis")
    out <- lapply(keys, function(k) lay[[k]] %||l% list())
    stats_named(out, keys)
  }
  xAxes <- lapply(layouts, pick_axes, "x")
  yAxes <- lapply(layouts, pick_axes, "y")
  if (!titleX) xAxes <- lapply(xAxes, function(ax) lapply(ax, function(a) { a$title <- NULL; a }))
  if (!titleY) yAxes <- lapply(yAxes, function(ax) lapply(ax, function(a) { a$title <- NULL; a }))
  xAxisN <- lengths(xAxes)
  yAxisN <- lengths(yAxes)
  # old -> new axis name dictionary
  ncols <- ceiling(length(plots) / nrows)
  xAxisID <- seq_len(sum(xAxisN))
  if (shareX) {
    if (length(unique(xAxisN)) > 1) warning("Must have a consistent number of axes per 'subplot' to share them.", call. = FALSE)
    else xAxisID <- rep(rep(seq_len(ncols * unique(xAxisN)), length.out = length(plots)), unique(xAxisN))
  }
  yAxisID <- seq_len(sum(yAxisN))
  if (shareY) {
    if (length(unique(yAxisN)) > 1) warning("Must have a consistent number of axes per 'subplot' to share them.", call. = FALSE)
    else yAxisID <- rep(rep(seq_len(nrows * unique(xAxisN)), each = ncols, length.out = length(plots)), unique(yAxisN))
  }
  # current "axis" names
  xCurrentNames <- unlist(lapply(xAxes, names))
  yCurrentNames <- unlist(lapply(yAxes, names))
  xNewNames <- paste0(
    sub("[0-9]+$", "", xCurrentNames), 
    sub("^1$", "", xAxisID)
  )
  yNewNames <- paste0(
    sub("[0-9]+$", "", yCurrentNames), 
    sub("^1$", "", yAxisID)
  )
  x_map <- split(stats_named(xCurrentNames, xNewNames), rep(seq_along(plots), xAxisN))
  y_map <- split(stats_named(yCurrentNames, yNewNames), rep(seq_along(plots), yAxisN))
  # domains of each subplot
  domainInfo <- get_domains(
    length(plots), nrows, margin, widths = widths, heights = heights
  )
  rescale_dom <- function(old, new) sort(rescale(unlist(old %||l% c(0, 1)), new, from = c(0, 1)))

  for (i in seq_along(plots)) {
    xm <- x_map[[i]]
    ym <- y_map[[i]]
    xAxes[[i]] <- stats_named(xAxes[[i]], names(xm))
    yAxes[[i]] <- stats_named(yAxes[[i]], names(ym))
    for (j in seq_along(xAxes[[i]])) {
      if (grepl("^geo[0-9]*$|^map[0-9]*$", names(xAxes[[i]])[j])) next
      m <- ym[ym %in% sub("y", "yaxis", xAxes[[i]][[j]]$anchor %||l% "y")]
      xAxes[[i]][[j]]$anchor <- sub("axis", "", names(m))
    }
    for (j in seq_along(yAxes[[i]])) {
      if (grepl("^geo[0-9]*$|^map[0-9]*$", names(yAxes[[i]])[j])) next
      m <- xm[xm %in% sub("x", "xaxis", yAxes[[i]][[j]]$anchor %||l% "x")]
      yAxes[[i]][[j]]$anchor <- sub("axis", "", names(m))
    }
    
    for (key in c("geo", "subplot", "xaxis", "yaxis")) {
      old <- unlist(lapply(traces[[i]], `[[`, key))
      if (!length(old)) next
      amap <- if (key == "yaxis") ym else xm
      amap <- stats_named(sub("axis", "", amap), sub("axis", "", names(amap)))
      new <- names(amap)[match(old, amap)]
      traces[[i]] <- Map(function(tr, a) { tr[[key]] <- a; tr }, traces[[i]], new)
      # bump annotation, image, shape xref/yref
      # (none of these layout components have geo/subplot support)
      ref <- list(xaxis = "xref", yaxis = "yref")[[key]]
      if (is.null(ref)) next
      bump <- function(obj, dflt = sub("ref", "", ref)) {
        obj[[ref]] <- obj[[ref]] %||l% dflt
        if (identical(obj[[ref]], "paper")) return(obj)
        idx <- match(obj[[ref]], amap)
        if (!is.na(idx)) obj[[ref]] <- names(amap)[idx][1]
        obj
      }
      annotations[[i]] <- lapply(annotations[[i]], bump)
      shapes[[i]] <- lapply(shapes[[i]], bump)
      images[[i]] <- lapply(images[[i]], bump, "paper")
    }
    xd <- as.numeric(domainInfo[i, c("xstart", "xend")])
    yd <- as.numeric(domainInfo[i, c("yend", "ystart")])
    fix <- function(ax, main) {
      if (is.list(ax$domain) && all(c("x", "y") %in% names(ax$domain))) {
        ax$domain$x <- rescale_dom(ax$domain$x, xd)
        ax$domain$y <- rescale_dom(ax$domain$y, yd)
      } else {
        ax$domain <- rescale_dom(ax$domain, main)
      }
      ax
    }
    xAxes[[i]] <- lapply(xAxes[[i]], fix, main = xd)
    yAxes[[i]] <- lapply(yAxes[[i]], fix, main = yd)
  }
  out <- list(data = unlist(traces, recursive = FALSE), layout = Reduce(modify_list, c(xAxes, rev(yAxes))))
  rows <- split(domainInfo, seq_along(plots))
  out$layout$annotations <- unlist(Map(reposition, annotations, rows), recursive = FALSE)
  out$layout$shapes <- unlist(Map(reposition, shapes, rows), recursive = FALSE)
  out$layout$images <- unlist(Map(reposition, images, rows), recursive = FALSE)
  other <- lapply(layouts, function(l) l[!grepl("^[x-y]axis|^geo[0-9]*$|^map[0-9]*$|annotations|shapes|images", names(l))])
  if (!identical(which_layout, "merge")) other <- other[which_layout[which_layout %in% seq_along(plots)]]
  out$layout <- Reduce(modify_list, other, out$layout)
  for (k in c("source", "config", "shinyEvents", "highlight", "selectize")) {
    vals <- compact(lapply(plots, `[[`, k))
    if (length(vals)) out[[k]] <- vals[[length(vals)]]
  }
  cw <- out$layout$colorway %||l% LP_COLORWAY
  out$data <- colorway_retrain(out$data, rep(cw, length.out = length(out$data)))
  out$subplot <- TRUE
  out$attrs <- list()
  legacy_widget_lazy(out)
}

# ----------------------------------------------------------------
# Functions used solely within subplot()
# ----------------------------------------------------------------



get_domains <- function(nplots = 1, nrows = 1, margins = 0.01, 
                        widths = NULL, heights = NULL) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  widths <- widths %||% rep(1 / ncols, ncols)
  heights <- heights %||% rep(1 / nrows, nrows)
  if (length(widths) != ncols) {
    stop("The length of the widths argument must be equal ",
         "to the number of columns", call. = FALSE)
  }
  if (length(heights) != nrows) {
    stop("The length of the heights argument is ", length(heights),
         ", but the number of rows is ", nrows, call. = FALSE)
  }
  if (any(widths < 0) | any(heights < 0)) {
    stop("The widths and heights arguments must contain positive values")
  }
  if (sum(widths) > (1 + sqrt(.Machine$double.eps)) | sum(heights) > (1 + sqrt(.Machine$double.eps))) {
    stop("The sum of the widths and heights arguments must be less than 1")
  }
  
  widths <- cumsum(c(0, widths))
  heights <- cumsum(c(0, heights))
  # 'center' these values if there is still room left 
  widths <- widths + (1 - max(widths)) / 2
  heights <- heights + (1 - max(heights)) / 2
  
  xs <- vector("list", ncols)
  for (i in seq_len(ncols)) {
    xs[[i]] <- c(
      xstart = widths[i] + if (i == 1) 0 else margins[1],
      xend = widths[i + 1] - if (i == ncols) 0 else margins[2]
    )
  }
  xz <- rep_len(xs, nplots)
  
  ys <- vector("list", nrows)
  for (i in seq_len(nplots)) {
    j <- ceiling(i / ncols)
    ys[[i]] <- c(
      ystart = 1 - (heights[j]) - if (j == 1) 0 else margins[3],
      yend = 1 - (heights[j + 1]) + if (j == nrows) 0 else margins[4]
    )
  }
  list2df(Map(c, xz, ys))
}

list2df <- function(x, nms) {
  #stopifnot(length(unique(sapply(x, length))) == 1)
  m <- if (length(x) == 1) t(x[[1]]) else do.call(rbind, x)
  row.names(m) <- NULL
  df <- data.frame(m)
  if (!missing(nms)) setNames(df, nms) else df
}

# translate x/y positions according to domain objects 
# (useful mostly for repositioning annotations/shapes in subplots)
reposition <- function(obj, domains) {
  # we need x and y in order to rescale them!
  xdom <- as.numeric(domains[c("xstart", "xend")])
  ydom <- as.numeric(domains[c("yend", "ystart")])
  
  for (i in seq_along(obj)) {
    o <- obj[[i]]
    xs <- if (identical(o$xref, "paper")) {
      if (is.numeric(o$sizex)) obj[[i]]$sizex <- o$sizex * abs(diff(xdom))
      if (identical(o$xsizemode, "pixel")) "xanchor" else c("x", "x0", "x1")
    }
    for (j in xs) {
      if (is.numeric(o[[j]])) {
        obj[[i]][[j]] <- scales::rescale(o[[j]], xdom, from = c(0, 1))
      }
    }
    ys <- if (identical(o$yref, "paper")) {
      if (is.numeric(o$sizey)) obj[[i]]$sizey <- o$sizey * abs(diff(ydom))
      if (identical(o$ysizemode, "pixel")) "yanchor" else c("y", "y0", "y1")
    }
    for (j in ys) {
      if (is.numeric(o[[j]])) {
        obj[[i]][[j]] <- scales::rescale(o[[j]], ydom, from = c(0, 1))
      }
    }
  }
  obj
}


colorway_retrain <- function(traces, colorway = colorway()) {
  colorway <- rep(colorway, length.out = length(traces))
  for (i in seq_along(traces)) {
    col <- prefix_class(default(colorway[[i]]), "colorway")
    traces[[i]] <- rapply(traces[[i]], function(x) { if (inherits(x, "colorway")) alpha_inherit(col, x) else x }, how = "replace")
  }
  traces 
}


# retrieve the alpha of an 'old' color code and apply it to a new color code
alpha_inherit <- function(new, old) {
  # should return the alpha in a rgba() code
  alphas <- as.numeric(col2rgb(rgb2hex(old), alpha = TRUE)["alpha", ] / 255)
  prefix_class(default(toRGB(new, alphas)), "colorway")
}
