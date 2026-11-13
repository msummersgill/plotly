#' Combine plots into a grid of subplots
#'
#' Arranges several plots in one figure, like the legacy plotly package's
#' `subplot()`. Each plot's axes are renumbered and placed in a grid cell.
#' For new code, [make_subplots()] offers more control.
#'
#' @param ... Plots ([plot_ly()] objects or [go_figure()]s), or one list of
#'   them.
#' @param nrows Number of rows.
#' @param widths,heights Relative column widths and row heights (summing to
#'   at most 1).
#' @param margin Space between subplots, as a fraction of the figure: one
#'   value, or four (left, right, top, bottom).
#' @param shareX,shareY Share x axes within columns / y axes within rows?
#' @param titleX,titleY Keep x / y axis titles?
#' @param which_layout Which plots' layouts to use (`"merge"` for all).
#' @return A [plot_ly()]-style object.
#' @export
#' @examples
#' p1 <- mtcars |> plot_ly(x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
#' p2 <- mtcars |> plot_ly(x = ~hp, y = ~mpg, type = "scatter", mode = "markers")
#' subplot(p1, p2, shareY = TRUE, titleX = TRUE)
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
  x_axes <- lapply(layouts, pick_axes, "x")
  y_axes <- lapply(layouts, pick_axes, "y")
  if (!titleX) x_axes <- lapply(x_axes, function(ax) lapply(ax, function(a) { a$title <- NULL; a }))
  if (!titleY) y_axes <- lapply(y_axes, function(ax) lapply(ax, function(a) { a$title <- NULL; a }))
  nx <- lengths(x_axes)
  ny <- lengths(y_axes)
  ncols <- ceiling(length(plots) / nrows)
  x_id <- seq_len(sum(nx))
  if (shareX) {
    if (length(unique(nx)) > 1) warning("Must have a consistent number of axes per 'subplot' to share them.", call. = FALSE)
    else x_id <- rep(rep(seq_len(ncols * unique(nx)), length.out = length(plots)), unique(nx))
  }
  y_id <- seq_len(sum(ny))
  if (shareY) {
    if (length(unique(ny)) > 1) warning("Must have a consistent number of axes per 'subplot' to share them.", call. = FALSE)
    else y_id <- rep(rep(seq_len(nrows * unique(nx)), each = ncols, length.out = length(plots)), unique(ny))
  }
  x_cur <- unlist(lapply(x_axes, names))
  y_cur <- unlist(lapply(y_axes, names))
  x_new <- paste0(sub("[0-9]+$", "", x_cur), sub("^1$", "", x_id))
  y_new <- paste0(sub("[0-9]+$", "", y_cur), sub("^1$", "", y_id))
  x_map <- split(stats_named(x_cur, x_new), rep(seq_along(plots), nx))
  y_map <- split(stats_named(y_cur, y_new), rep(seq_along(plots), ny))
  dom <- get_domains(length(plots), nrows, margin, widths = widths, heights = heights)
  rescale_dom <- function(old, new) sort(rescale(unlist(old %||l% c(0, 1)), new, from = c(0, 1)))

  for (i in seq_along(plots)) {
    xm <- x_map[[i]]
    ym <- y_map[[i]]
    x_axes[[i]] <- stats_named(x_axes[[i]], names(xm))
    y_axes[[i]] <- stats_named(y_axes[[i]], names(ym))
    for (j in seq_along(x_axes[[i]])) {
      if (grepl("^geo[0-9]*$|^map[0-9]*$", names(x_axes[[i]])[j])) next
      m <- ym[ym %in% sub("y", "yaxis", x_axes[[i]][[j]]$anchor %||l% "y")]
      x_axes[[i]][[j]]$anchor <- sub("axis", "", names(m))
    }
    for (j in seq_along(y_axes[[i]])) {
      if (grepl("^geo[0-9]*$|^map[0-9]*$", names(y_axes[[i]])[j])) next
      m <- xm[xm %in% sub("x", "xaxis", y_axes[[i]][[j]]$anchor %||l% "x")]
      y_axes[[i]][[j]]$anchor <- sub("axis", "", names(m))
    }
    for (key in c("geo", "subplot", "xaxis", "yaxis")) {
      old <- unlist(lapply(traces[[i]], `[[`, key))
      if (!length(old)) next
      amap <- if (key == "yaxis") ym else xm
      amap <- stats_named(sub("axis", "", amap), sub("axis", "", names(amap)))
      new <- names(amap)[match(old, amap)]
      traces[[i]] <- Map(function(tr, a) { tr[[key]] <- a; tr }, traces[[i]], new)
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
    xd <- as.numeric(dom[i, c("xstart", "xend")])
    yd <- as.numeric(dom[i, c("yend", "ystart")])
    fix <- function(ax, main) {
      if (is.list(ax$domain) && all(c("x", "y") %in% names(ax$domain))) {
        ax$domain$x <- rescale_dom(ax$domain$x, xd)
        ax$domain$y <- rescale_dom(ax$domain$y, yd)
      } else {
        ax$domain <- rescale_dom(ax$domain, main)
      }
      ax
    }
    x_axes[[i]] <- lapply(x_axes[[i]], fix, main = xd)
    y_axes[[i]] <- lapply(y_axes[[i]], fix, main = yd)
  }
  out <- list(data = unlist(traces, recursive = FALSE), layout = Reduce(modify_list, c(x_axes, rev(y_axes))))
  rows <- split(dom, seq_along(plots))
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

get_domains <- function(nplots = 1, nrows = 1, margins = 0.01, widths = NULL, heights = NULL) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  widths <- widths %||l% rep(1 / ncols, ncols)
  heights <- heights %||l% rep(1 / nrows, nrows)
  if (length(widths) != ncols) stop("The length of the widths argument must be equal to the number of columns", call. = FALSE)
  if (length(heights) != nrows) stop("The length of the heights argument is ", length(heights), ", but the number of rows is ", nrows, call. = FALSE)
  if (any(widths < 0) || any(heights < 0)) stop("The widths and heights arguments must contain positive values", call. = FALSE)
  if (sum(widths) > 1 + sqrt(.Machine$double.eps) || sum(heights) > 1 + sqrt(.Machine$double.eps)) {
    stop("The sum of the widths and heights arguments must be less than 1", call. = FALSE)
  }
  widths <- cumsum(c(0, widths))
  heights <- cumsum(c(0, heights))
  widths <- widths + (1 - max(widths)) / 2
  heights <- heights + (1 - max(heights)) / 2
  xs <- lapply(seq_len(ncols), function(i) c(xstart = widths[i] + if (i == 1) 0 else margins[1],
                                            xend = widths[i + 1] - if (i == ncols) 0 else margins[2]))
  xz <- rep_len(xs, nplots)
  ys <- lapply(seq_len(nplots), function(i) {
    j <- ceiling(i / ncols)
    c(ystart = 1 - heights[j] - if (j == 1) 0 else margins[3], yend = 1 - heights[j + 1] + if (j == nrows) 0 else margins[4])
  })
  as.data.frame(do.call(rbind, Map(c, xz, ys)))
}

reposition <- function(obj, domains) {
  xd <- as.numeric(domains[c("xstart", "xend")])
  yd <- as.numeric(domains[c("yend", "ystart")])
  for (i in seq_along(obj)) {
    o <- obj[[i]]
    xs <- if (identical(o$xref, "paper")) {
      if (is.numeric(o$sizex)) obj[[i]]$sizex <- o$sizex * abs(diff(xd))
      if (identical(o$xsizemode, "pixel")) "xanchor" else c("x", "x0", "x1")
    }
    for (j in xs) if (is.numeric(o[[j]])) obj[[i]][[j]] <- rescale(o[[j]], xd, from = c(0, 1))
    ys <- if (identical(o$yref, "paper")) {
      if (is.numeric(o$sizey)) obj[[i]]$sizey <- o$sizey * abs(diff(yd))
      if (identical(o$ysizemode, "pixel")) "yanchor" else c("y", "y0", "y1")
    }
    for (j in ys) if (is.numeric(o[[j]])) obj[[i]][[j]] <- rescale(o[[j]], yd, from = c(0, 1))
  }
  obj
}

# Re-assign colorway colors (marked by the build step) after combining plots.
colorway_retrain <- function(traces, colorway) {
  for (i in seq_along(traces)) {
    col <- colorway[[i]]
    retrain <- function(x) {
      if (inherits(x, "colorway")) {
        alpha <- as.numeric(grDevices::col2rgb(rgb2hex(x), alpha = TRUE)["alpha", ] / 255)
        prefix_class(default(toRGB(col, alpha)), "colorway")
      } else if (is.list(x) && !is.data.frame(x)) {
        cls <- oldClass(x)
        x <- lapply(x, retrain)
        oldClass(x) <- cls
        x
      } else x
    }
    traces[[i]] <- retrain(traces[[i]])
  }
  traces
}

