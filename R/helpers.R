#' @rdname hide_legend
#' @export
colorbar <- function(p, ..., limits = NULL, which = 1) {
  p <- plotly_build(p)
  is_bar <- vapply(p$x$data, is.colorbar, logical(1))
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
    if (z_color) p$x$data[[i]]$colorbar <- modify_list(tr$colorbar, list(...))
    else p$x$data[[i]]$marker$colorbar <- modify_list(tr$marker$colorbar, list(...))
  }
  p
}


#' @rdname hide_legend
#' @export
hide_guides <- function(p) hide_legend(hide_colorbar(p))


#' @rdname hide_legend
#' @export
hide_colorbar <- function(p) {
  if (inherits(p, c("gg", "ggmatrix"))) p <- plotly_build(p)
  if (inherits(p, "go_figure")) {
    p <- for_each_trace(p, function(tr) {
      if (has_attr(tr$type, "showscale")) tr$showscale <- FALSE
      if (has_attr(tr$type, "marker.showscale")) tr$marker$showscale <- FALSE
      tr
    })
    return(update_layout(p, coloraxis_showscale = FALSE))
  }
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    type <- p$x$data[[i]]$type
    if (has_attr(type, "showscale")) p$x$data[[i]]$showscale <- default(FALSE)
    if (has_attr(type, "marker.showscale")) p$x$data[[i]]$marker$showscale <- default(FALSE)
  }
  p
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


#' @rdname plot_ly
#' @export
plotly_empty <- function(...) {
  ax <- list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
  layout(plot_ly(...), xaxis = ax, yaxis = ax)
}


#' Encode a raster as a data URI
#'
#' Converts a raster (or matrix/array convertible with
#' [grDevices::as.raster()]) to a PNG data URI, e.g. for layout images.
#' Requires the png package.
#'
#' @param r A raster, matrix or array.
#' @param ... Passed to [grDevices::as.raster()].
#' @return A character string.
#' @export
#' @examples
#' if (requireNamespace("png", quietly = TRUE)) substr(raster2uri(matrix(1:4 / 4, 2)), 1, 30)
raster2uri <- function(r, ...) {
  if (!requireNamespace("png", quietly = TRUE)) stop("raster2uri() needs the png package", call. = FALSE)
  if (inherits(r, "nativeRaster")) {
    img <- r
  } else {
    r <- grDevices::as.raster(r, ...)
    rgbs <- grDevices::col2rgb(c(r), alpha = TRUE) / 255
    nr <- dim(r)[1]
    nc <- dim(r)[2]
    ch <- function(i) matrix(rgbs[i, ], nrow = nr, ncol = nc, byrow = TRUE)
    img <- array(c(ch(1), ch(2), ch(3), ch(4)), dim = c(dim(r), 4))
  }
  paste0("data:image/png;base64,", base64enc::base64encode(png::writePNG(img)))
}
