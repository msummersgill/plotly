#' Modify the layout of a plotly visualization
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plotly.com/r/reference/layout/}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in [plot_ly()].
#' @author Carson Sievert
#' @export
layout <- function(p, ..., data = NULL) {
  UseMethod("layout")
}

#' @export
layout.matrix <- function(p, ..., data = NULL) {
  # workaround for the popular graphics::layout() function
  # https://github.com/ropensci/plotly/issues/464
  graphics::layout(p, ...)
}

#' @export
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  idx <- which(vapply(p, is_legacy_plot, logical(1)))
  for (i in idx) {
    p[[i]] <- layout.plotly(p[[i]], ...)
  }
  p
}

#' @export
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
  if (!is.null(attrs[["height"]]) || !is.null(attrs[["width"]])) {
    warning("Specifying width/height in layout() is now deprecated.\n", 
            "Please specify in ggplotly() or plot_ly()", call. = FALSE)
  }
  # similar to add_trace()
  p$x$layoutAttrs <- c(
    p$x$layoutAttrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  p
}

#' Add a range slider to the x-axis
#'
#' @param p plotly object.
#' @param start a start date/value.
#' @param end an end date/value.
#' @param ... these arguments are documented here 
#' \url{https://plotly.com/r/reference/#layout-xaxis-rangeslider}
#' @export
#' @author Carson Sievert
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>% 
#'   add_lines() %>%
#'   rangeslider()
#'   
#' d <- tibble::tibble(
#'   time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
#'   y = rnorm(seq_along(time))
#'  )
#'  
#' plot_ly(d, x = ~time, y = ~y) %>%
#'   add_lines() %>%
#'   rangeslider(d$time[5], d$time[50])
#'   
#' 
rangeslider <- function(p, start = NULL, end = NULL, ...) {
  rng <- if (!is.null(start) || !is.null(end)) list(to_milliseconds(start), to_milliseconds(end))
  if (inherits(p, "go_figure")) {
    patch <- c(list(rangeslider = c(list(visible = TRUE), list(...))), if (!is.null(rng)) list(range = rng))
    return(update_xaxes(p, patch))
  }
  if (sum(grepl("^xaxis", names(p$x$layout))) > 1) {
    stop("Can only add a rangeslider to a plot with one x-axis", call. = FALSE)
  }
  p$x$layout$xaxis$range <- rng
  p$x$layout$xaxis$rangeslider <- list(visible = TRUE, ...)
  p
}

# config() for plot_ly() objects, with plotly 4.x's handling of options
# plotly.js no longer has: `collaborate` and `cloud` warn as before, options
# of removed features (Mapbox, Chart Studio links, global transforms) are
# ignored, and other unknown options are dropped with a warning when the
# plot is built (verify_config()), as plotly.js ignored them.
LP_REMOVED_CONFIG <- c("mapboxAccessToken", "showLink", "linkText", "sendData", "showSources",
                       "showEditInChartStudio", "globalTransforms", "collaborate")

#' @export
config.plotly <- function(p, ..., cloud = FALSE, showSendToCloud = cloud) {
  args <- list(...)
  if ("collaborate" %in% names(args)) warning("The collaborate button is no longer supported", call. = FALSE)
  if (isTRUE(cloud)) warning("The `cloud` argument is deprecated. Use `showSendToCloud` instead.", call. = FALSE)
  args <- args[!names(args) %in% LP_REMOVED_CONFIG]
  if (identical(args$mathjax, "local")) {
    warning("`mathjax = \"local\"` is no longer supported; MathJax is loaded from a CDN", call. = FALSE)
    args$mathjax <- "cdn"
  }
  if (!missing(cloud) || !missing(showSendToCloud)) args$showSendToCloud <- isTRUE(showSendToCloud)
  core_args <- c("locale", "mathjax", names(schema(jsonedit = FALSE)$config))
  unknown <- args[!names(args) %in% core_args]
  args <- args[names(args) %in% core_args]
  p <- do.call(utils::getS3method("config", "default", envir = asNamespace("plotly.core")), c(list(p), args))
  if (length(unknown)) p$x$config <- utils::modifyList(p$x$config %||l% list(), unknown)
  p
}

verify_config <- function(p) {
  bad <- setdiff(names(p$x$config), names(schema(jsonedit = FALSE)$config))
  for (b in bad) {
    warning("'", b, "' is not a valid config attribute and will be ignored (see plotly.core::schema()$config)", call. = FALSE)
    p$x$config[[b]] <- NULL
  }
  p
}
