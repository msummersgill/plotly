LP_CT_KEY <- ".crossTalkKey"

is_shared <- function(x) inherits(x, "SharedData")

require_crosstalk <- function() {
  if (!requireNamespace("crosstalk", quietly = TRUE)) {
    stop("Linked highlighting needs the crosstalk package: install.packages(\"crosstalk\")", call. = FALSE)
  }
}

#' Query graphical elements in multiple linked views
#' 
#' This function sets a variety of options for brushing (i.e., highlighting)
#' multiple plots. These options are primarily designed for linking
#' multiple plotly graphs, and may not behave as expected when linking 
#' plotly to another htmlwidget package via crosstalk. In some cases,
#' other htmlwidgets will respect these options, such as persistent selection
#' in leaflet (see `demo("highlight-leaflet", package = "plotly")`).
#' 
#' @param p a plotly visualization.
#' @param on turn on a selection on which event(s)? To disable on events 
#' altogether, use `NULL`. Currently the following are supported:
#' \itemize{
#'  \item `'plotly_click'`
#'  \item `'plotly_hover'`
#'  \item `'plotly_selected'`: triggered through rectangular 
#'  (layout.dragmode = 'select') or lasso (layout.dragmode = 'lasso') brush.
#' }
#' @param off turn off a selection on which event(s)? To disable off 
#' events altogether, use `NULL`. Currently the following are supported:
#' \itemize{
#'  \item `'plotly_doubleclick'`: triggered on a double mouse click while
#'  (layout.dragmode = 'zoom') or (layout.dragmode = 'pan')
#'  \item `'plotly_deselect'`: triggered on a double mouse click while 
#'  (layout.dragmode = 'select') or (layout.dragmode = 'lasso')
#'  \item `'plotly_relayout'`: triggered whenever axes are rescaled 
#'  (i.e., clicking the home button in the modebar) or whenever the height/width
#'  of the plot changes.
#' }
#' @param persistent should selections persist (i.e., accumulate)? We often
#' refer to the default (`FALSE`) as a 'transient' selection mode; 
#' which is recommended, because one may switch from 'transient' to 
#' 'persistent' selection by holding the shift key.
#' @param dynamic should a widget for changing selection colors be included? 
#' @param color character string of color(s) to use for 
#' highlighting selections. See [toRGB()] for valid color
#' specifications. If `NULL` (the default), the color of selected marks
#' are not altered.
#' @param selectize whether or not to render a selectize.js widget for selecting
#'   [highlight_key()] values. A list of additional selectize.js options may
#'   also be provided. The label used for this widget should be set via the
#'   `groupName` argument of [highlight_key()].
#' @param defaultValues a vector of values for setting a "default selection".
#' These values should match the key attribute.
#' @param opacityDim a number between 0 and 1 used to reduce the
#' opacity of non-selected traces (by multiplying with the existing opacity).
#' @param selected attributes of the selection, see [attrs_selected()].
#' @param debounce amount of time to wait before firing an event (in milliseconds).
#' The default of 0 means do not debounce at all. 
#' Debouncing is mainly useful when `on = "plotly_hover"` to avoid firing too many events
#' when users clickly move the mouse over relevant graphical marks.
#' @param ... currently not supported.
#' @export
#' @author Carson Sievert
#' @references \url{https://plotly-r.com/client-side-linking.html}
#' @seealso [attrs_selected()]
#' @examplesIf interactive()
#' 
#' # These examples are designed to show you how to highlight/brush a *single*
#' # view. For examples of multiple linked views, see `demo(package = "plotly")` 
#' 
#' d <- highlight_key(txhousing, ~city)
#' p <- ggplot(d, aes(date, median, group = city)) + geom_line()
#' gg <- ggplotly(p, tooltip = "city") 
#' highlight(gg, dynamic = TRUE)
#' 
#' # supply custom colors to the brush 
#' cols <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
#' highlight(gg, on = "plotly_hover", color = cols, dynamic = TRUE)
#' 
#' # Use attrs_selected() for complete control over the selection appearance
#' # note any relevant colors you specify here should override the color argument
#' s <- attrs_selected(
#'   showlegend = TRUE,
#'   mode = "lines+markers",
#'   marker = list(symbol = "x")
#' )
#' 
#' highlight(layout(gg, showlegend = TRUE), selected = s)
#' 

highlight <- function(p, on = "plotly_click", off, persistent = getOption("persistent", FALSE),
                      dynamic = FALSE, color = NULL, selectize = FALSE, defaultValues = NULL,
                      opacityDim = getOption("opacityDim", 0.2), selected = attrs_selected(),
                      debounce = 0, ...) {
  if (!is_legacy_plot(p)) stop("highlight() works with plot_ly() objects.", call. = FALSE)
  if (length(list(...))) {
    warning("The following arguments are not supported:\n", toString(names(list(...))), "\n",
            "Arguments such as: hoverinfo and showInLegend \nhave been replaced by selected and other",
            call. = FALSE)
  }
  if (opacityDim < 0 || 1 < opacityDim) stop("opacityDim must be between 0 and 1", call. = FALSE)
  if (dynamic && length(color) < 2) {
    message("Adding more colors to the selection color palette.")
    color <- c(color, RColorBrewer::brewer.pal(4, "Set1"))
  }
  if (!dynamic && length(color) > 1) {
    warning("Can only use a single color for selections when `dynamic = FALSE`.", call. = FALSE)
    color <- color[1]
  }
  off_options <- paste0("plotly_", c("doubleclick", "deselect", "relayout"))
  off <- if (missing(off)) default(highlight_off(on)) else if (!is.null(off)) match.arg(off, off_options)
  if (isTRUE(persistent)) {
    message("We recommend setting `persistent` to `FALSE` (the default) because persistent selection mode ",
            "can now be used by holding the shift key (while triggering the `on` event).")
  }
  p$x$highlight <- list(
    on = if (!is.null(on)) match.arg(on, paste0("plotly_", c("click", "hover", "selected", "selecting"))),
    off = off,
    persistent = persistent,
    dynamic = dynamic,
    color = toRGB(color),
    selectize = selectize,
    defaultValues = defaultValues,
    opacityDim = opacityDim,
    selected = selected,
    debounce = debounce
  )
  p
}

highlight_off <- function(on) {
  switch(on %||l% "",
    plotly_selecting = , plotly_selected = "plotly_deselect",
    plotly_click = , plotly_hover = "plotly_doubleclick",
    "plotly_relayout"
  )
}

#' Specify attributes of selection traces
#' 
#' By default the name of the selection trace derives from the selected values.
#' 
#' 
#' @param opacity a number between 0 and 1 specifying the overall opacity of
#' the selected trace
#' @param ... other trace attributes attached to the selection trace.
#' @export
#' @author Carson Sievert

attrs_selected <- function(opacity = 1, ...) {
  if (opacity < 0 || 1 < opacity) {
    stop("opacity must be between 0 and 1", call. = FALSE)
  }
  
  args <- list(
    opacity = opacity
  )
  
  # TODO: verify attr names... maybe that should happen in the build step?
  dots <- list(...)
  
  
  c(dots, args)
}

# Build step: highlight defaults, crosstalk groups, selectize payloads and
# the default dragmode (legacy supply_highlight_attrs()).
supply_highlight_attrs <- function(p) {
  p$x$highlight <- p$x$highlight %||l% highlight_defaults()
  sets <- lapply(p$x$data, `[[`, "set")
  no_set <- vapply(sets, is.null, logical(1))
  if (all(no_set)) return(p)
  hl <- p$x$highlight
  groups <- unique(unlist(sets))
  p$x$highlight$ctGroups <- I(groups)
  keys <- stats_named(lapply(p$x$data[!no_set], `[[`, "key"), unlist(sets[!no_set]))
  for (g in groups) {
    raw_k <- unique(unlist(keys[names(keys) %in% g], use.names = FALSE))
    k <- as.character(raw_k)
    if (!length(k)) next
    vals <- intersect(as.character(hl$defaultValues), k)
    if (length(vals)) p$x$highlight$defaultSelection[[g]] <- I(vals)
    selectize <- hl$selectize %||l% FALSE
    if (!identical(selectize, FALSE)) {
      opts <- list(items = data.frame(value = raw_k, label = raw_k, stringsAsFactors = FALSE), group = g)
      if (!is.logical(selectize)) opts <- modify_list(opts, selectize)
      old <- p$x$selectize[[g]]
      if (length(old)) {
        if (length(setdiff(k, old$items$value))) {
          warning("Overwriting the existing selectize payload for group '", g, "'. If you've previously ",
                  "modified this payload in some way, consider modifying it again.", call. = FALSE)
        } else {
          opts <- old
        }
      }
      p$x$selectize[[g]] <- opts
    }
  }
  drag <- switch(hl$on %||l% "", plotly_selected = , plotly_selecting = "select", "zoom")
  p$x$layout$dragmode <- p$x$layout$dragmode %|D|% default(drag)
  if (is_default(hl$off)) {
    message(sprintf("Setting the `off` event (i.e., '%s') to match the `on` event (i.e., '%s'). You can change this default via the `highlight()` function.",
                    remove_class(hl$off, "plotly_default"), hl$on))
    p$x$highlight$off <- remove_class(hl$off, "plotly_default")
  }
  p
}

# legacy_figure(): move keys and sets out of the traces and frames (and drop
# add_sf()'s internal bounding boxes).
extract_keys <- function(x) {
  take <- function(traces, what) {
    v <- lapply(traces, `[[`, what)
    if (all(vapply(v, is.null, logical(1)))) NULL else v
  }
  # a simple key with several values (the whole trace stands for them) is
  # sent as {simple: [...]}; a single value or a per-point array as is
  take_keys <- function(traces) {
    v <- lapply(traces, function(tr) {
      k <- tr[["key"]]
      if (!is.null(k) && isTRUE(tr[["_isSimpleKey"]]) && length(k) > 1) list(simple = I(as.character(k)))
      else if (length(k) == 1 && !is.list(k)) as.character(k)
      else k
    })
    if (all(vapply(v, is.null, logical(1)))) NULL else v
  }
  drop <- function(traces) lapply(traces, function(tr) {
    tr$key <- NULL; tr$set <- NULL; tr[["_bbox"]] <- NULL
    tr[["_isSimpleKey"]] <- NULL; tr[["_isNestedKey"]] <- NULL
    tr
  })
  x$keys <- take_keys(x$data)
  x$sets <- take(x$data, "set")
  x$data <- drop(x$data)
  if (length(x$frames)) {
    fk <- lapply(x$frames, function(f) list(keys = take_keys(f$data), sets = take(f$data, "set")))
    if (any(vapply(fk, function(f) !is.null(f$keys) || !is.null(f$sets), logical(1)))) x$frameKeys <- fk
    x$frames <- lapply(x$frames, function(f) { f$data <- drop(f$data); f })
  }
  if (is.null(x$sets)) {
    x$highlight <- NULL
    x$selectize <- NULL
  }
  x
}


# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------


highlight_defaults <- function() {
  list(on = "plotly_click", off = "plotly_doubleclick", persistent = getOption("persistent", FALSE),
       dynamic = FALSE, selectize = FALSE, opacityDim = getOption("opacityDim", 0.2),
       selected = attrs_selected(), debounce = 0)
}
