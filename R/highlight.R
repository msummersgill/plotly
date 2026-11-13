LP_CT_KEY <- ".crossTalkKey"

lp_is_shared <- function(x) inherits(x, "SharedData")

lp_require_crosstalk <- function() {
  if (!requireNamespace("crosstalk", quietly = TRUE)) {
    stop("Linked highlighting needs the crosstalk package: install.packages(\"crosstalk\")", call. = FALSE)
  }
}

# SharedData -> its data with the key column and a "set" attribute (the
# crosstalk group).
lp_shared_data <- function(dat) {
  if (!lp_is_shared(dat)) return(dat)
  key <- dat$key()
  set <- dat$groupName()
  d <- dat$origData()
  if (data.table::is.data.table(d)) {
    d <- data.table::copy(d)
    data.table::set(d, j = LP_CT_KEY, value = key)
  } else {
    d[[LP_CT_KEY]] <- key
  }
  attr(d, "set") <- set
  d
}

#' Linked highlighting (brushing) across plots
#'
#' Sets how selections are made and shown in plots whose data is a
#' [highlight_key()] (a [crosstalk::SharedData] object). Selecting marks in
#' one plot highlights the observations with the same keys in every plot (and
#' other crosstalk-compatible htmlwidgets) sharing the data's group, without
#' a Shiny server. Holding the shift key while selecting adds to the current
#' selection. Requires the crosstalk package.
#'
#' @param p A [plot_ly()] object.
#' @param on Event that selects: `"plotly_click"`, `"plotly_hover"`,
#'   `"plotly_selected"` (box or lasso selection) or `"plotly_selecting"`
#'   (while selecting). `NULL` disables selecting from the plot.
#' @param off Event that clears the selection: `"plotly_doubleclick"`,
#'   `"plotly_deselect"` or `"plotly_relayout"`. Default: the one matching
#'   `on`. `NULL` disables it.
#' @param persistent Accumulate selections instead of replacing them (the
#'   shift key does this temporarily)?
#' @param dynamic Add a colour picker for the selection colour?
#' @param color Selection colour(s), or `NULL` to keep the marks' colours.
#'   Several colours are offered by the picker when `dynamic = TRUE`.
#' @param selectize Add a selectize.js dropdown for choosing keys (labelled
#'   with the `group` of [highlight_key()])? A list of selectize.js options may
#'   be given instead of `TRUE`.
#' @param defaultValues Keys selected when the plot is shown.
#' @param opacityDim Multiplier (0 to 1) for the opacity of unselected traces.
#' @param selected Attributes of the selection traces, see
#'   [attrs_selected()].
#' @param debounce Milliseconds to wait before selecting (useful with
#'   `on = "plotly_hover"`).
#' @param ... Not supported.
#' @return The plot.
#' @seealso [highlight_key()], [crosstalk::bscols()] to arrange linked views.
#' @export
#' @examplesIf requireNamespace("crosstalk", quietly = TRUE)
#' d <- mtcars |> highlight_key(~cyl)
#' p <- d |> plot_ly(x = ~wt, y = ~mpg) |> add_markers()
#' highlight(p, on = "plotly_selected", color = "red")
#'
#' # two linked views of the same data
#' s <- highlight_key(data.frame(x = 1:10, y = (1:10)^2, g = rep(c("a", "b"), 5)))
#' subplot(
#'   s |> plot_ly(x = ~x, y = ~y) |> add_markers(),
#'   s |> plot_ly(x = ~g) |> add_histogram()
#' ) |> highlight(on = "plotly_selected", dynamic = TRUE)
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
  off <- if (missing(off)) lp_default(lp_highlight_off(on)) else if (!is.null(off)) match.arg(off, off_options)
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

lp_highlight_off <- function(on) {
  switch(on %||l% "",
    plotly_selecting = , plotly_selected = "plotly_deselect",
    plotly_click = , plotly_hover = "plotly_doubleclick",
    "plotly_relayout"
  )
}

#' Attributes of selection traces
#'
#' Selected observations are drawn as new traces on top of the original ones;
#' these attributes are applied to them (see [highlight()]).
#'
#' @param opacity Opacity (0 to 1) of the selection traces.
#' @param ... Other trace attributes for the selection traces (e.g.
#'   `mode = "lines+markers"`, `showlegend = TRUE`).
#' @return A list of attributes.
#' @export
#' @examples
#' attrs_selected(showlegend = TRUE, marker = list(symbol = "x"))
attrs_selected <- function(opacity = 1, ...) {
  if (opacity < 0 || 1 < opacity) stop("opacity must be between 0 and 1", call. = FALSE)
  c(list(...), list(opacity = opacity))
}

# Build step: highlight defaults, crosstalk groups, selectize payloads and
# the default dragmode (legacy supply_highlight_attrs()).
lp_supply_highlight_attrs <- function(p) {
  p$x$highlight <- p$x$highlight %||l% lp_highlight_defaults()
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
      if (!is.logical(selectize)) opts <- lp_modify_list(opts, selectize)
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
  p$x$layout$dragmode <- p$x$layout$dragmode %|D|% lp_default(drag)
  if (lp_is_default(hl$off)) {
    message(sprintf("Setting the `off` event (i.e., '%s') to match the `on` event (i.e., '%s'). You can change this default via the `highlight()` function.",
                    lp_remove_class(hl$off, "plotly_default"), hl$on))
    p$x$highlight$off <- lp_remove_class(hl$off, "plotly_default")
  }
  p
}

# legacy_figure(): move keys and sets out of the traces and frames (and drop
# add_sf()'s internal bounding boxes).
lp_extract_keys <- function(x) {
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

highlight_defaults <- function() lp_highlight_defaults()


# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------


lp_highlight_defaults <- function() {
  list(on = "plotly_click", off = "plotly_doubleclick", persistent = getOption("persistent", FALSE),
       dynamic = FALSE, selectize = FALSE, opacityDim = getOption("opacityDim", 0.2),
       selected = attrs_selected(), debounce = 0)
}


