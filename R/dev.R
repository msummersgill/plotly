
#' Acquire (and optionally display) plotly's plot schema
#' 
#' The schema contains valid attributes names, their value type, 
#' default values (if any), and min/max values (if applicable).
#' 
#' @param jsonedit use `listviewer::jsonedit` to view the JSON?
#' @param ... other options passed onto `listviewer::jsonedit`
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' s <- schema()
#' 
#' # retrieve acceptable `layout.mapbox.style` values
#' if (!is.na(Sys.getenv('MAPBOX_TOKEN', NA))) {
#'   styles <- s$layout$layoutAttributes$mapbox$style$values
#'   subplot(
#'     plot_mapbox() %>% layout(mapbox = list(style = styles[3])),
#'     plot_mapbox() %>% layout(mapbox = list(style = styles[5]))
#'   )
#' }
#' 
#' 
#' 

schema <- function(jsonedit = interactive(), ...) {
  s <- plotly.core::schema(jsonedit = FALSE)
  s$transforms <- transforms_schema()
  if (jsonedit) {
    if (!requireNamespace("listviewer", quietly = TRUE)) stop("schema(jsonedit = TRUE) needs the listviewer package", call. = FALSE)
    print(listviewer::jsonedit(s, mode = "form"))
  }
  invisible(s)
}

# The transforms plot_ly() applies (R/transforms.R), in the form of
# plotly.js 2's schema.
transforms_schema <- function() {
  enum <- function(values, dflt) list(valType = "enumerated", values = values, dflt = dflt)
  list(
    filter = list(attributes = list(
      enabled = list(valType = "boolean", dflt = TRUE), target = list(valType = "any", dflt = "x"),
      operation = enum(c("=", "!=", "<", ">=", ">", "<=", "[]", "()", "[)", "(]", "][", ")(", "](", ")[", "{}", "}{"), "="),
      value = list(valType = "any", dflt = 0), preservegaps = list(valType = "boolean", dflt = FALSE))),
    groupby = list(attributes = list(
      enabled = list(valType = "boolean", dflt = TRUE), groups = list(valType = "data_array"),
      nameformat = list(valType = "string"),
      styles = list(items = list(style = list(target = list(valType = "string"), value = list(valType = "any")))))),
    aggregate = list(attributes = list(
      enabled = list(valType = "boolean", dflt = TRUE), groups = list(valType = "string", dflt = "x"),
      aggregations = list(items = list(aggregation = list(
        target = list(valType = "string"), func = enum(names(LP_AGG_FUNCS), "first"),
        funcmode = enum(c("sample", "population"), "sample"), enabled = list(valType = "boolean", dflt = TRUE)))))),
    sort = list(attributes = list(
      enabled = list(valType = "boolean", dflt = TRUE), target = list(valType = "any", dflt = "x"),
      order = enum(c("ascending", "descending"), "ascending")))
  )
}
