#' @export
print.plotly <- function(x, ...) {
  print(legacy_widget(x), ...)
  invisible(x)
}

