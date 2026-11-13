shiny_defaults_set <- function(p) {
  p$x$shinyEvents <- p$x$shinyEvents %||% shiny_event_defaults()
  p
}

shiny_event_defaults <- function() {
  c(
    "plotly_hover", 
    "plotly_click", 
    "plotly_selected", 
    "plotly_relayout", 
    "plotly_brushed",
    "plotly_brushing",
    "plotly_clickannotation",
    "plotly_doubleclick", 
    "plotly_deselect", 
    "plotly_afterplot",
    "plotly_sunburstclick"
  )
}
