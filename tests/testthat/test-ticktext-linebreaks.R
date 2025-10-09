
# Compute margin
comp_margin <- function(gg, axisTickText) {
  plot <- ggfun("plot_clone")(gg)
  theme <- calculated_theme_elements(plot)
  
  pm <- unitConvert(theme$plot.margin, "pixels")
  gglayout <- list(
    margin = list(t = pm[[1]], r = pm[[2]], b = pm[[3]], l = pm[[4]])
  )
  axisTitle <- theme[["axis.title.y"]]
  axisObj <- list(
    ticktext = "djdfkjdfdklj",
    tickfont = text2font(theme[["axis.text"]], "width"),
    ticklen = unitConvert(theme$axis.ticks.length, "pixels", "width")
  )
  
  gglayout$margin[["l"]] + axisObj$ticklen +
    bbox(axisTickText, 0, axisObj$tickfont$size)[["width"]] +
    bbox("y", 90, unitConvert(axisTitle, "pixels", "width"))[["width"]]
}

expect_margin <- function(L, gg, ticktext) {
  margin_l <- comp_margin(gg, ticktext)
  expect_equal(round(L$layout$margin$l, 10), round(margin_l, 10))
}
