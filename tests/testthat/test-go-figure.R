# plot_ly() objects are built by plotly_build() and rendered as plotly.core
# figures (R/go_figure.R)

rendered <- function(p) as_widget(p)$x

test_that("plot_ly() objects render as plotly.core figures", {
  p <- plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(cyl), type = "scatter", mode = "markers")
  fig <- as_go_figure(p)
  expect_s3_class(fig, "go_figure")
  expect_length(fig$data, 3)
  w <- as_widget(p)
  expect_s3_class(w, "htmlwidget")
  expect_identical(class(w)[[1]], "plotly_core")
  # the lazy widget renders through plotly.core's widget
  expect_identical(class(p)[1:2], c("plotly_core", "plotly"))
  expect_length(rendered(p)$data, 3)
})

test_that("plotly.core's add_<type>() functions add layers to plot_ly() objects lazily", {
  d <- data.frame(x = c("a", "b", "c"), y = c(3, 1, 2), g = c("u", "v", "u"))
  p <- plot_ly(d, x = ~x, y = ~y) |> add_bar(color = ~g)
  # nothing is evaluated until the plot is built
  expect_s3_class(p$x$attrs[[2]]$y, "formula")
  expect_identical(p$x$attrs[[2]]$type, "bar")
  b <- plotly_build(p)$x$data
  expect_length(b, 2)
  expect_true(all(vapply(b, `[[`, "", "type") == "bar"))
  # also when called as plotly.core::add_scatter()
  p2 <- plotly.core::add_scatter(plot_ly(d, x = ~x, y = ~y), mode = "markers")
  expect_identical(plotly_build(p2)$x$data[[1]]$type, "scatter")
})

test_that("trace helpers without a graph object are deprecated, once per session", {
  rm(list = ls(plotly:::deprecation_shown), envir = plotly:::deprecation_shown)
  p <- plot_ly(mtcars, x = ~wt, y = ~mpg)
  expect_warning(p1 <- add_markers(p), "add_scatter")
  expect_silent(add_markers(p))
  expect_identical(plotly_build(p1)$x$data[[1]]$mode, "markers")
  expect_warning(add_lines(p), "add_scatter")
  expect_warning(add_bars(p), "add_bar")
  expect_warning(add_boxplot(p), "add_box")
  # add_lines() keeps sorting by x
  d <- data.frame(x = c(3, 1, 2), y = 1:3)
  expect_equivalent(plotly_build(add_lines(plot_ly(d, x = ~x, y = ~y)))$x$data[[1]]$x, c(1, 2, 3))
  # plotly's own uses don't warn
  rm(list = ls(plotly:::deprecation_shown), envir = plotly:::deprecation_shown)
  expect_silent(plotly_build(plot_dendro(as.dendrogram(hclust(dist(USArrests[1:5, ]))))))
})

test_that("Mapbox traces and layout are upgraded to MapLibre map ones", {
  d <- data.frame(lat = c(40, 41), lon = c(-70, -71))
  p <- plot_mapbox(d, lat = ~lat, lon = ~lon, mode = "markers") |>
    layout(mapbox = list(style = "stamen-terrain", zoom = 3, accesstoken = "x")) |>
    config(mapboxAccessToken = "x")
  x <- rendered(p)
  expect_identical(x$data[[1]]$type, "scattermap")
  expect_identical(x$layout$map$style, "outdoors")
  expect_identical(x$layout$map$zoom, 3)
  expect_null(x$layout$mapbox)
  expect_null(x$config$mapboxAccessToken)
})

test_that("other removed plotly.js features are upgraded", {
  expect_identical(rendered(plot_ly(z = volcano, type = "heatmapgl"))$data[[1]]$type, "heatmap")
  polar <- rendered(plot_ly(r = 1:3, t = c(0, 90, 180), type = "scatter", mode = "lines"))
  expect_identical(polar$data[[1]]$type, "scatterpolar")
  expect_identical(polar$layout$polar$angularaxis$direction, "clockwise")
  expect_warning(
    x <- rendered(layout(plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers"), title = "a", titlefont = list(size = 20))),
    "titlefont"
  )
  expect_identical(x$layout$title$font$size, 20)
  tf <- rendered(plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers",
                         transforms = list(list(type = "groupby", groups = ~cyl))))
  expect_length(tf$data, 3)
  expect_false(is.null(schema(jsonedit = FALSE)$transforms$groupby))
})

test_that("config, linked highlighting and frames carry over to the figure", {
  p <- plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers") |>
    config(displayModeBar = FALSE, locale = "zh-CN", mathjax = "cdn")
  x <- rendered(p)
  expect_false(x$config$displayModeBar)
  expect_identical(x$config$locale, "zh-cn")
  expect_true(any(vapply(as_widget(p)$dependencies, function(d) identical(d$name, "mathjax"), logical(1))))
  h <- rendered(highlight(plot_ly(highlight_key(mtcars, ~cyl), x = ~wt, y = ~mpg, type = "scatter", mode = "markers"),
                          on = "plotly_selected"))
  expect_false(is.null(h$keys))
  expect_false(is.null(h$sets))
  expect_null(h$data[[1]]$key)
  a <- rendered(plot_ly(data.frame(x = rep(1:3, 2), y = 1:6, f = rep(1:2, each = 3)), x = ~x, y = ~y,
                        frame = ~f, type = "scatter", mode = "markers"))
  expect_length(a$frames, 2)
})
