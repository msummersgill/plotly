# plot_ly() cases evaluated with both the legacy plotly package (fixtures) and
# plotly.core (tests/test-compat.R). Use |> rather than %>%.
cases <- list(
  markers = quote(plot_ly(mtcars, x = ~wt, y = ~mpg)),
  color_discrete = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(cyl))),
  color_numeric = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~hp)),
  size = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, size = ~hp, type = "scatter", mode = "markers")),
  symbol = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, symbol = ~factor(gear), type = "scatter", mode = "markers")),
  colors_palette = quote(plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, colors = "Set1", type = "scatter", mode = "markers")),
  colors_named = quote(plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, colors = c(setosa = "red", versicolor = "blue", virginica = "green"), type = "scatter", mode = "markers")),
  lines_linetype = quote(plot_ly(data.frame(x = rep(1:5, 2), y = c(1:5, 5:1), g = rep(c("a", "b"), each = 5)), x = ~x, y = ~y, linetype = ~g) |> add_lines()),
  add_markers_lines = quote(plot_ly(mtcars, x = ~wt, y = ~mpg) |> add_markers(color = I("red")) |> add_lines(data = data.frame(wt = c(2, 5), mpg = c(30, 12)), inherit = FALSE, x = ~wt, y = ~mpg)),
  bars = quote(plot_ly(data.frame(x = c("a", "b", "c"), y = c(3, 1, 2)), x = ~x, y = ~y, type = "bar")),
  bars_color = quote(plot_ly(mtcars, x = ~factor(cyl), y = ~mpg, color = ~factor(am), type = "bar")),
  histogram = quote(plot_ly(mtcars, x = ~mpg) |> add_histogram()),
  boxplot = quote(plot_ly(mtcars, x = ~factor(cyl), y = ~mpg) |> add_boxplot()),
  heatmap = quote(plot_ly(z = volcano[1:10, 1:10], type = "heatmap")),
  surface = quote(plot_ly(z = volcano[1:10, 1:10]) |> add_surface()),
  pie = quote(plot_ly(data.frame(l = c("a", "b", "c"), v = c(1, 2, 3)), labels = ~l, values = ~v, type = "pie")),
  layout = quote(plot_ly(mtcars, x = ~wt, y = ~mpg) |> layout(title = "Cars", xaxis = list(title = "Weight"), yaxis = list(range = c(0, 40)))),
  segments = quote(plot_ly(data.frame(x = 1:3, y = 1:3, xe = 2:4, ye = 3:1), x = ~x, y = ~y, xend = ~xe, yend = ~ye) |> add_segments()),
  ribbons = quote(plot_ly(data.frame(x = 1:5, lo = 1:5, hi = 3:7), x = ~x, ymin = ~lo, ymax = ~hi) |> add_ribbons()),
  polygons = quote(plot_ly(data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)), x = ~x, y = ~y) |> add_polygons()),
  text = quote(plot_ly(mtcars[1:5, ], x = ~wt, y = ~mpg, text = ~rownames(mtcars)[1:5]) |> add_text()),
  split = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, split = ~cyl, type = "scatter", mode = "lines")),
  frame = quote(plot_ly(data.frame(x = rep(1:3, 3), y = c(1:3, 2:4, 3:5), t = rep(1:3, each = 3)), x = ~x, y = ~y, frame = ~t, type = "scatter", mode = "markers")),
  scatter3d = quote(plot_ly(mtcars, x = ~wt, y = ~hp, z = ~mpg, color = ~factor(cyl))),
  annotations = quote(plot_ly(mtcars, x = ~wt, y = ~mpg) |> add_annotations(text = "heavy", x = 5, y = 15)),
  style = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(cyl)) |> style(marker.size = 15, traces = 2)),
  subplot = quote(subplot(plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers"), plot_ly(mtcars, x = ~hp, y = ~mpg, type = "scatter", mode = "markers"), shareY = TRUE, titleX = TRUE)),
  subplot_rows = quote(subplot(plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers"), plot_ly(mtcars, x = ~hp, type = "histogram"), nrows = 2, heights = c(0.7, 0.3))),
  stroke = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, stroke = ~factor(cyl), type = "scatter", mode = "markers", span = I(3))),
  alpha = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = I("blue"), alpha = 0.3, type = "scatter", mode = "markers")),
  hide_legend = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(cyl)) |> hide_legend()),
  colorbar = quote(plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~hp) |> colorbar(title = "HP"))
)

# add_sf() cases (need the sf package; North Carolina counties shipped with sf)
nc5 <- quote(sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5, ])
sf_cases <- list(
  sf_polygons = bquote(plot_ly(.(nc5))),
  sf_split = bquote(plot_ly(.(nc5)) |> add_sf(split = ~NAME, color = I("gray40"), text = ~NAME, hoverinfo = "text")),
  sf_color = bquote(plot_ly(.(nc5), color = ~AREA)),
  sf_lines = bquote(plot_ly(sf::st_cast(.(nc5), "MULTILINESTRING"))),
  sf_points = bquote(plot_ly(sf::st_centroid(.(nc5)))),
  sf_geo = bquote(plot_geo(.(nc5)) |> add_sf()),
  sf_markers_layer = bquote(plot_ly(.(nc5)) |> add_sf() |> add_sf(data = sf::st_centroid(.(nc5)), color = I("red")))
)
if (requireNamespace("sf", quietly = TRUE)) cases <- c(cases, sf_cases)
