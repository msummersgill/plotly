# Static image export is plotly.core's (save_image(), kaleido()); its own
# tests cover the Kaleido integration.
test_that("save_image() and kaleido() come from plotly.core", {
  expect_identical(save_image, plotly.core::save_image)
  expect_identical(kaleido, plotly.core::kaleido)
})

test_that("orca() and export() are deprecated in favour of save_image()", {
  p <- plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
  testthat::local_mocked_bindings(save_image = function(p, file, ...) invisible(file), .package = "plotly")
  expect_warning(orca(p, "plot.png"), "save_image")
  expect_warning(export(p, "plot.png"), "save_image")
  expect_error(orca_serve(), "save_image")
})
