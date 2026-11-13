# plotly.js is bundled with plotly.core (loaded once per page), so partial
# bundles are no longer used.
test_that("partial_bundle() and remove_typedarray_polyfill() are deprecated no-ops", {
  p <- plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")
  expect_warning(p2 <- partial_bundle(p, type = "basic"), "deprecated")
  expect_identical(p2, p)
  expect_warning(p3 <- remove_typedarray_polyfill(p), "deprecated")
  expect_identical(p3, p)
})
