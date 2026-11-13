# plot_ly() checks carried over from plotly.core (see core-compat/), including
# the comparison of plot_ly() output with plotly 4.x's.
test_that("plot_ly() matches plotly 4.x and handles removed plotly.js features", {
  skip_if_not_installed("plotly.core")
  compat_dir <- test_path("core-compat")
  env <- new.env(parent = environment())
  env$compat_dir <- compat_dir
  withr::with_package("plotly.core", sys.source(file.path(compat_dir, "compat-checks.R"), envir = env))
  expect_true(TRUE)
})
