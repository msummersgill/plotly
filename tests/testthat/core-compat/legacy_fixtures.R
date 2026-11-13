# Reference output from the legacy plotly R package for tests/test-compat.R.
#
# Run from the package root with the legacy plotly package installed:
#   Rscript codegen/fixtures/legacy_fixtures.R
# Evaluates the cases in tests/fixtures/legacy-cases.R with plotly::plotly_build()
# and writes tests/fixtures/legacy-plotly.json.
source(file.path("tests", "fixtures", "legacy-cases.R"))
out <- list(plotly_version = as.character(utils::packageVersion("plotly")), figures = list())
for (nm in names(cases)) {
  p <- suppressWarnings(suppressMessages(eval(cases[[nm]], envir = new.env(parent = asNamespace("plotly")))))
  x <- suppressWarnings(suppressMessages(plotly::plotly_build(p)))$x
  out$figures[[nm]] <- jsonlite::fromJSON(plotly:::to_JSON(x[c("data", "layout", "frames")]), simplifyVector = FALSE)
}
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
           file.path("tests", "fixtures", "legacy-plotly.json"))
