test_that(
  "icicle trace basically works", {
    icicle <- yyjsonr::from_json_conn(
      "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/uniformtext_icicle.json", vectors_to_df = FALSE
    )
    expect_doppelganger(
      as_widget(icicle), 
      "uniformtext_icicle"
    )
})
