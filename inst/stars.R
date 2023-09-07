library(yyjsonr)
library(ggplot2)

stars <- yyjsonr::from_json_conn(
  "https://raw.githubusercontent.com/cpsievert/starline/master/data/stars.json", 
  vectors_to_df = FALSE
)

dats <- lapply(stars$repos, function(r) {
  starz <- unlist(r$stars$dates)
  starz <- starz[sort(names(starz))]
  data.frame(
    date = as.Date(names(starz)),
    value = cumsum(starz),
    repo = r$uri, 
    stringsAsFactors = FALSE
  )
})

d <- dplyr::bind_rows(dats)
ggplot(d, aes(date, value, color = repo)) + 
  geom_line() + ylab("Number of stars")
