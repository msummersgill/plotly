economics <- read.csv("data-raw/economics.csv")
economics$date <- as.Date(economics$date)
save(economics, file = "data/economics.rda")
