#' Wind data
#'
#' Description TBD.
#'
#' @format A data frame with three variables: `r`, `t`,
#'   `nms`.
"wind"

#' Mic data
#'
#' Description TBD.
#'
#' @format A data frame with three variables: `r`, `t`,
#'   `nms`.
"mic"

#' Hobbs data
#'
#' Description TBD.
#'
#' @format A data frame with three variables: `r`, `t`,
#'   `nms`.
"hobbs"


#' Minnesotan Indian Reservation Lands
#'
#' @references \url{https://www.dot.state.mn.us/maps/gdma/gis-data.html}
#'
#' @format An sf data frame with 13 features and 5 fields
#' 
"res_mn"

#' US economic time series
#'
#' This dataset was produced from US economic time series data available from
#' \url{https://fred.stlouisfed.org/}. `economics` is in "wide"
#' format, `economics_long` is in "long" format.
#'
#' @format A data frame with 574 rows and 6 variables:
#' \describe{
#'   \item{date}{Month of data collection}
#'   \item{pce}{personal consumption expenditures, in billions of dollars,
#'     \url{https://fred.stlouisfed.org/series/PCE}}
#'   \item{pop}{total population, in thousands,
#'     \url{https://fred.stlouisfed.org/series/POP}}
#'   \item{psavert}{personal savings rate,
#'     \url{https://fred.stlouisfed.org/series/PSAVERT/}}
#'   \item{uempmed}{median duration of unemployment, in weeks,
#'     \url{https://fred.stlouisfed.org/series/UEMPMED}}
#'   \item{unemploy}{number of unemployed in thousands,
#'     \url{https://fred.stlouisfed.org/series/UNEMPLOY}}
#' }
#'
"economics"

#' Housing sales in TX
#'
#' Information about the housing market in Texas provided by the TAMU
#' real estate center, \url{https://trerc.tamu.edu/}.
#'
#' @format A data frame with 8602 observations and 9 variables:
#' \describe{
#' \item{city}{Name of multiple listing service (MLS) area}
#' \item{year,month,date}{Date}
#' \item{sales}{Number of sales}
#' \item{volume}{Total value of sales}
#' \item{median}{Median sale price}
#' \item{listings}{Total active listings}
#' \item{inventory}{"Months inventory": amount of time it would take to sell
#'   all current listings at current pace of sales.}
#' }
"txhousing"

#' Prices of over 50,000 round cut diamonds
#'
#' A dataset containing the prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price in US dollars ($326--$18,823)}
#'   \item{carat}{weight of the diamond (0.2--5.01)}
#'   \item{cut}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#'   \item{color}{diamond colour, from D (best) to J (worst)}
#'   \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#'   \item{x}{length in mm (0--10.74)}
#'   \item{y}{width in mm (0--58.9)}
#'   \item{z}{depth in mm (0--31.8)}
#'   \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#'   \item{table}{width of top of diamond relative to widest point (43--95)}
#' }
"diamonds"

