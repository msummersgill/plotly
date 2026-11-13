#' Tools for working with plotly's REST API (v2)
#' 
#' Convenience functions for working with version 2 of plotly's REST API.
#' Upload R objects to a plotly account via `api_create()` and download
#' plotly objects via `api_download_plot()`/`api_download_grid()`.
#' For anything else, use `api()`.
#' 
#' @param id a filename id. 
#' @param username a plotly username.
#' 
#' @param x An R object to hosted on plotly's web platform. 
#' Can be a plotly/ggplot2 object or a \link{data.frame}.
#' @param filename character vector naming file(s). If `x` is a plot,
#' can be a vector of length 2 naming both the plot AND the underlying grid.
#' @param fileopt character string describing whether to "overwrite" existing 
#' files or ensure "new" file(s) are always created.
#' @param sharing If 'public', anyone can view this graph. It will appear in 
#' your profile and can appear in search engines. You do not need to be
#' logged in to Plotly to view this chart.
#' If 'private', only you can view this plot. It will not appear in the
#' Plotly feed, your profile, or search engines. You must be logged in to 
#' Plotly to view this graph. You can privately share this graph with other 
#' Plotly users in your online Plotly account and they will need to be logged 
#' in to view this plot.
#' If 'secret', anyone with this secret link can view this chart. It will
#' not appear in the Plotly feed, your profile, or search engines. 
#' If it is embedded inside a webpage or an IPython notebook, anybody who is 
#' viewing that page will be able to view the graph. 
#' You do not need to be logged in to view this plot.
#' 
#' @param endpoint the endpoint (i.e., location) for the request. 
#' To see a list of all available endpoints, call `api()`.
#' Any relevant query parameters should be included here (see examples).
#' @param verb name of the HTTP verb to use (as in, [httr::RETRY()]).
#' @param body body of the HTTP request(as in, [httr::RETRY()]).
#' If this value is not already converted to JSON 
#' (via [jsonlite::toJSON()]), it uses the internal `to_JSON()`
#' to ensure values are "automatically unboxed" (i.e., vec.
#'
#' @param ... For `api()`, these arguments are passed onto 
#' [httr::RETRY()]. For `api_create()`, these arguments are
#' included in the body of the HTTP request.
#' 
#' @export
#' @rdname api
#' @author Carson Sievert
#' @seealso [signup()]
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' 
#' # ------------------------------------------------------------
#' # api_create() makes it easy to upload ggplot2/plotly objects
#' # and/or data frames to your plotly account
#' # ------------------------------------------------------------
#' 
#' # A data frame creates a plotly "grid". Printing one will take you 
#' # to the it's web address so you can start creating!
#' (m <- api_create(mtcars))
#' 
#' # A plotly/ggplot2 object create a plotly "plot".
#' p <- plot_ly(mtcars, x = ~factor(vs))
#' (r <- api_create(p))
#' 
#' # api_create() returns metadata about the remote "file". Here is
#' # one way you could use that metadata to download a plot for local use:
#' fileID <- strsplit(r$file$fid, ":")[[1]]
#' layout(
#'   api_download_plot(fileID[2], fileID[1]),
#'   title = sprintf("Local version of <a href='%s'>this</a> plot", r$file$web_url)
#' )
#'
#' ------------------------------------------------------------
#' # The api() function provides a low-level interface for performing 
#' # any action at any endpoint! It always returns a list.
#' # ------------------------------------------------------------
#' 
#' # list all the endpoints
#' api()
#' 
#' # search the entire platform!
#' api("search?q=overdose")
#' api("search?q=plottype:pie trump fake")
#' 
#' # these examples will require a user account
#' usr <- Sys.getenv("plotly_username", NA)
#' if (!is.na(usr)) {
#'   # your account info
#'   api(sprintf("users/%s", usr))
#'   # your folders/files
#'   api(sprintf("folders/home?user=%s", usr))
#' }
#' 
#' # Retrieve a specific file
#' api("files/cpsievert:14681")
#'
#' # change the filename
#' # (note: this won't work unless you have proper credentials to the relevant account)
#' api("files/cpsievert:14681", "PATCH", list(filename = "toy file"))
#'
#' # Copy a file
#' api("files/cpsievert:14681/copy", "POST")
#'
#' # Create a folder
#' api("folders", "POST", list(path = "/starts/at/root/and/ends/here"))
#' 
#' }
#' 
#' @export
api_create <- function(x = last_plot(), filename = NULL, 
                       fileopt = c("overwrite", "new"),
                       sharing = c("public", "private", "secret"), ...) defunct("api_create", LP_CHART_STUDIO)



#' @rdname api
#' @export
api_download_plot <- function(id, username) defunct("api_download_plot", LP_CHART_STUDIO)


#' @rdname api
#' @export
api_download_grid <- function(id, username) defunct("api_download_grid", LP_CHART_STUDIO)


#' @rdname api
#' @export
api <- function(endpoint = "/", verb = "GET", body = NULL, ...) defunct("api", LP_CHART_STUDIO)
