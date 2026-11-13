#' Static image exporting via orca
#' 
#' This function is deprecated, use [save_image()] instead.
#' 
#' @param p a plotly object.
#' @param file output filename.
#' @param format the output format (png, jpeg, webp, svg, pdf, eps).
#' @param scale Sets the image scale. Applies to all output images.
#' @param width Sets the image width. If not set, defaults to `layout.width` value. 
#' Applies to all output images.
#' @param height Sets the image height. If not set, defaults to `layout.height` value. 
#' Applies to all output images.
#' @param mathjax whether or not to include MathJax (required to render [TeX]).
#' If `TRUE`, the PLOTLY_MATHJAX_PATH environment variable must be set and point 
#' to the location of MathJax (this variable is also used to render [TeX] in 
#' interactive graphs, see [config]).
#' @param parallel_limit Sets the limit of parallel tasks run.
#' @param verbose Turn on verbose logging on stdout.
#' @param debug Starts app in debug mode and turn on verbose logs on stdout.
#' @param safe Turns on safe mode: where figures likely to make browser window 
#' hang during image generating are skipped.
#' @param more_args additional arguments to pass along to system command. This is useful
#' for specifying display and/or electron options, such as `--enable-webgl` or `--disable-gpu`.
#' @param ... for `orca()`, additional arguments passed along to `processx::run`. For 
#' `orca_serve()`, additional arguments passed along to `processx::process`.
#' @export
#' @author Carson Sievert
#' @md
#' @rdname orca
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' # NOTE: in a headless environment, you may need to set `more_args="--enable-webgl"`
#' # to export webgl correctly
#' p <- plot_ly(z = ~volcano) %>% add_surface()
#' orca(p, "surface-plot.svg")
#' 
#' #' # launch the server
#' server <- orca_serve()
#' 
#' # export as many graphs as you'd like
#' server$export(qplot(1:10), "test1.pdf")
#' server$export(plot_ly(x = 1:10, y = 1:10), "test2.pdf")
#' 
#' # the underlying process is exposed as a field, so you
#' # have full control over the external process
#' server$process$is_alive()
#' 
#' # convenience method for closing down the server
#' server$close()
#' 
#' # remove the exported files from disk
#' unlink("test1.pdf")
#' unlink("test2.pdf")
#' }
#' 

orca <- function(p, file = "plot.png", format = tools::file_ext(file), 
                 scale = NULL, width = NULL, height = NULL, mathjax = FALSE,
                 parallel_limit = NULL, verbose = FALSE, debug = FALSE, 
                 safe = FALSE, more_args = NULL, ...) {
  deprecated("orca", "use save_image() (needs Kaleido) instead.")
  if (!identical(tolower(format), tolower(tools::file_ext(file)))) file <- paste0(tools::file_path_sans_ext(file), ".", format)
  save_image(p, file, width = width, height = height, scale = scale)
}

#' Orca image export server
#' 
#' @param port Sets the server's port number.
#' @param keep_alive Turn on keep alive mode where orca will (try to) relaunch server if process unexpectedly exits.
#' @param window_max_number Sets maximum number of browser windows the server can keep open at a given time.
#' @param request_limit Sets a request limit that makes orca exit when reached.
#' @param quiet Suppress all logging info.
#' 
#' @section Methods:
#' 
#' The `orca_serve()` function returns an object with two methods:
#' 
#' \describe{
#'   \item{\code{export(p, file = "plot.png", format = tools::file_ext(file), scale = NULL, width = NULL, height = NULL)}}{
#'     Export a static image of a plotly graph. Arguments found here are the same as those found in `orca()`
#'   }
#'   \item{\code{close()}}{Close down the orca server and kill the underlying node process.}
#' }
#' 
#' @section Fields:
#' 
#' The `orca_serve()` function returns an object with two fields:
#' 
#' \describe{
#'   \item{\code{port}}{The port number that the server is listening to.}
#'   \item{\code{process}}{An R6 class for controlling and querying the underlying node process.}
#' }
#' 
#' @export
#' @rdname orca

orca_serve <- function(port = 5151, mathjax = FALSE, safe = FALSE, request_limit = NULL,
                       keep_alive = TRUE, window_max_number = NULL, quiet = FALSE, 
                       debug = FALSE, more_args = NULL, ...) defunct("orca_serve", "orca is no longer supported; use save_image() instead.")
