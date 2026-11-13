#' Convert R colours to RGBA hexadecimal colour values
#' @param x see the `col` argument in `col2rgb` for valid specifications
#' @param alpha alpha channel on 0-1 scale
#' @return hexadecimal colour value (if is.na(x), return "transparent" for compatibility with Plotly)
#' @export
#' @seealso [showRGB()]
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' toRGB("steelblue") 
#' # [1] "rgba(70,130,180,1)"
#' 
#' m <- list(
#'   color = toRGB("red"),
#'   line = list(
#'     color = toRGB("black"), 
#'     width = 19
#'   )
#' )
#' 
#' plot_ly(x = 1, y = 1, marker = m)
#' 
toRGB <- function(x, alpha = 1) {
  if (length(x) == 0) return(x)
  if (any(x %in% "transparent")) return(x)
  # allow x/alpha vector(s), but only sensible cases...
  if (length(x) != length(alpha)) {
    # try to reduce alpha to length 1 (in case x is of length 1)
    alpha <- uniq(alpha)
    if (length(x) != length(alpha) && length(alpha) != 1) {
      stop(
        "alpha must be of length 1 or the same length as x",
        call. = FALSE
      )
    }
  }
  # add alpha to already converted "rgb(x,y,z)" codes
  idx <- grepl("^rgba\\(", x) & alpha <= 1 & 0 <= alpha
  if (any(idx)) {
    x[idx] <- rgb2hex(x[idx])
  }
  # for some reason ggplot2 has "NA" in some place (instead of NA)
  if (is.character(x)) {
    x[x == "NA"] <- NA
  }
  # as of ggplot2 version 1.1, an NA alpha is treated as though it's 1
  alpha[is.na(alpha)] <- 1
  rgb_matrix <- grDevices::col2rgb(x, alpha = TRUE)
  # multiply the existing alpha with specified alpha (both on 0-1 scale)
  rgb_matrix["alpha", ] <- alpha * scales::rescale(
    rgb_matrix["alpha", ], from = c(0, 255)
  )
  rgb_matrix["alpha", ] <- round(rgb_matrix["alpha", ], 4)
  rgba <- sprintf("rgba(%s)", apply(rgb_matrix, 2, paste, collapse = ","))
  
  rgba[is.na(x)] <- "transparent"
  rgba
}


#' View colors already formatted by toRGB()
#' 
#' Useful for viewing colors after they've been converted to plotly.js'
#' color format -- "rgba(255, 255, 255, 1)"
#' 
#' @param x character string specifying color(s).
#' @param ... arguments passed along to `scales::show_col`.
#' @export
#' @author Carson Sievert
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' showRGB(toRGB(colors()), labels = FALSE)
showRGB <- function(x, ...) {
  if (!all(grepl("^rgba", x))) stop("All values must begin with rgba", call. = FALSE)
  cols <- vapply(strsplit(x, ","), function(y) {
    v <- as.numeric(gsub("rgba\\(|\\)", "", y))
    grDevices::rgb(v[1] / 255, v[2] / 255, v[3] / 255, v[4])
  }, "")
  n <- length(cols)
  graphics::plot.new()
  graphics::plot.window(c(0, n), c(0, 1))
  graphics::rect(seq_len(n) - 1, 0, seq_len(n), 1, col = cols, border = "white")
  invisible(x)
}

# take a 'plotly color' and produce a hex code
rgb2hex <- function(string = "rgba(255,255,255,1)") {
  vals <- sub("rgba\\(", "", sub("\\)", "", string))
  valz <- strsplit(vals, ",")
  sapply(valz, function(x) {
    x <- setNames(as.numeric(x), c("red", "green", "blue", "alpha"))
    x[["alpha"]] <- x[["alpha"]] * 255
    do.call(grDevices::rgb, c(x, list(maxColorValue = 255)))
  })
}
