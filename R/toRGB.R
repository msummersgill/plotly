#' Convert colors to rgba() strings
#'
#' `toRGB()` converts R colors (names, hex codes or rgba() strings) to
#' plotly.js `rgba()` strings, with an optional alpha (opacity) multiplier.
#' `showRGB()` draws swatches of `rgba()` colors.
#'
#' @param x A character vector of colors.
#' @param alpha Opacity multiplier between 0 and 1 (length 1 or the length of
#'   `x`).
#' @param ... Unused.
#' @return `toRGB()` returns a character vector; `showRGB()` returns `x`
#'   invisibly.
#' @export
#' @examples
#' toRGB("steelblue", 0.5)
toRGB <- function(x, alpha = 1) {
  if (length(x) == 0) return(x)
  if (any(x %in% "transparent")) return(x)
  if (length(x) != length(alpha)) {
    alpha <- uniq(alpha)
    if (length(x) != length(alpha) && length(alpha) != 1) stop("alpha must be of length 1 or the same length as x", call. = FALSE)
  }
  idx <- grepl("^rgba\\(", x) & alpha <= 1 & 0 <= alpha
  if (any(idx)) x[idx] <- rgb2hex(x[idx])
  if (is.character(x)) x[x == "NA"] <- NA
  alpha[is.na(alpha)] <- 1
  m <- grDevices::col2rgb(x, alpha = TRUE)
  m["alpha", ] <- round(alpha * m["alpha", ] / 255, 4)
  rgba <- sprintf("rgba(%s)", apply(m, 2, paste, collapse = ","))
  rgba[is.na(x)] <- "transparent"
  rgba
}


#' @rdname toRGB
#' @export
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

rgb2hex <- function(string) {
  vapply(strsplit(sub("rgba\\(", "", sub("\\)", "", string)), ","), function(v) {
    v <- as.numeric(v)
    grDevices::rgb(v[1], v[2], v[3], v[4] * 255, maxColorValue = 255)
  }, "")
}

