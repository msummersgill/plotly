#' Animation options
#'
#' Controls animations of [plot_ly()] objects with a `frame` mapping:
#' `animation_opts()` sets timing, `animation_slider()` and
#' `animation_button()` modify (or hide) the slider and play button.
#'
#' @param p A [plot_ly()] object.
#' @param frame Milliseconds per frame (including the transition).
#' @param transition Milliseconds of the transition between frames.
#' @param easing Transition easing (e.g. `"linear"`, `"cubic-in-out"`).
#' @param redraw Redraw the plot at the end of each transition?
#' @param mode `"immediate"`, `"next"` or `"afterall"`.
#' @param hide Remove the slider?
#' @param label Button label.
#' @param ... Slider or button attributes.
#' @return The modified plot.
#' @export
#' @examples
#' df <- data.frame(x = rep(1:5, 3), y = runif(15), t = rep(1:3, each = 5))
#' df |> plot_ly(x = ~x, y = ~y, frame = ~t) |> animation_opts(1000, easing = "elastic")
animation_opts <- function(p, frame = 500, transition = frame, easing = "linear", redraw = TRUE, mode = "immediate") {
  p$animation <- lp_animation_format(frame, transition, easing, redraw, mode)
  p
}


#' @rdname animation_opts
#' @export
animation_slider <- function(p, hide = FALSE, ...) {
  p <- plotly_build(p)
  is_slider <- vapply(p$x$layout$sliders, function(s) inherits(s, "aniSlider"), logical(1))
  if (hide) {
    p$x$layout$sliders[is_slider] <- NULL
    return(p)
  }
  i <- which(is_slider)
  p$x$layout$sliders[[i]] <- structure(lp_modify_list(unclass(p$x$layout$sliders[[i]]), list(...)), class = "aniSlider")
  p
}


#' @rdname animation_opts
#' @export
animation_button <- function(p, ..., label) {
  p <- plotly_build(p)
  i <- which(vapply(p$x$layout$updatemenus, function(s) inherits(s, "aniButton"), logical(1)))
  if (!missing(label)) p$x$layout$updatemenus[[i]]$buttons[[1]]$label <- label
  p$x$layout$updatemenus[[i]] <- structure(lp_modify_list(unclass(p$x$layout$updatemenus[[i]]), list(...)), class = "aniButton")
  p
}


lp_animation_button_supply <- function(p) {
  menus <- p$x$layout$updatemenus
  is_btn <- vapply(menus, function(s) inherits(s, "aniButton"), logical(1))
  idx <- if (sum(is_btn) == 1) which(is_btn) else length(menus) + 1
  opts <- p$animation %||l% lp_animation_defaults()
  p$x$layout$updatemenus[[idx]] <- structure(list(
    type = "buttons", direction = "right", showactive = FALSE, y = 0, x = 0, yanchor = "top", xanchor = "right",
    pad = list(t = 60, r = 5),
    buttons = list(list(label = "Play", method = "animate",
                        args = list(NULL, lp_modify_list(list(fromcurrent = TRUE, mode = "immediate"), opts))))
  ), class = "aniButton")
  p
}


lp_animation_slider_supply <- function(p, ...) {
  sliders <- p$x$layout$sliders
  is_slider <- vapply(sliders, function(s) inherits(s, "aniSlider"), logical(1))
  idx <- if (sum(is_slider) == 1) which(is_slider) else length(sliders) + 1
  opts <- p$animation %||l% lp_animation_defaults()
  steps <- lapply(p$x$frames, function(f) list(method = "animate", args = list(list(f$name), opts), label = f$name, value = f$name))
  slider <- lp_modify_list(if (any(is_slider)) unclass(sliders[[which(is_slider)]]) else list(), compact(list(...)))
  slider$steps <- steps
  slider$visible <- slider$visible %||l% TRUE
  slider$pad$t <- slider$pad[["t"]] %||l% 40
  p$x$layout$sliders[[idx]] <- structure(slider, class = "aniSlider")
  p
}


