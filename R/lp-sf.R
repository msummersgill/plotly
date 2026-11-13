# Simple features (sf) layers for plot_ly() objects: a port of the legacy
# plotly package's add_sf(), to_basic.GeomSf(), fortify_sf(),
# sf_default_attrs() and fit_bounds() (R/add.R, R/layers2traces.R, R/sf.R,
# R/utils.R in plotly.R), without dplyr and rlang. lp_sf_crs() is written for
# this package (the legacy st_cast_crs() came from the leaflet package).

#' Add a simple features (sf) layer
#'
#' Draws the geometries of an [sf](https://r-spatial.github.io/sf/) object:
#' polygons as filled outlines, lines as paths and points as markers (one
#' trace per geometry kind). Works with [plot_ly()] (cartesian, with an equal
#' aspect ratio), [plot_geo()] and [plot_mapbox()]; `plot_ly()` with sf data
#' and no layers adds one automatically. Requires the sf package.
#'
#' For maps, data are transformed to longitude/latitude (WGS84, EPSG:4326)
#' when they use another coordinate reference system (with a warning); data
#' with none are assumed to be EPSG:4326. The initial view fits the data.
#'
#' @param p A [plot_ly()] object.
#' @param ... Trace attributes (e.g. `color`, `split`, `text`, `hoverinfo`).
#' @param x,y Coordinates (default: the geometries' coordinates).
#' @param data An sf object (default: the plot's data).
#' @param inherit Inherit attributes from [plot_ly()]?
#' @return The plot.
#' @export
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc |> plot_ly()
#' nc |> plot_ly() |> add_sf(color = ~AREA, split = ~NAME, hoverinfo = "text", text = ~NAME)
#' nc |> plot_geo() |> add_sf(color = I("gray50"))
add_sf <- function(p, ..., x = ~x, y = ~y, data = NULL, inherit = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("add_sf() needs the sf package: install.packages(\"sf\")", call. = FALSE)
  }
  dat <- plotly_data(add_data(p, data))
  if (!inherits(dat, "sf")) {
    stop("The `data` for an `add_sf()` layer must be an sf object, not an object of class: ",
         class(dat)[1], call. = FALSE)
  }
  if (lp_is_geo(p)) dat <- lp_sf_crs(dat)
  bbox <- as.list(sf::st_bbox(dat))
  set <- attr(dat, "set")
  d <- lp_sf_basic(dat)
  attrz <- if (inherit) lp_modify_list(unclass(p$x$attrs[[1]]), list(...)) else list(...)
  for (i in seq_along(d)) {
    attrs <- lp_modify_list(lp_sf_default_attrs(d[[i]]), attrz)
    # scatter3d doesn't support fill
    if ("z" %in% names(attrs)) attrs$fill <- NULL
    args <- list(p = p, class = "plotly_sf", x = x, y = y, `_bbox` = bbox, set = set,
                 data = d[[i]], inherit = inherit)
    args <- lp_modify_list(args, attrs)
    p <- do.call(lp_add_classed, args)
  }
  p
}

# Longitude/latitude (WGS84) coordinates, as needed by geo and map traces.
lp_sf_crs <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    warning("Missing coordinate reference system (crs). Defaulting to EPSG:4326", call. = FALSE)
    return(sf::st_set_crs(x, 4326))
  }
  if (isTRUE(sf::st_is_longlat(x)) && identical(crs, sf::st_crs(4326))) return(x)
  if (isTRUE(sf::st_is_longlat(x)) && grepl("WGS84|WGS 84", crs$wkt %||l% "")) return(x)
  warning("Geo and map traces need longitude/latitude coordinates on the WGS84 datum (EPSG:4326), ",
          "but the crs is '", crs$input %||l% "unknown", "'. Attempting transformation to the target coordinate system.", call. = FALSE)
  sf::st_transform(x, 4326)
}

LP_SF_TYPES <- c(
  TRIANGLE = "polygon", TIN = "polygon", POLYHEDRALSURFACE = "polygon", SURFACE = "polygon",
  CURVE = "path", MULTISURFACE = "polygon", MULTICURVE = "path", CURVEPOLYGON = "polygon",
  COMPOUNDCURVE = "path", CIRCULARSTRING = "path", MULTIPOLYGON = "polygon", MULTILINESTRING = "path",
  MULTIPOINT = "point", POLYGON = "polygon", LINESTRING = "path", POINT = "point"
)

# One data frame of coordinates per geometry kind (polygon, path, point),
# each holding the features' other columns.
lp_sf_basic <- function(data) {
  data <- sf::st_as_sf(data)
  geom_type <- as.character(sf::st_geometry_type(data))
  # expand collections into one row per feature
  if ("GEOMETRYCOLLECTION" %in% geom_type) {
    data <- sf::st_cast(data)
    geom_type <- as.character(sf::st_geometry_type(data))
  }
  kind <- LP_SF_TYPES[geom_type]
  kinds <- intersect(c("path", "point", "polygon"), kind)
  stats_named(lapply(kinds, function(k) {
    structure(lp_fortify_sf(data[kind %in% k, ]), sf_kind = k)
  }), kinds)
}

# Coordinates of identical geometries joined with the feature data. Lines
# and polygons are grouped by the feature and ring ids (l1, l2, l3).
lp_fortify_sf <- function(model) {
  coords <- sf::st_coordinates(sf::st_geometry(model))
  colnames(coords) <- tolower(colnames(coords))
  lcols <- grep("^l", colnames(coords))
  set <- attr(model, "set")
  model <- sf::st_drop_geometry(model)
  coords <- data.table::as.data.table(coords)
  model <- data.table::as.data.table(model)
  if (!length(lcols)) {
    d <- cbind(model, coords)
  } else {
    nms <- intersect(names(model), colnames(coords))
    if (length(nms)) {
      warning(sprintf("Found columns named: '%s' ", paste(nms, collapse = "', '")),
              "in your data. These names conflict with auto-generated sf coordinates ",
              "it might be a good idea to change these names.", call. = FALSE)
    }
    # the last coordinate column is the feature (row) id
    d <- cbind(coords, model[coords[[ncol(coords)]], setdiff(names(model), names(coords)), with = FALSE])
    group_var <- as.list(d)[names(d) %in% c("group", "l1", "l2", "l3")]
    data.table::set(d, j = "group", value = Reduce(function(...) paste(..., sep = "-"), group_var))
    attr(d, "plotly_groups") <- "group"
  }
  attr(d, "set") <- set
  d
}

lp_sf_default_attrs <- function(d) {
  switch(attr(d, "sf_kind"),
    polygon = list(mode = "lines", fill = "toself"),
    path = list(mode = "lines"),
    point = list(mode = "markers")
  )
}

# Build step: initial view from the sf layers' bounding boxes (map view,
# geo lon/lat ranges, or equal-aspect cartesian axes without grids).
lp_fit_bounds <- function(p) {
  if (!any(vapply(p$x$data, function(tr) !is.null(tr[["_bbox"]]), logical(1)))) return(p)
  traces_on <- function(attr, id) lapply(p$x$data, function(tr) if (identical(id, tr[[attr]])) tr[["_bbox"]])
  for (id in unique(grep("^map", unlist(lapply(p$x$data, `[[`, "subplot")), value = TRUE))) {
    rng <- lp_bboxes2range(traces_on("subplot", id), f = 0.01)
    if (!length(rng)) next
    p$x$fitBounds[[id]] <- I(c(min(rng$xrng), min(rng$yrng), max(rng$xrng), max(rng$yrng)))
    p$x$layout[[id]]$center$lat <- p$x$layout[[id]]$center$lat %||l% mean(rng$yrng)
    p$x$layout[[id]]$center$lon <- p$x$layout[[id]]$center$lon %||l% mean(rng$xrng)
  }
  for (id in unique(grep("^geo", unlist(lapply(p$x$data, `[[`, "geo")), value = TRUE))) {
    rng <- lp_bboxes2range(traces_on("geo", id), f = 0.01)
    if (!length(rng)) next
    p$x$layout[[id]]$lataxis$range <- rng$yrng
    p$x$layout[[id]]$lonaxis$range <- rng$xrng
  }
  rows <- compact(lapply(p$x$data, function(tr) c(tr[["xaxis"]], tr[["yaxis"]])))
  for (r in rows) {
    bboxes <- lapply(p$x$data, function(tr) {
      if (identical(r[[1]], tr[["xaxis"]]) && identical(r[[2]], tr[["yaxis"]])) tr[["_bbox"]]
    })
    rng <- lp_bboxes2range(bboxes, f = 0.01)
    if (!length(rng)) next
    xname <- sub("x", "xaxis", r[[1]])
    yname <- sub("y", "yaxis", r[[2]])
    eaxis <- list(showgrid = FALSE, zeroline = FALSE, ticks = "", showticklabels = FALSE)
    p$x$layout[[xname]] <- lp_modify_list(eaxis, p$x$layout[[xname]])
    p$x$layout[[yname]] <- lp_modify_list(eaxis, p$x$layout[[yname]])
    p$x$layout[[xname]]$title <- p$x$layout[[xname]]$title %|D|% NULL
    p$x$layout[[yname]]$title <- p$x$layout[[yname]]$title %|D|% NULL
    p$x$layout[[xname]]$scaleanchor <- r[[2]]
    p$x$layout[[xname]]$scaleratio <- cos(mean(rng$yrng) * pi / 180)
  }
  p
}

lp_bboxes2range <- function(bboxes, ...) {
  if (sum(lengths(bboxes)) == 0) return(NULL)
  get <- function(k) unlist(lapply(bboxes, `[[`, k))
  list(yrng = grDevices::extendrange(range(get("ymin"), get("ymax"), na.rm = TRUE), ...),
       xrng = grDevices::extendrange(range(get("xmin"), get("xmax"), na.rm = TRUE), ...))
}
