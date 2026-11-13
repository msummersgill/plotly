# IMPORTANT: fortify_sf() should only be used on collections of identical 
# geometries -- https://github.com/r-spatial/sf/issues/584

# Note how, in to_basic.GeomSf(), we split data into a list of data.frames
# (each data frame has at most one geometry type).

# Longitude/latitude (WGS84) coordinates, as needed by geo and map traces.
sf_crs <- function(x) {
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
sf_basic <- function(data) {
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
    structure(fortify_sf(data[kind %in% k, ]), sf_kind = k)
  }), kinds)
}

# TODO: speed this up by avoiding conversion redundant features (e.g. animation)
fortify_sf <- function(model) {
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
  # a data frame (with the attributes above), as ggplotly() expects
  attrs <- list(set = attr(d, "set"), plotly_groups = attr(d, "plotly_groups"))
  d <- as.data.frame(d)
  for (a in names(attrs)) if (!is.null(attrs[[a]])) attr(d, a) <- attrs[[a]]
  d
}

# the minimal set of attribute defaults
sf_default_attrs <- function(d) {
  switch(attr(d, "sf_kind"),
    polygon = list(mode = "lines", fill = "toself"),
    path = list(mode = "lines"),
    point = list(mode = "markers")
  )
}

fit_bounds <- function(p) {
  if (!any(vapply(p$x$data, function(tr) !is.null(tr[["_bbox"]]), logical(1)))) return(p)
  traces_on <- function(attr, id) lapply(p$x$data, function(tr) if (identical(id, tr[[attr]])) tr[["_bbox"]])
  for (id in unique(grep("^map", unlist(lapply(p$x$data, `[[`, "subplot")), value = TRUE))) {
    rng <- bboxes2range(traces_on("subplot", id), f = 0.01)
    if (!length(rng)) next
    p$x$fitBounds[[id]] <- I(c(min(rng$xrng), min(rng$yrng), max(rng$xrng), max(rng$yrng)))
    p$x$layout[[id]]$center$lat <- p$x$layout[[id]]$center$lat %||l% mean(rng$yrng)
    p$x$layout[[id]]$center$lon <- p$x$layout[[id]]$center$lon %||l% mean(rng$xrng)
  }
  for (id in unique(grep("^geo", unlist(lapply(p$x$data, `[[`, "geo")), value = TRUE))) {
    rng <- bboxes2range(traces_on("geo", id), f = 0.01)
    if (!length(rng)) next
    p$x$layout[[id]]$lataxis$range <- rng$yrng
    p$x$layout[[id]]$lonaxis$range <- rng$xrng
  }
  rows <- compact(lapply(p$x$data, function(tr) c(tr[["xaxis"]], tr[["yaxis"]])))
  for (r in rows) {
    bboxes <- lapply(p$x$data, function(tr) {
      if (identical(r[[1]], tr[["xaxis"]]) && identical(r[[2]], tr[["yaxis"]])) tr[["_bbox"]]
    })
    rng <- bboxes2range(bboxes, f = 0.01)
    if (!length(rng)) next
    xname <- sub("x", "xaxis", r[[1]])
    yname <- sub("y", "yaxis", r[[2]])
    eaxis <- list(showgrid = FALSE, zeroline = FALSE, ticks = "", showticklabels = FALSE)
    p$x$layout[[xname]] <- modify_list(eaxis, p$x$layout[[xname]])
    p$x$layout[[yname]] <- modify_list(eaxis, p$x$layout[[yname]])
    p$x$layout[[xname]]$title <- p$x$layout[[xname]]$title %|D|% NULL
    p$x$layout[[yname]]$title <- p$x$layout[[yname]]$title %|D|% NULL
    p$x$layout[[xname]]$scaleanchor <- r[[2]]
    p$x$layout[[xname]]$scaleratio <- cos(mean(rng$yrng) * pi / 180)
  }
  p
}

# find the x/y layout range of a collection of trace._bboxes
bboxes2range <- function(bboxes, ...) {
  if (sum(lengths(bboxes)) == 0) return(NULL)
  yrng <- c(
    min(unlist(lapply(bboxes, "[[", "ymin")), na.rm = TRUE),
    max(unlist(lapply(bboxes, "[[", "ymax")), na.rm = TRUE)
  )
  xrng <- c(
    min(unlist(lapply(bboxes, "[[", "xmin")), na.rm = TRUE),
    max(unlist(lapply(bboxes, "[[", "xmax")), na.rm = TRUE)
  )
  list(
    yrng = grDevices::extendrange(yrng, ...),
    xrng = grDevices::extendrange(xrng, ...)
  )
}
