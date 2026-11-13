# Arrays of a trace that transforms subset: atomic vectors (at any depth)
# with one value per point. Lists named here are never per-point arrays.
LP_TRANSFORM_SKIP <- c("transforms", "colorscale", "dimensions", "colorbar", "line_shape")

# Paths (character vectors) of the trace's per-point arrays of length n.
lp_point_arrays <- function(tr, n, path = character()) {
  out <- list()
  for (k in names(tr)) {
    if (!nzchar(k) || k %in% LP_TRANSFORM_SKIP) next
    v <- tr[[k]]
    if (is.atomic(v) && !is.null(v) && length(v) == n && (n > 1 || inherits(v, "AsIs"))) {
      out[[length(out) + 1]] <- c(path, k)
    } else if (is.list(v) && !is.data.frame(v) && !is.null(names(v))) {
      out <- c(out, lp_point_arrays(v, n, c(path, k)))
    }
  }
  out
}

lp_get_path <- function(x, path) {
  for (k in path) x <- if (is.list(x)) x[[k]] else NULL
  x
}

lp_set_path <- function(x, path, value) {
  if (length(path) == 1) {
    x[[path]] <- value
  } else {
    x[[path[1]]] <- lp_set_path(x[[path[1]]] %||l% list(), path[-1], value)
  }
  x
}

# The values a transform refers to: an attribute name ("x", "marker.color")
# or an array.
lp_transform_target <- function(tr, target, default) {
  target <- target %||l% default
  if (is.character(target) && length(target) == 1) {
    return(lp_get_path(tr, strsplit(target, ".", fixed = TRUE)[[1]]))
  }
  if (is.list(target)) target <- unlist(target)
  if (is.factor(target)) target <- as.character(target)
  target
}

# Every per-point array of `tr` (length of `ref`) indexed by `idx`.
lp_subset_trace <- function(tr, n, idx) {
  for (path in lp_point_arrays(tr, n)) {
    v <- lp_get_path(tr, path)
    tr <- lp_set_path(tr, path, v[idx])
  }
  tr
}

lp_apply_transforms <- function(p) {
  if (!any(vapply(p$x$data, function(tr) length(tr[["transforms"]]) > 0, logical(1)))) return(p)
  n_traces <- length(p$x$data)
  p$x$data <- unlist(lapply(p$x$data, function(tr) {
    tfs <- tr[["transforms"]]
    tr[["transforms"]] <- NULL
    traces <- list(tr)
    for (tf in tfs) {
      if (!is.list(tf) || isFALSE(tf$enabled)) next
      type <- tf$type %||l% "filter"
      f <- switch(type,
        filter = lp_transform_filter, groupby = lp_transform_groupby,
        aggregate = lp_transform_aggregate, sort = lp_transform_sort,
        {
          warning("Unknown transform type '", type, "' ignored", call. = FALSE)
          NULL
        }
      )
      if (!is.null(f)) traces <- unlist(lapply(traces, f, tf = tf, n_traces = n_traces), recursive = FALSE)
    }
    traces
  }), recursive = FALSE)
  p
}

lp_transform_filter <- function(tr, tf, ...) {
  target <- lp_transform_target(tr, tf$target, "x")
  n <- length(target)
  if (!n) return(list(tr))
  op <- tf$operation %||l% "="
  value <- unlist(tf$value %||l% 0)
  num <- function(v) suppressWarnings(as.numeric(v))
  numeric_cmp <- (is.numeric(target) || !anyNA(num(target[!is.na(target)]))) && !anyNA(num(value))
  t <- if (numeric_cmp) num(target) else as.character(target)
  v <- if (numeric_cmp) num(value) else as.character(value)
  a <- v[1]
  b <- if (length(v) > 1) v[2] else v[1]
  keep <- switch(op,
    "=" = t == a, "!=" = t != a, "<" = t < a, ">" = t > a, "<=" = t <= a, ">=" = t >= a,
    "[]" = t >= a & t <= b, "()" = t > a & t < b, "[)" = t >= a & t < b, "(]" = t > a & t <= b,
    "][" = t <= a | t >= b, ")(" = t < a | t > b, "](" = t <= a | t > b, ")[" = t < a | t >= b,
    "{}" = t %in% v, "}{" = !t %in% v,
    stop("Unknown filter operation '", op, "'", call. = FALSE)
  )
  keep <- !is.na(keep) & keep
  if (isTRUE(tf$preservegaps)) {
    # filtered points become gaps: missing values in x/y (and z)
    for (k in intersect(c("x", "y", "z"), names(tr))) {
      if (length(tr[[k]]) == n) tr[[k]][!keep] <- NA
    }
    return(list(tr))
  }
  list(lp_subset_trace(tr, n, which(keep)))
}

lp_transform_sort <- function(tr, tf, ...) {
  target <- lp_transform_target(tr, tf$target, "x")
  n <- length(target)
  if (n < 2) return(list(tr))
  idx <- order(target, decreasing = identical(tf$order, "descending"), method = "radix")
  list(lp_subset_trace(tr, n, idx))
}

lp_transform_groupby <- function(tr, tf, n_traces = 1, ...) {
  groups <- lp_transform_target(tr, tf$groups, NULL)
  n <- length(groups)
  if (!n) return(list(tr))
  g <- as.character(groups)
  levels <- unique(g)
  fmt <- tf$nameformat %||l% if (n_traces > 1) "%{group} (%{trace})" else "%{group}"
  styles <- tf$styles %||l% list()
  lapply(levels, function(lv) {
    out <- lp_subset_trace(tr, n, which(g == lv))
    name <- gsub("%{group}", lv, fmt, fixed = TRUE)
    out$name <- gsub("%{trace}", tr$name %||l% "", name, fixed = TRUE)
    for (s in styles) {
      if (identical(as.character(unlist(s$target)), lv) && is.list(s$value)) {
        out <- lp_modify_list(out, s$value)
      }
    }
    out
  })
}

LP_AGG_FUNCS <- list(
  count = function(v, ...) length(v),
  sum = function(v, ...) sum(as.numeric(v), na.rm = TRUE),
  avg = function(v, ...) mean(as.numeric(v), na.rm = TRUE),
  median = function(v, ...) stats::median(as.numeric(v), na.rm = TRUE),
  mode = function(v, ...) {
    tab <- table(factor(v, levels = unique(v)))
    v[match(names(tab)[which.max(tab)], as.character(v))]
  },
  rms = function(v, ...) sqrt(mean(as.numeric(v)^2, na.rm = TRUE)),
  stddev = function(v, population = FALSE) {
    x <- as.numeric(v)
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    d <- sum((x - mean(x))^2)
    sqrt(d / if (population) length(x) else max(length(x) - 1, 1))
  },
  min = function(v, ...) {
    if (is.numeric(v)) min(v, na.rm = TRUE) else v[order(v)][1]
  },
  max = function(v, ...) {
    if (is.numeric(v)) max(v, na.rm = TRUE) else v[order(v, decreasing = TRUE)][1]
  },
  first = function(v, ...) v[1],
  last = function(v, ...) v[length(v)],
  change = function(v, ...) as.numeric(v[length(v)]) - as.numeric(v[1]),
  range = function(v, ...) diff(range(as.numeric(v), na.rm = TRUE))
)

lp_transform_aggregate <- function(tr, tf, ...) {
  group_attr <- if (is.character(tf$groups) && length(tf$groups) == 1) tf$groups else if (is.null(tf$groups)) "x" else NULL
  groups <- lp_transform_target(tr, tf$groups, "x")
  n <- length(groups)
  if (!n) return(list(tr))
  g <- as.character(groups)
  first_idx <- which(!duplicated(g))
  members <- split(seq_len(n), factor(g, levels = unique(g)))
  funcs <- list()
  for (a in tf$aggregations %||l% list()) {
    if (is.list(a) && !isFALSE(a$enabled) && !is.null(a$target)) {
      funcs[[a$target]] <- list(func = a$func %||l% "first", population = identical(a$funcmode, "population"))
    }
  }
  for (path in lp_point_arrays(tr, n)) {
    key <- paste(path, collapse = ".")
    v <- lp_get_path(tr, path)
    spec <- funcs[[key]]
    if (is.null(spec) || identical(key, group_attr)) {
      out <- v[first_idx]
    } else {
      f <- LP_AGG_FUNCS[[spec$func]]
      if (is.null(f)) stop("Unknown aggregation function '", spec$func, "'", call. = FALSE)
      out <- unlist(lapply(members, function(i) f(v[i], population = spec$population)), use.names = FALSE)
    }
    tr <- lp_set_path(tr, path, out)
  }
  list(tr)
}

