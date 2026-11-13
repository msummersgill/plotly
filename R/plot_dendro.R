

#' Plot an interactive dendrogram
#' 
#' This function takes advantage of nested key selections to implement an 
#' interactive dendrogram. Selecting a node selects all the labels (i.e. leafs)
#' under that node.
#' 
#' @param d a dendrogram object
#' @param set defines a crosstalk group
#' @param xmin minimum of the range of the x-scale
#' @param width width
#' @param height height
#' @param ... arguments supplied to [subplot()]
#' @export
#' @author Carson Sievert
#' @seealso [plot_ly()], [plot_mapbox()], [ggplotly()] 
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' \dontrun{
#' hc <- hclust(dist(USArrests), "ave")
#' dend1 <- as.dendrogram(hc)
#' plot_dendro(dend1, height = 600) %>% 
#'   hide_legend() %>% 
#'   highlight(persistent = TRUE, dynamic = TRUE)
#' }
#' 

plot_dendro <- function(d, set = "A", xmin = -50, height = 500, width = 500, ...) {
  # get x/y locations of every node in the tree
  allXY <- get_xy(d)
  # get non-zero heights so we can split on them and find the relevant labels
  non0 <- allXY[["y"]][allXY[["y"]] > 0]
  # splitting on the minimum height would generate all terminal nodes anyway
  split <- non0[min(non0) < non0]
  # label is a list-column since non-zero heights have multiple labels
  # for now, we just have access to terminal node labels
  labs <- labels(d)
  allXY$label <- vector("list", nrow(allXY))
  allXY$label[[1]] <- labs
  allXY$label[allXY$y == 0] <- labs
  
  # collect all the *unique* non-trivial nodes
  nodes <- list()
  for (i in split) {
    dsub <- cut(d, i)$lower
    for (j in seq_along(dsub)) {
      s <- dsub[[j]]
      if (is.leaf(s)) next
      if (any(vapply(nodes, function(x) identical(x, s), logical(1)))) next
      nodes[[length(nodes) + 1]] <- s
    }
  }
  
  heights <- sapply(nodes, function(x) attr(x, "height"))
  labs <- lapply(nodes, labels)
  
  # NOTE: this won't support nodes that have the same height 
  # but that isn't possible, right?
  for (i in seq_along(heights)) {
    allXY$label[[which(allXY$y == heights[i])]] <- labs[[i]]
  }
  
  tidy_segments <- dendextend::as.ggdend(d)$segments
  
  allTXT <- allXY[allXY$y == 0, ]
  
  blank_axis <- list(
    title = "",
    showticklabels = FALSE,
    zeroline = FALSE
  )
  
  allXY$members <- sapply(allXY$label, length)
  allTXT$label <- as.character(allTXT$label)
  
  allXY %>% 
    plot_ly(x = ~y, y = ~x, color = I("black"), hoverinfo = "none",
            height = height, width = width) %>%
    add_segments(
      data = tidy_segments, xend = ~yend, yend = ~xend, showlegend = FALSE
    ) %>%
    add_markers(
      data = allXY[allXY$y > 0, ], key = ~label, set = set, name = "nodes", 
      text = ~paste0("members: ", members), hoverinfo = "text"
    ) %>%
    add_text(
      data = allTXT, x = 0, y = ~x, text = ~label, key = ~label, set = set,
      textposition = "middle left", name = "labels"
    ) %>%
    layout(
      dragmode = "select", 
      xaxis = c(blank_axis, list(range = c(xmin, extendrange(allXY[["y"]])[2]))),
      yaxis = c(blank_axis, list(range = extendrange(allXY[["x"]])))
    )
}

get_xy <- function(node) {
  m <- dendextend::get_nodes_xy(node)
  colnames(m) <- c("x", "y")
  tibble::as_tibble(m)
}
