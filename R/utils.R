#' Remove all unreachable nodes
#'
#' Remove all nodes that do not have a degree of at least 1.
#'
#' @param graph A [tidygraph::tidygraph] object
#'
#' @return Tidygraph with all isolated nodes removed.
#'
#' @export
remove_unreachable_nodes <- function(graph) {
  subgraph.edges(graph, eids = E(graph), delete.vertices = TRUE)
}

# Decorate a graph with edge, node, and target ids, and add class of rpp_graph as sign to later functions
decorate_graph <- function(graph, edgeset) {
  decorated_graph <- add_oids(graph)
  target_lgl <- seq_len(ecount(decorated_graph)) %in% edgeset
  decorated_graph <- add_target_status(decorated_graph, target_lgl)
  structure(decorated_graph, class = c(class(decorated_graph), "rpp_graph"))
}

is_graph_decorated <- function(graph) {
  inherits(graph, "rpp_graph") &
    ".oid" %in% vertex_attr_names(graph) &
    ".oid" %in% edge_attr_names(graph) &
    ".target" %in% edge_attr_names(graph)
}

# Add original node and edge ids to a graph that can be traced during transformations
add_oids <- function(graph) {
  add_edge_oids(add_node_oids(graph))
}

add_node_oids <- function(graph) {
  vertex_attr(graph, ".oid") <- seq_len(vcount(graph))
  graph
}

add_edge_oids <- function(graph) {
  edge_attr(graph, ".oid") <- seq_len(ecount(graph))
  graph
}

# Add target attribute to edges
add_target_status <- function(graph, targets) {
  assertthat::assert_that(is.logical(targets))
  assertthat::assert_that(ecount(graph) == length(targets), msg = "targets must be the same length as the number of edges in the graph")

  edge_attr(graph, ".target") <- targets
  graph
}

# Get original graph node and target indices
get_index <- function(graph, element_type, id, f) {
  assertthat::assert_that(inherits(graph, "igraph"))
  assertthat::assert_that(assertthat::is.count(id))

  i <- which(id == f(graph, ".oid"))

  if (length(i) == 0) return(NA_integer_)
  assertthat::assert_that(length(i) == 1, msg = paste0("More than one ", element_type, " with the .oid ", id, " were returned; only 1 expected."))

  i
}

get_node_index <- function(graph, node_oid) {
  get_index(graph, "node", node_oid, vertex_attr)
}

get_edge_index <- function(graph, edge_oid) {
  get_index(graph, "edge", edge_oid, edge_attr)
}
