#' Remove all unreachable nodes
#'
#' Remove all nodes that do not have a degree of at least 1.
#'
#' @param graph A [tidygraph::tidygraph] object
#'
#' @return Tidygraph with all isolated nodes removed.
#'
#' @import tidygraph
#' @export
remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
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
  assert_that(is.logical(targets))
  assert_that(ecount(graph) == length(targets), msg = "targets must be the same length as the number of edges in the graph")

  edge_attr(graph, ".target") <- targets
  graph
}

# Get original graph node and target indices
get_index <- function(graph, element_type, id, f) {
  stopifnot(inherits(graph, "igraph"))
  stopifnot(is.count(id))

  i <- which(id == f(graph, ".oid"))

  if (length(i) == 0) return(NA_integer_)
  if (length(i) > 1) stop("More than one ", element_type, " with the .oid ", id, " were returned; only 1 expected.")

  i
}

get_node_index <- function(graph, node_oid) {
  get_index(graph, "node", node_oid, vertex_attr)
}

get_edge_index <- function(graph, edge_oid) {
  get_index(graph, "edge", edge_oid, edge_attr)
}
