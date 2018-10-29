#' Find solution to the rural postman problem
#'
#' @param graph A [tidygraph::tidygraph] object
#' @param edgeset A vector of edge ids that match to the edge attribute `name` in `graph`
#'
#' @return An ordered vector of edge IDs
rpp <- function(graph, edgeset) {

}

# Create subnetwork of required edges and their nodes
#' @import igraph
create_subnetwork <- function(graph) {
  subgraph.edges(graph,
                eids = E(graph)[which(edge_attr(graph, ".target") == TRUE)],
                delete.vertices = TRUE)
}

#' @import igraph
target_adjacent_node_oids <- function(graph) {
  purrr::set_names(vertex_attr(create_subnetwork(graph), ".oid"))
}

# Complete this network by adding artificial edges. New artificial edges should
# have weight defined as shortest path between each of its nodes from the
# original network
#' @import igraph furrr future
full_path_weights <- function(graph) {
  selected_nodes <- target_adjacent_node_oids(graph)

  wo_targets <- subgraph.edges(graph,
                             eids = E(graph)[which(!(edge_attr(graph, ".target")))],
                             delete.vertices = FALSE)

  plan(multiprocess)
  res <- future_map_dfr(selected_nodes, node_to_nodes_distance, targets = selected_nodes,
                        graph = wo_targets, .id = "to")
  res[["to"]] <- as.integer(res[["to"]])
  res
}

node_to_nodes_distance <- function(x, targets, graph, ...) {
  original_ids <- as_tibble(graph, "nodes")$pid
  search_index <- match(x, original_ids)
  target_indices <- match(targets, original_ids)

  assert_that(noNA(search_index))
  assert_that(noNA(target_indices))

  to_distance <- t(distances(graph, v = search_index, to = target_indices))[,1]

  stopifnot(length(to_distance) == length(targets))

  data_frame(from = targets, distance = to_distance)
}

#' @import assertthat dplyr
path_weight_lookup <- function(from_id, to_id, weight_table) {
  assert_that(is.count(from_id))
  assert_that(is.count(to_id))

  res <- weight_table %>%
    filter(from == from_id, to == to_id) %>%
    pull(distance)

  assert_that(is.number(res), msg = paste0("Results: ", str(res)))

  res
}

#' @import tidygraph purrr dplyr
complete_sub_graph <- function(graph, original_graph) {

  # Get the subgraph edges as a data frame
  g_edges <- as_tibble(graph, active = "edges")

  # Map original node ids to the new ids assigned within the subgraph
  node_crosswalk <- bind_rows(
    select(g_edges, new = from, old = from_id),
    select(g_edges, new = to, old = to_id)
  ) %>% distinct()

  # Set up a table of new edges
  edge_placeholders <- g_edges %>%
    complete(from, to) %>%
    # Join original IDs
    left_join(node_crosswalk, by = c("from" = "new")) %>%
    mutate(from_id = coalesce(from_id, old)) %>%
    select(-old) %>%
    # Join original IDs
    left_join(node_crosswalk, by = c("to" = "new")) %>%
    mutate(to_id = coalesce(to_id, old)) %>%
    select(-old) %>%
    # No loops
    filter(from_id != to_id) %>%
    # Don't duplicate the original subgraph edges
    anti_join(g_edges, by = c("from_id", "to_id"))

  # Fill out attributes for these new edges
  completed <- edge_placeholders %>%
    mutate(
      # Calculate the weight of each of these new edges based on the path
      # distance between both nodes in the original network. N.B. the node IDs
      # must be the ones from the original graph, not the subnetwork, ergo using
      # from_id and to_id
      weight = map2_dbl(from_id, to_id, path_weight_lookup, weight_table = path_weight_lookup_table),
      original = FALSE,
      target = FALSE) %>%
    # Remove all infinite edges
    filter(!is.infinite(weight))

  bind_edges(graph, completed)
}

# Simplify this "completed" network by eliminating all artificial edges for which
# 1) the cost is over some threshold k (?)
# 2) One parallel edge if it has the same cost

#' @import tidygraph dplyr
prune_complete_network <- function(graph, k = Inf) {
  graph %>%
    activate(edges) %>%
    # Always keep original edges
    filter(original == TRUE | weight < k)
}
