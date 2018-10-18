#' Find solution to the rural postman problem
#'
#' @param graph A [tidygraph::tidygraph] object
#' @param edgeset A vector of edge ids that match to the edge attribute `name` in `graph`
#'
#' @return An ordered vector of edge IDs
rpp <- function(graph, edgeset) {

}

#' @import tidygraph
remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}

#' @import ggraph
showme <- function(g) {
  ggraph(g, layout = "nicely") +
    geom_edge_link(aes(color = target, alpha = original, width = weight),
                  start_cap = circle(3, 'mm'),
                  end_cap = circle(3, 'mm')) +
    geom_node_label(aes(label = pid)) +
    scale_edge_colour_manual(values = c("TRUE" = "red", "FALSE" = "gray"), na.value = "gray", guide = FALSE) +
    scale_edge_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2), guide = FALSE) +
    theme_graph()
}

# Create subnetwork of required edges and their nodes
#' @import tidygraph
create_subnetwork <- function(graph) {
  graph %>%
    activate(edges) %>%
    filter(target == TRUE) %>%
    remove_unreachable_nodes()
}

# Complete this network by adding artificial edges. New artificial edges should
# have weight defined as shortest path between each of its nodes from the
# original network

# TODO before calculating weight, REMOVE bridges so they can't be crossed
# (this may lead to unreachable places, maybe?)
#' @import tidygraph dplyr furrr future
full_path_weights <- function(graph) {
  plan(multiprocess)
  selected_nodes <- graph %>%
    as_tibble(active = "nodes") %>%
    filter(adjacent_to_selected) %>%
    pull(pid) %>%
    set_names()

  res <- future_map_dfr(selected_nodes, function(x) {
    search_index <- match(x, as_tibble(graph, "nodes")$pid)
    graph %>%
      activate(edges) %>%
      filter(target == FALSE) %>%
      activate(nodes) %>%
      mutate(distance = node_distance_to(search_index, weights = NULL)) %>%
      as_tibble() %>%
      select(from = pid, distance)
  }, .id = "to", .progress = TRUE) %>%
    mutate_at(vars(to), as.integer)
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
