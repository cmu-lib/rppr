#' Find solution to the rural postman problem
#'
#' @param graph A [tidygraph::tidygraph] object
#' @param edgeset A vector of edge ids that match to the edge attribute `name` in `graph`
#'
#' @return An ordered vector of edge IDs
#'
#' @import tidygraph
rpp <- function(graph, edgeset) {

}

remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}

set.seed(10)

library(tidygraph)
library(ggraph)
library(igraph)
library(tidyr)
library(purrr)
library(digest)
library(dplyr)

g <- play_erdos_renyi(n = 30, 0.07)
e <- seq_len(ecount(g)) %in% sample.int(ecount(g), size = 10)

g <- g %>%
  activate(nodes) %>%
  # Create a persistent ID so that we can more easily track nodes through
  # resampling
  mutate(pid = row_number()) %>%
  activate(edges) %>%
  mutate(
    from_id = from,
    to_id = to,
    original = TRUE,
    weight = sample.int(10, size = ecount(g), replace = TRUE),
    target = e)

showme <- function(g) {
  ggraph(g, layout = "fr") +
    geom_edge_arc(aes(color = target, alpha = original),
                  arrow = arrow(length = unit(4, 'mm')),
                  start_cap = circle(3, 'mm'),
                  end_cap = circle(3, 'mm'),
                  curvature = 0.4) +
    geom_node_point() +
    scale_edge_colour_manual(values = c("TRUE" = "red", "FALSE" = "gray"), na.value = "gray", guide = FALSE) +
    scale_edge_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2, guide = FALSE)) +
    theme_graph()
}

showme(g)
# Create subnetwork of required edges and their nodes

subnetwork <- g %>%
  activate(edges) %>%
  filter(target == TRUE) %>%
  remove_unreachable_nodes()
subnetwork
showme(subnetwork)

# Complete this network by adding artificial edges. New artificial edges should
# have weight defined as shortest path between each of its nodes from the
# original network

check_path_weight <- function(source, destination, graph) {
  message(source, "---", destination)
  graph %>%
    activate(nodes) %>%
    mutate(distance = node_distance_to(nodes = destination, weights = weight)) %>%
    as_tibble() %>%
    slice(source) %>%
    pull(distance)
}

complete_sub_graph <- function(graph) {
  plan(multiprocess)

  g_edges <- as_tibble(graph, active = "edges")
  node_crosswalk <- bind_rows(
    select(g_edges, new = from, old = from_id),
    select(g_edges, new = to, old = to_id)
  ) %>% distinct()

  completed <- g_edges %>%
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
    anti_join(g_edges, by = c("from_id", "to_id")) %>%
    mutate(
      original = FALSE,
      # Calculate the weight of each of these new edges based on the path
      # distance between both nodes in the original network. N.B. the node IDs
      # must be the ones from the original graph, not the subnetwork, ergo using
      # from_id and to_id
      weight = future_map2_dbl(from_id, to_id, check_path_weight, graph = graph, .progress = TRUE),
      target = FALSE)

  bind_edges(graph, completed)
}

cg <- complete_sub_graph(g)

showme(cg)

mst <- to_minimum_spanning_tree(cg, weights = weight)
showme(mst_g)

mst_g <- g %>%
  morph(to_minimum_spanning_tree) %>%
  mutate(mst = TRUE) %>%
  unmorph()
mst_g

showme(mst_g)
