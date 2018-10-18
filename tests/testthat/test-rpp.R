context("rpp")

library(tidygraph)
library(igraph)
set.seed(10)

n_nodes <- 20
g <- play_geometry(n = n_nodes, radius = 0.25, torus = TRUE)
which_e <- sample.int(ecount(g), size = 10)
e <- seq_len(ecount(g)) %in% which_e

g <- g %>%
  activate(nodes) %>%
  # Create a persistent ID so that we can more easily track nodes through
  # resampling
  mutate(pid = dplyr::row_number()) %>%
  activate(edges) %>%
  mutate(
    from_id = from,
    to_id = to,
    original = TRUE,
    weight = sample.int(10, size = ecount(g), replace = TRUE),
    target = e) %>%
  activate(nodes) %>%
  mutate(adjacent_to_selected = map_local_lgl(.f = function(neighborhood, ...) {
    neighborhood %>%
      as_tibble(active = "edges") %>%
      pull(target) %>%
      any()
  })) %>%
  remove_unreachable_nodes()

path_weight_lookup_table <- full_path_weights(g)

cg <- complete_sub_graph(subnetwork, original_graph = g)

showme(cg)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
