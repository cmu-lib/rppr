library(purrr)
library(igraph)
library(bench)
library(furrr)
library(tidygraph)
library(dplyr)
library(assertthat)
library(future)
library(tibble)

set.seed(10)
devtools::load_all()

node_sizes <- c(10, 30, 50, 100, 200, 500, 1000)
edge_counts <- c(5, 50, 100, 300, 500)

gridsearch <- cross_df(list(n_nodes = node_sizes, n_edges = edge_counts))

graph_battery <- pmap(gridsearch, function(n_nodes, n_edges) {
  g <- play_geometry(n = n_nodes, radius = 0.25, torus = TRUE)
  if (n_edges > ecount(g)) return(NULL)

  which_e <- sample.int(ecount(g), size = n_edges)
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
}) %>% compact()

timed_weighting <- rerun(6, {
  map_df(graph_battery, function(g) {
  enframe(system.time(full_path_weights(g)))
}, .id = "battery_index")
  }) %>%
  bind_rows(.id = "replicate")

save(timed_weighting, file = "pathweights_timing.rda")

# library(ggplot2)
#
# read_graph_settings <- function(g) {
#   data_frame(
#     v_count = vcount(g),
#     e_count = ecount(g),
#     s_count = g %>% as_tibble("edges") %>% pull(target) %>% sum()
#   )
# }
#
# graph_settings <- map_df(graph_battery, read_graph_settings, .id = "battery_index")
#
# timing_report <- timed_weighting %>%
#   left_join(graph_settings, by = "battery_index") %>%
#   filter(name == "elapsed")
#
# ggplot(timing_report, aes(x = v_count, color = as.factor(s_count), y = value)) +
#   geom_point()
#
# timing_model <- lm(value ~ poly(v_count, 2) + poly(e_count, 2) + poly(s_count, 2), timing_report)
# summary(timing_model)
