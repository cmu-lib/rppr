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

node_sizes <- c(10, 30, 50, 100, 200, 500, 1000, 3000)
edge_counts <- c(5, 50, 100, 300, 500, 700)

gridsearch <- cross_df(list(n_nodes = node_sizes, n_edges = edge_counts))

graph_battery <- pmap(gridsearch, function(n_nodes, n_edges) {
  message(n_nodes, "\t", n_edges)
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

c <- 0
timed_weighting <- rerun(6, {
  c <<- c + 1
  suppressWarnings({
    map_df(graph_battery[1], function(g) {
      v_count <- vcount(g)
      e_count <- ecount(g)
      s_count <- g %>% as_tibble("edges") %>% pull(target) %>% sum()

      message("Run ", c, "\t", "V: ", v_count, "\tE: ", s_count, "\t...", appendLF = FALSE)
      res <- enframe(system.time(full_path_weights(g))) %>%
        mutate(v_count = v_count, e_count = e_count, s_count = s_count)

      message("done")

      res

    }, .id = "battery_index")
  })
}) %>%
  bind_rows(.id = "replicate")

save(timed_weighting, file = "pathweights_timing.rda")
