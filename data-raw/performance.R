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

node_sizes <- c(30, 50, 100, 200, 500, 1000, 2000, 3000, 5000, 7000, 10000, 12000)
edge_counts <- c(5, 50, 100, 300, 500, 700, 1000, 3000, 5000)

gridsearch <- cross_df(list(n_nodes = node_sizes, n_edges = edge_counts))

any_incident_target <- function(x, graph) {
  es <- incident(graph, x)
  any(edge_attr(graph, name = "target", index = as.integer(es)))
}

plan(multiprocess)
graph_battery <- future_pmap(gridsearch, function(n_nodes, n_edges) {
  message(n_nodes, "\t", n_edges)
  g <- play_geometry(n = n_nodes, radius = 2/sqrt(n_nodes))
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
    mutate(adjacent_to_selected = map_lgl(pid, any_incident_target, graph = .)) %>%
    remove_unreachable_nodes()

  sampled_edges <- as_tibble(g, "edges") %>%
    sample_frac(size = 0.1)

  mst_g <- g %>%
    convert(to_minimum_spanning_tree)

  augmented_mst_g <- mst_g %>%
    bind_edges(sampled_edges)

  augmented_mst_g

}) %>% compact()

c <- 0
timed_weighting <- rerun(20, {
  c <<- c + 1
  suppressWarnings({
    map_df(graph_battery, function(g) {
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
