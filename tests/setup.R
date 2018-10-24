# Generate necessary graph data

library(tidygraph)
library(igraph)
set.seed(10)

n_count <- 30
s_count <- 5

g <- play_geometry(n = n_count, radius = 0.2)

e_count <- ecount(g)

which_e <- sample.int(e_count, size = 5, replace = FALSE)
e <- seq_len(e_count) %in% which_e

island_g <- g %>%
  bind_nodes(data.frame(x = 0, y = 0))
