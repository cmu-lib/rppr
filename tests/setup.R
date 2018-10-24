# Generate necessary graph data

library(tidygraph)
library(igraph)
set.seed(10)
g <- play_geometry(n = 30, radius = 0.2)

island_g <- g %>%
  bind_nodes(data.frame(x = 0, y = 0))
