library(igraph)
library(dplyr)
library(ggplot)

load("pathweights_timing.rda")

library(ggplot2)

read_graph_settings <- function(g) {
  data_frame(
    v_count = vcount(g),
    e_count = ecount(g),
    s_count = g %>% as_tibble("edges") %>% pull(target) %>% sum()
  )
}

graph_settings <- map_df(graph_battery, read_graph_settings, .id = "battery_index")

timing_report <- timed_weighting %>%
  left_join(graph_settings, by = "battery_index") %>%
  filter(name == "elapsed")

ggplot(timing_report, aes(x = v_count, color = as.factor(s_count), y = value)) +
  geom_point()

timing_model <- lm(value ~ poly(v_count, 2) + poly(e_count, 2) + poly(s_count, 2), timing_report)
summary(timing_model)
