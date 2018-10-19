library(igraph)
library(dplyr)
library(ggplot)

load("pathweights_timing.rda")

library(ggplot2)

timing_report <- timed_weighting %>%
  filter(name == "elapsed")

ggplot(timing_report, aes(x = v_count, color = as.factor(s_count), y = value)) +
  geom_point()

timing_model <- lm(value ~ poly(v_count, 2) + poly(e_count, 2) + poly(s_count, 2), timing_report)
summary(timing_model)
