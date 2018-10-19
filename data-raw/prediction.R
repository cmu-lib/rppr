library(igraph)
library(dplyr)
library(ggplot2)
library(lubridate)

load("pathweights_timing.rda")

timing_report <- timed_weighting %>%
  filter(name == "elapsed") %>%
  mutate(grouping = group_indices(., replicate, s_count))

ggplot(timing_report, aes(x = v_count, y = value)) +
  geom_point() +
  labs(y = "seconds")

timing_model <- lm(value ~ v_count * e_count * s_count, timing_report)

summary(timing_model)

pgh <- data_frame(
  v_count = 1853767,
  e_count = 245821,
  s_count = 2667
)

pgh_prediction <- predict(timing_model, newdata = pgh, se.fit = TRUE)

seconds_to_period(pgh_prediction$fit - pgh_prediction$se.fit/2)
seconds_to_period(pgh_prediction$fit + pgh_prediction$se.fit/2)
