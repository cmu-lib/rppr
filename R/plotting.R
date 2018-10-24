#' @import ggraph
showme <- function(g) {
  ggraph(g, layout = "nicely") +
    geom_edge_link(aes(color = target, alpha = original, width = weight),
                   start_cap = circle(3, 'mm'),
                   end_cap = circle(3, 'mm')) +
    geom_node_label(aes(label = pid)) +
    scale_edge_colour_manual(values = c("TRUE" = "red", "FALSE" = "gray"), na.value = "gray", guide = FALSE) +
    scale_edge_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2), guide = FALSE) +
    theme_graph()
}
