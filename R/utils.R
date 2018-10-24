#' Remove all unreachable nodes
#'
#' Remove all nodes that do not have a degree of at least 1.
#'
#' @param graph A [tidygraph::tidygraph] object
#'
#' @return Tidygraph with all isolated nodes removed.
#'
#' @import tidygraph
#' @export
remove_unreachable_nodes <- function(graph) {
  graph %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}
