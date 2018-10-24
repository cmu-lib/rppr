context("utils")

test_that("remove unreachable nodes", {
  expect_true(any(degree(island_g) == 0))
  expect_true(!(any(degree(remove_unreachable_nodes(island_g)) == 0)))
})

test_that("Add node and edge ids", {
  numbered_graph <- add_oids(g)
  expect_equivalent(get_node_index(numbered_graph, 5), 5)
  expect_equivalent(get_edge_index(numbered_graph, 5), 5)
})

test_that("Get node ids", {
  pruned_nodes <- numbered_graph %>%
    activate(nodes) %>%
    slice(5:n_count)

  expect_equivalent(get_node_index(pruned_nodes, 7), 3)
  expect_true(is.na(get_node_index(pruned_nodes, 1)))

  malformed_graph <- numbered_graph
  vertex_attr(malformed_graph, ".oid") <- rep(5, n_count)
  expect_error(get_node_index(malformed_graph, 5))
})

test_that("Get edge ids", {

  pruned_edges <- numbered_graph %>%
    activate(edges) %>%
    slice(5:n_count)

  expect_equivalent(get_edge_index(pruned_edges, 7), 3)
  expect_true(is.na(get_edge_index(pruned_edges, 1)))

  malformed_graph <- numbered_graph
  edge_attr(malformed_graph, ".oid") <- rep(5, e_count)
  expect_error(get_edge_index(malformed_graph, 5))
})
