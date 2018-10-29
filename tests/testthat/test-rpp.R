context("rpp")

test_that("subnetwork of targeted edges", {
  subset_network <- create_subnetwork(rppr_graph)
  expect_equivalent(ecount(subset_network), s_count)

  # Expect all nodes have at least one connection
  expect_false(any(degree(subset_network, mode = "all") == 0))
})

test_that("target adjacent node ids", {
  tids <- target_adjacent_node_oids(rppr_graph)
  expect_true(all(map_lgl(
    # edge sets to test
    incident_edges(rppr_graph, tids, "total"),
    # Every edge set should include at least one target edge
    function(x) any(edge_attr(rppr_graph, name = ".target", index = x)))))
})

test_that("node-node distance", {
  dist_source <- 1
  dist_targets <- c(5, 6, 7)
  test_distance <- node_to_nodes_distance(dist_source, dist_targets, rppr_graph)
  expect_equivalent(test_distance[["from"]], dist_targets)
  expect_is(test_distance[["distance"]], "numeric")
})
