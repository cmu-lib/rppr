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
  dist_source <- 1L
  dist_targets <- c(5L, 6L, 7L)
  test_distance <- node_to_nodes_distance(dist_source, dist_targets, rppr_graph)
  expect_equivalent(test_distance[["from"]], dist_targets)
  expect_is(test_distance[["distance"]], "numeric")
})

test_that("full path weights", {
  fpw <- full_path_weights(rppr_graph)
  incident_nodes <- target_adjacent_node_oids(rppr_graph)

  expect_is(fpw, "data.frame")
  expect_equivalent(sort(unique(fpw[["to"]])), incident_nodes)
  expect_equivalent(sort(unique(fpw[["from"]])), incident_nodes)
  expect_is(fpw[["distance"]], "numeric")
})
