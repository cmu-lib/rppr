context("rpp")

test_that("subnetwork of targeted edges", {
  subset_network <- create_subnetwork(rppr_graph)
  expect_equivalent(ecount(subset_network), s_count)

  # Expect all nodes have at least one connection
  expect_false(any(degree(subset_network, mode = "all") == 0))
})
