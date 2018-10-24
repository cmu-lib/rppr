context("utils")

test_that("remove unreachable nodes", {
  expect_equivalent(vcount(remove_unreachable_nodes(island_g)), vcount(g))
})
