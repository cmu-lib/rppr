context("utils")

test_that("remove unreachable nodes", {
  expect_true(any(degree(island_g) == 0))
  expect_true(!(any(degree(remove_unreachable_nodes(island_g)) == 0)))
})
