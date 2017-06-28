context("annotation_namespaces")

test_that("empty namespaces work correctly", {
  expect_equal(get_namespaces(), list())
})
