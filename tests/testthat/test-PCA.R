#PCA
test_that("PCA function works as expected", {
  # Generate some sample data for testing
  set.seed(123)
  data <- matrix(rnorm(100), ncol = 5)

  # Test the PCA function with default options
  result <- PCA(data)
  expect_type(result$data, "list")
  expect_type(result$plots, "list")
  expect_equal(length(result$data), 10)  # Check if all expected elements are present in the output list
  expect_equal(length(result$plots), 3)  # Check if all expected elements are present in the output list

  # Test the PCA function with specific options
  result <- PCA(data, center = FALSE, scale. = FALSE, rank = 3, plot = FALSE)
  expect_type(result$data, "list")
  expect_type(result$plots, "list")
  expect_equal(length(result$plots), 0)
  expect_equal(length(result$data), 10)
  expect_equal(length(result[["data"]][["pcdf"]]), 3)
  expect_false(result[["data"]][["center"]],
               info = "Expect 'center' to be FALSE")
  expect_false(result[["data"]][["scale"]],
               info = "Expect 'center' to be FALSE")


  # Add more test cases as needed
})
