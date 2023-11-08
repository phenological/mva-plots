# Define a test for the symmetric_limits function
test_that("symmetric_limits calculates symmetric limits", {
  # Create a vector for testing
  x <- c(-3, 1, -5, 4, 2, -6)

  # Calculate the expected symmetric limits
  expected_limits <- c(-6, 6)

  # Call the symmetric_limits function
  calculated_limits <- symmetric_limits(x)

  # Check if the calculated symmetric limits are as expected
  expect_equal(calculated_limits, expected_limits)
})
