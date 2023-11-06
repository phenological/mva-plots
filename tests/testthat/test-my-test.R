test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("PCA",{
  expect_known
})

#Packages are installed and loaded
test_that("Check if required packages are installed and loaded", {
  # Define the list of required packages
  required_packages <- c("GGally",
                         "ggplot2",
                         "ggrepel",
                         "plotly",
                         "PRROC",
                         "roxygen2",
                         "stats",
                         "testthat")

  # Check each package
  for (pkg in required_packages) {
    is_installed_and_loaded <- requireNamespace(pkg, quietly = TRUE)
    expect_true(is_installed_and_loaded,
                info = if (is_installed_and_loaded) {
                  paste0(pkg, " package is installed and loaded.")
                } else {
                  paste0(pkg, " package is NOT installed and loaded.")
                })
  }
})


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
#gg_circle

test_that("gg_circle produces the expected plot", {
  set.seed(123)
  data <- matrix(rnorm(100), ncol = 5)
  result <- PCA(data)

  # Define test parameters
  rx <- 1
  ry <- 1
  xc <- 0
  yc <- 0
  color <- "blue"  # Change color for the test
  fill <- "red"   # Change fill for the test
  linetype <- "solid"  # Change linetype for the test

  # Create the ggplot object using the gg_circle function
  p <- gg_circle(rx, ry, xc, yc, color, fill, linetype)

  # Expected plot
  expected_plot <- ggplot() +
    geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), fill = "red", color = "blue", linetype = "solid") +
    theme_minimal()

  # Compare the produced plot with the expected plot
  expect_equal(ggplot_build(p)$plot, ggplot_build(expected_plot)$plot)
})

#ellipseOptions

#plotScores
test_that("pcaGrid is a gg object",{
  data("iris")
  a <- PCA(data=iris[,1:4])
  p <- plotScores(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["pcaGrid"]], class = "gg")
})

test_that("plots is a list",{
  data("iris")
  a <- PCA(data=iris[,1:4])
  p <- plotScores(model = a, optns = list())
  expect_type(p$plots,"list")
})

#plotLoadings
test_that("plotLoadingGrid is a gg object",{
  data("iris")
  a <- PCA(data=iris[,1:4])
  p <- plotLoadings(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["plotLoadingGrid"]], class = "gg")
})

#foldChange

#cliffsDelta


#eruptionPlot
test_that("plotLoadingGrid is a gg object",{
  data("mtcars")
  a <- PCA(data=mtcars[,1:7])
  p <- eruptionPlot(model = a, optns = list(factor=mtcars[,"vs"],))
  expect_s3_class(object= p[["plots"]][["eruptionPlot"]], class = "gg")
})

#oplsda

#oplsdaPredict

#permutationPlot










test_that("Plot uses correct data", {
  p <- plot_fun(df)
  expect_that(df, equals(p$data))

})

##  Load the proto library for accessing sub-components of the ggplot2
##    plot objects:
library(proto)

test_that("Plot layers match expectations",{
  p <- plot_fun(df)
  expect_is(p$layers[[1]], "proto")
  expect_identical(p$layers[[1]]$geom$objname, "bar")
  expect_identical(p$layers[[1]]$stat$objname, "identity")
})

test_that("Scale is labelled 'Proportion'",{
  p <- plot_fun(df)
  expect_identical(p$labels$y, "Proportion")
})

test_that("Scale range is NULL",{
  p <- plot_fun(df)
  expect_null(p$scales$scales[[1]]$range$range)
})
