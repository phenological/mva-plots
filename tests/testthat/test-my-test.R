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




#ellipseOptions





#foldChange

#cliffsDelta




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
