test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("PCA",{
  expect_known
})


#PCA


#plotScores
test_that("pcaGrid is a gg object",{
  data("iris")
  a <- PCA(data=iris[,1:4], annotation =iris[,5])
  p <- plotScores(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["pcaGrid"]], class = "gg")
})

test_that("plots is a list",{
  data("iris")
  a <- PCA(data=iris[,1:4], annotation =iris[,5])
  p <- plotScores(model = a, optns = list())
  expect_type(p$plots,"list")
})

#plotLoadings
test_that("plotLoadingGrid is a gg object",{
  data("iris")
  a <- PCA(data=iris[,1:4], annotation =iris[,5])
  p <- plotLoadings(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["plotLoadingGrid"]], class = "gg")
})

#eruptionPlot
test_that("plotLoadingGrid is a gg object",{
  data("mtcars")
  a <- PCA(data=mtcars[,1:7], annotation =mtcars[,8:11])
  p <- eruptionPlot(model = a, factor=mtcars[,"vs"], optns = list())
  expect_s3_class(object= p[["plots"]][["eruptionPlot"]], class = "gg")
})










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
