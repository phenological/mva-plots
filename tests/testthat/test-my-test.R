test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("pcresults",{
  expect_known
})


test_that("Plot returns ggplot object",{
  data("iris")
  a <- pcResults(data=iris[,1:4], annotation =iris[,5])
  p <- plotScores(model = a, optns = list())
  expect_type(p$plots$pcaGrid,"list")
})

test_that("Plot returns ggplot object",{
  data("iris")
  a <- pcResults(data=iris[,1:4], annotation =iris[,5])
  p <- plotScores(model = a, optns = list())
  expect_type(p$plots,"list")
})

test_that("Plot returns ggplot object",{
  p <- plot_fun(df)
  expect_is(p,"ggplot")
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
