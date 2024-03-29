#gg_circle

test_that("gg_circle produces the expected plot", {

  #generate data
  set.seed(123)
  data <- matrix(rnorm(100), ncol = 5)
  #Do PCA
  result <- PCA(data)
  n <- nrow(result[["data"]][["rawData"]])
  ci <- 0.95
  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

  #make ggplot
  p<- ggplot(data=result$data$pcdf, aes(x=result$data$pcdf$PC4, y=result$data$pcdf$PC5)) +
    geom_point() +
    gg_circle( rx = sqrt(var(result$data$pcdf$PC4) * hotFisN),
               ry = sqrt(var(result$data$pcdf$PC5) * hotFisN),
               xc = 0,
               yc = 0)

  expect_equal(length(p[["layers"]]), 2)  # Has an extra layer been added

  #correct data in the second layer of the plot to make the circle
  expect_equal(length(p[["layers"]][[2]][["data"]]), 3)
  expect_equal(names(p[["layers"]][[2]][["data"]]), c("x","ymin","ymax"))
  expect_equal(nrow(p[["layers"]][[2]][["data"]]), 100)
  expect_equal(length(p[["labels"]]), 4)

})
