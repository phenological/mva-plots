
test_that("PCA produced model is handled appropriately", {
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])
  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)
  data$age = seq(from = 18, to = 60, length.out = 20)
  data$sex <- sample(c("female", "male"), 20, replace = TRUE)

  #list object
  a <- PCA(data=data[,1:5])
  p <- eruptionPlot(model = a, optns = list(factor=data[,"sex"], method = "none", color = "pval"))

  #correct class
  expect_s3_class(object= p[["plots"]][["eruptionPlot"]], class = "gg")
  expect_equal(length(p), 2)
  expect_equal(length(p[["plots"]]), 4)
})


test_that("oplsda produced model is handled appropriately", {
  #opls object
  a <- oplsda(X=mtcars[,1:5], Y = mtcars$vs, type = "OPLS")

  #eruption
  p <- eruptionPlot(model = a, optns = list(factor=mtcars[,"vs"]))

  #is eruptionData appended
  expect_true("eruptionData" %in% names(p@suppLs),
              "'eruptionData' should be present in p@suppLs")

  #are all elements of eruptionData present
  elements_to_check <- c("cd", "fc", "pval", "pvalRaw", "loadings", "id", "corr")
  expect_true(all(elements_to_check %in% names(p@suppLs[["eruptionData"]])))

  #is eruptionPlot appended
  expect_true("eruptionPlot" %in% names(p@suppLs),
              "'eruptionPlot' should be present in p@suppLs")

  #is eruptionPlot a gg object
  tryCatch(
    expect_s3_class(p@suppLs[["eruptionPlot"]], "gg"),
    error = function(e) {
      stop("'eruptionPlot' should be a gg object")
    }
  )
})


