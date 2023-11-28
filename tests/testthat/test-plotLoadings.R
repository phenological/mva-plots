#plotLoadings
test_that("plotLoadingGrid is a gg object",{
  data("iris")
  a <- PCA(data = iris[,1:4])
  p <- plotLoadings(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["plotLoadingGrid"]], class = "gg")
})


test_that("opls object is handled correctly", {
  ##opls object
  exampleData <- mtcars

  colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

  exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
  exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
  rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

  model <- oplsda(Y = (exampleData[, "status"]),
                  X = exampleData[,1:5],
                  type = "OPLS")

  #make plotloadings for oplsda model with ellipse and outliers included

  pl<- plotLoadings(model = model)

  #is the plot appended to the model
  expect_true("LoadingsPlot" %in% names(pl@suppLs), "LoadingsPlot should exist in pl@suppLs")


})
