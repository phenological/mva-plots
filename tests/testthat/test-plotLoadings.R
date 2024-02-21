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

test_that("can create flat loadings plot for O-PLS(DA)", {
  exampleData <- mtcars

  colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

  exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
  exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
  rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

  model <- oplsda(Y = (exampleData[, "status"]),
                  X = exampleData[,1:5],
                  type = "OPLS")

  test<- plotLoadings(model = model,
                      flat =TRUE)
  t <- ggplot_build(test@suppLs[["LoadingsPlot"]])
  first_entry <- t[["data"]][[1]][["y"]][1]

  # Check if all entries are the same
  all_same <- all(sapply(t[["data"]][[1]][["y"]], identical, first_entry))

  # Assert that all entries are the same
  expect_true(all_same)
})


#want flat plotloadings and plotscores for OPLS, since you don't want the orthogonal component graphed.
#O-PLS-DA currently only handles 2 groups in Y so would allow positions of each group at 1 and 2 on y axis (for example) for the plotscores. doesn't matter for the loadings.
#O-PLS driven by a continuous Y, perhaps don't need to position them separately via "group" still have cont colour just appear along the same line.


