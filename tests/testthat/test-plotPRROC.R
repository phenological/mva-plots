exampleData <- mtcars

colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

exampleData <- exampleData [,1:9]

#create a second dataset
## Select the first 5 columns
columns_to_jitter <- c("metab1", "metab2", "metab3", "metab4", "metab5")
exampleData2 <- exampleData
## Apply jitter to the selected columns
exampleData2[, columns_to_jitter] <- lapply(exampleData[, columns_to_jitter], function(x) jitter(x, factor = 0.1))

##more than 2 groups
exampleData$ageGroup <- ifelse(exampleData$age < 15, "low",
                               ifelse(exampleData$age >= 15 & exampleData$age <= 19, "middle", "high"))
exampleData2$ageGroup <- ifelse(exampleData2$age < 15, "low",
                                ifelse(exampleData2$age >= 15 & exampleData2$age <= 19, "middle", "high"))


#use a model

##non oplsda or ropls models
test_that("non ropls or oplsda models are rejected", {
  pcaModel <- PCA(data = exampleData[,1:5])
  expect_error(object = plotPRROC(model = pcaModel),
               regexp = "model is not from ropls or oplsda function, Try x and y input instead")
})

##non DA model
test_that("non DA models are rejected", {
  oplsModel <- oplsda(X = exampleData[,1:5],
                      Y = exampleData$age,
                      type = "OPLS")
  expect_error(object = plotPRROC(model = oplsModel),
               regexp = "Your model is not a descriminant analysis and not appropriate for PRROC. Consider using RMSE, MAE or other regression metrics")

})

##DA model
test_that("DA models are appropriately handled",{
  oplsdaModel <- oplsda(X = exampleData[,1:5],
                        Y = exampleData$status,
                        type = "OPLS")
  prroc <- plotPRROC(model = oplsdaModel)
expect_length(object = prroc, n = 3)
expect_contains(object = names(prroc), expected = c("plot", "PR", "ROC"))
expect_s3_class(object = prroc[["plot"]], class = "gg")
})

##PLSDA more than 3 groups

test_that("PLSDA with more than 2 groups is handled appropriately", {

  plsdaModel <- oplsda(X = exampleData[,1:5],
                       Y = exampleData$ageGroup,
                       type = "PLS")
  expect_warning(object = plotPRROC(model = plsdaModel),
                 regexp = "Your model has more than 2 classes. Consider the One-vs-Rest, One-vs-One or another scheme, the first two classes are the default.")
})

#use x and y
test_that("x and y handled appropriately", {
  Y = as.numeric(as.factor(exampleData$sex))
  prroc <- plotPRROC(x = exampleData$metab1[which(Y == 1)],
                     y = exampleData$metab1[which(Y == 2)])

  expect_length(object = prroc, n = 3)
  expect_contains(object = names(prroc), expected = c("plot", "PR", "ROC"))
  expect_s3_class(object = prroc[["plot"]], class = "gg")

})

