test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

exampleData <- mtcars

colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

exampleData <- exampleData [,1:9]

oplsdaModel <- oplsda(Y = (exampleData[, "status"]),
                      X = exampleData[,1:5],
                      type = "OPLS")

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

