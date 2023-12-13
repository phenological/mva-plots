source("test_data.R")

test_that("O-PLS-DA model works with predictions", {
  ###Set up data####
  oplsdaModel <- oplsda(Y = (exampleData[, "status"]),
                        X = exampleData[,1:5],
                        type = "OPLS")

  #predict
  predictModel <- oplsdaPredict(model = oplsdaModel,
                                newdata = exampleData2[,1:5],
                                optns = list(real = exampleData2$status))

  ####do you get ortho and pred scores####
  expect_contains(names(predictModel), expected = c("orthoScoreMN", "predScoreMN"))

  ####are there the correct number of Scores####
  expect_equal(object = length(predictModel$orthoScoreMN)/ncol(predictModel$orthoScoreMN),
               expected = nrow(exampleData2))

  ####is predY as expected (a character)####
  expect_type(object = predictModel[["predY"]],
              type = "character")

  ####confusionMatrix exists####
  #sum of all elements should be same as number of rows in second data set
  expect_equal(object = sum(predictModel[["confusionMatrix"]][["table"]]), expected = nrow(exampleData2))

})

#OPLS
test_that("O-PLS model works", {
  ####Set up data####

  oplsdaModel <- oplsda(Y = (exampleData[, "age"]),
                        X = exampleData[,1:5],
                        type = "OPLS")
  #prediction
  ####no confusion matrix with warning####
  expect_warning(object = predictModel <-oplsdaPredict(model = oplsdaModel,
                                        newdata = exampleData2[,1:5],
                                        optns = list(real = exampleData2$age)),
                 regexp = "Your Y is not a factor or factorizable character, therefore no confusion matrix is supplied",
                 )

  expect_warning({
    predictModel <- oplsdaPredict(model = oplsdaModel,
                                  newdata = exampleData2[, 1:5],
                                  optns = list(real = exampleData2$age))

    # Continue with other expectations
    ####do you get ortho and pred scores####
    expect_contains(names(predictModel), expected = c("orthoScoreMN", "predScoreMN"))
    ####are there the correct number of Scores####
    expect_equal(object = length(predictModel$orthoScoreMN), expected = nrow(exampleData2))
    ####is the predY as expected (a double)####
    #note that expect_type is based on typeof(), not class() or is()
    expect_type(object = predictModel[["predY"]], type = "double")
  })

})

#PLSDA
test_that("PLS-DA model works", {
  #Set up data
  oplsdaModel <- oplsda(Y = (exampleData[, "status"]),
                        X = exampleData[,1:5],
                        type = "PLS",
                        optns = list(predI = 2))
  #predict
  predictModel <- oplsdaPredict(model = oplsdaModel,
                                newdata = exampleData2[,1:5],
                                optns = list(real = exampleData2$status))

  ####Should be no ortho scores####
  expect_equal(object = length(predictModel$orthoScoreMN),
               expected = 0)

  ####should be 2 pred scores components####
  expect_equal(object = ncol(predictModel$predScoreMN),
               expected = 2)

  ####are there the correct number of Scores####
  expect_equal(
    object = length(predictModel$predScoreMN) / ncol(predictModel$predScoreMN),
    expected = nrow(exampleData2)
  )

  ####is the predY as expected (a character)####
  #note that expect_type is based on typeof(), not class() or is()
  expect_type(object = predictModel[["predY"]], type = "character")

  expect_warning({
  oplsdaModel <- oplsda(Y = (exampleData[, "ageGroup"]),
                        X = exampleData[,1:5],
                        type = "PLS")

  predictModel <- oplsdaPredict(model = oplsdaModel,
                                newdata = exampleData2[,1:5],
                                optns = list(real = as.factor(exampleData2$ageGroup)))


  ####confusionMatrix exists####
  #sum of all elements should be same as number of rows in second data set
  expect_equal(object = sum(predictModel[["confusionMatrix"]][["table"]]), expected = nrow(exampleData2))

  })

})

#PLS
test_that("PLS model works", {
  #Set up data
  oplsdaModel <- oplsda(Y = (exampleData[, "age"]),
                        X = exampleData[,1:5],
                        type = "PLS")
####no confusion matrix with warning####

expect_warning(object = oplsdaPredict(model = oplsdaModel,
                                      newdata = exampleData2[,1:5],
                                      optns = list(real = exampleData2$age)),
               regexp = "Your Y is not a factor or factorizable character, therefore no confusion matrix is supplied")

expect_warning({
  predictModel <- oplsdaPredict(model = oplsdaModel,
                                newdata = exampleData2[,1:5],
                                optns = list(real = exampleData2$age))

  ####are there the correct number of Scores####
  expect_equal(object = length(predictModel$predScoreMN)/ncol(predictModel$predScoreMN), expected = nrow(exampleData2))

  ####is the predY as expected (a double)####
  #note that expect_type is based on typeof(), not class() or is()
  expect_type(object = predictModel[["predY"]], type = "double")

  ####Should be no ortho scores####
  expect_equal(object = length(predictModel$orthoScoreMN),
               expected = 0)
})

})


