
test_that("Check if x has been correctly appended to the model when a matrix is supplied", {

  #matrix instead of data frame
  df<-as.matrix(mtcars)

  #opls object
  a <- oplsda(X = df[,1:5],
              Y = df[, 8],
              type = "OPLS")

  #ensure x is appended to model@suppLs
  expect_true("x" %in% names(a@suppLs),
              "'x' should be present in model@suppLs")

  #should have the number of elements as x variables supplied to oplsda function
  expect_equal(length(a@suppLs[["x"]]), ncol(df[,1:5]))
})

test_that("When performing OPLS, if number of levels is not 2, the correct action is taken", {

  #if there are less than 2 levels in your Y, it stops
  mtcars$paint <- factor("red")
  expect_error(oplsda(X = mtcars[,1:5], Y = mtcars$paint, type = "OPLS"),
               "Error: You have less than 2 levels in your Y. DA requires at least 2 levels.")

  #if there are more than 2 levels in your Y, it stops
  mtcars$paint <- factor(rep(c("red", "green", "blue"), length.out = nrow(mtcars)))
  expect_error(oplsda(X = mtcars[,1:5], Y = mtcars$paint, type = "OPLS"),
               "Error: You have more than 2 levels in your Y. OPLS-DA requires exactly 2 levels.")

  #if there are more than 2 levels in your Y, but one has no values, the empty one is removed, a model still built and a warning telling you the change in levels
  mtcars$paint <- rep(c("red", "green"), length.out = nrow(mtcars))
  mtcars$paint <- factor(mtcars$paint, levels = c("red", "blue", "green"))

  expect_warning(object = oplsda(X = mtcars[, 1:5], Y = mtcars$paint, type = "OPLS"), "Your levels in Y have been changed from 3 to 2. Old levels are red, blue, green. Your new levels are green, red")

})


test_that("When performing PLS, if number of levels is not 2, the correct action is taken", {

  #if there are less than 2 levels in your Y, it stops
  mtcars$paint <- factor("red")
  expect_error(oplsda(X = mtcars[,1:5], Y = mtcars$paint, type = "PLS"),
               "Error: You have less than 2 levels in your Y. DA requires at least 2 levels.")

  #if there are more than 2 levels in your Y, it doesn't stop
  mtcars$paint <- factor(rep(c("red", "green", "blue"), length.out = nrow(mtcars)))
  expect_no_error(oplsda(X = mtcars[,1:5], Y = mtcars$paint, type = "PLS"))

  #if there are more than 2 levels in your Y, but one has no values, the empty one is removed, a model still built and a warning telling you the change in levels
  mtcars$paint <- rep(c("red", "green"), length.out = nrow(mtcars))
  mtcars$paint <- factor(mtcars$paint, levels = c("red", "blue", "green"))

  expect_warning(object = oplsda(X = mtcars[, 1:5], Y = mtcars$paint, type = "PLS"), "Your levels in Y have been changed from 3 to 2. Old levels are red, blue, green. Your new levels are green, red")

})


#Model will build with metabom8 but not ropls without added code
test_that("OPLS models build even when o1 is not R1 significance", {

  X <- lipoData[,1:112]
  Y <- lipoData[,113:114]

  #oplsda
  model <- oplsda(X = X, Y = Y$category, type = "OPLS")
  test <- capture.output(model)
  expect_false(object = any(grepl("Empty 'opls' object", test)))

 #opls
  model <- oplsda(X = X, Y = Y$continuous, type = "OPLS")
  test <- capture.output(model)
  expect_false(object = any(grepl("Empty 'opls' object", test)))

})
