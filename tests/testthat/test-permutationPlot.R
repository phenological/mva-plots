test_that("permutation plot is appended to model", {
  data(mtcars)
  a <- oplsda(X = mtcars[,1:7], Y = mtcars[,8], type = "OPLS", optns = list(permI = 50))
  b <- permutationPlot(model = a, optns=list(shape="square", colorQ = "#FF5500", colorR = "pink", size = 5))

  #is the plot appended to the model
  expect_true("permutationPlot" %in% names(b@suppLs), "permutationPlot should exist in b@suppLs")


})
