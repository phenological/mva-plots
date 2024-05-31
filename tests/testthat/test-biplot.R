
test_that("pca biplot works",{
  pca<- PCA(data = iris[,1:4])
  expect_no_error(object = biplot1(model = pca,
                                   PCi = 1,
                                   PCj = 2,
                                   optns = list(color = iris$Species,
                                                shape = "triangle",
                                                alpha = 0.7,
                                                ellipse = "color")))




})


test_that("pls biplot works", {
  pl<-
    oplsda(X = iris[,1:4], Y = iris$Species, type = "PLS")

expect_no_error(object = biplot1(model = pl,
                                 optns = list(PCi = 1,
                                              PCj = 2,
                                              shape = "triangle",
                                              alpha = 0.7,
                                              ellipse = "color",
                                              plotTitle = "new biplot")))

})


test_that("opls biplot works", {
  opl <-
    oplsda(X = mtcars[,1:7], Y = as.factor(mtcars$vs), type = "OPLS")
  expect_no_error(object =   biplot1(model = opl,
                                     optns = list(
                                       shape = "triangle",
                                       alpha = 0.7,
                                       ellipse = "color"))  )

})

