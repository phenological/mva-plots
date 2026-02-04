
test_that("pca biplot works",{
  pca<- PCA(data = iris[,1:4])
  expect_no_error(object = biplot(model = pca,
                                   PCi = 1,
                                   PCj = 2,
                                   optns = list(color = iris$Species,
                                                shape = "triangle",
                                                alpha = 0.7,
                                                ellipse = "color")))

  bp <- biplot(model = pca,
               PCi = 1,
               PCj = 2,
               optns = list(color = iris$Species,
                            shape = "triangle",
                            alpha = 0.7,
                            ellipse = "color"))

  #did it append to the model
  expect_contains(object = names(bp[["plots"]]), expected = "biplot")

})


test_that("pls biplot works", {
  pl<-
    oplsda(X = iris[,1:4], Y = iris$Species, type = "PLS")

expect_no_error(object = biplot(model = pl,
                                PCi = 1,
                                PCj = 3,
                                 optns = list(shape = "triangle",
                                              alpha = 0.7,
                                              ellipse = "color",
                                              plotTitle = "new biplot")))

bp <- biplot(model = pl,
             PCi = 1,
             PCj = 3,
             optns = list(shape = "triangle",
                          alpha = 0.7,
                          ellipse = "color",
                          plotTitle = "new biplot"))

#did the plot get appended to the model
expect_contains(object = names(bp@suppLs), expected = "biplot")

})


test_that("opls biplot works", {
  opl <-
    oplsda(X = mtcars[,1:7],
           Y = as.factor(mtcars$vs),
           type = "OPLS")

  expect_no_error(object = biplot(model = opl,
                                  optns = list(shape = "triangle",
                                                alpha = 0.7,
                                                ellipse = "color")))

  bp <- biplot(model = opl,
               optns = list(shape = "triangle",
                            alpha = 0.7,
                            ellipse = "color"))
  expect_contains(object = names(bp@suppLs), expected = "biplot")

})

test_that("max number of labels allowed is 50", {
  pca<- PCA(data = lipoData[,1:51])

  expect_error(object = biplot(model = pca,
                                zoom = 1,
                                optns = list(color = lipoData$category)))

})

test_that("zoom works", {
  pca <- PCA(data = lipoData[,1:50])

  expect_error(object = biplot(model = pca,
                                zoom = 1.1,
                                optns = list(color = lipoData$category)))


  # biplot(model = pca,
  #               PCi = 1,
  #               PCj = 3,
  #         zoom = 0.25,
  #         optns = list(color = lipoData$category))



})
