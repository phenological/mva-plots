
test_that("PCA labels outliers", {
  data(iris)
  a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
  b <- plotScores(model = a,
                  optns = list(color = iris[,"Species"],
                               ellipse = "t",
                               outlierLabels = iris$Species,
                               discretePalette = c("setosa" = "purple",
                                                   "versicolor" = "orange",
                                                   "virginica" = "steelblue"),
                               colorTitle = "Flower Species",
                               plotTitle = "Iris PCA grid",
                               thresh = 3,
                               alpha = 0.7))

#are there 3 entries, one for each plot in the grid
  c <- b[["data"]][["outliers"]]
   expect_length(object = c, n = 3)

#are they names appropriately
   expect_contains(object = names(b[["data"]][["outliers"]]), expected = "PC2vPC3")

   #do they have entries appropriate for species
   d <- b[["data"]][["outliers"]][["PC1vPC2"]][1]

   expect_contains(object = levels(d[[1]]), expected = c("setosa", "versicolor", "virginica"))

  #for a single plot istead of a grid:

  b <- plotScores(model = a,
                  optns = list(color = iris[,"Species"],
                               outlierLabels = row.names(iris),
                               PCi = 2,
                               PCj = 3,
                               ellipse = "hotellings",
                               discretePalette = c("setosa" = "purple",
                                                   "versicolor" = "orange",
                                                   "virginica" = "steelblue"),
                               colorTitle = "Flower Species",
                               plotTitle = "Iris PCA grid",
                               alpha = 0.7))

  expect_false(object = is.null(b[["plots"]][["pcaSingle"]][["data"]][["optns$outlierLabels"]]))

})

test_that("PLS model works", {
  pl<-
    oplsda(X = iris[,1:4], Y = iris$Species, type = "PLS")

 a <- plotScores(model = pl,
             optns = list(color = iris$Species,
                          PCi = 1,
                          PCj = 3,
                          ellipse = "hotellings",
                          outlierLabels = iris$Species))

#the outliers should be appended to the model and have the same levels as species
expect_contains(object = levels(a@suppLs[["outlierID"]]), expected = unique(iris$Species))


})

test_that("OPLS model works", {
 opls <-  oplsda(X = mtcars[,1:7], Y = as.factor(mtcars$vs), type = "OPLS")

 ps <- plotScores(model = opls,
            optns = list(color = as.factor(mtcars$vs),
                         ellipse = "t",
                         outlierLabels = row.names(mtcars)))

 a <- ps@suppLs[["outlierID"]]

 #the outliers should be appended to the model and should be entries from the rownames (what was set as the outlier labels)
 expect_contains(object = row.names(mtcars), expected = a)
})
