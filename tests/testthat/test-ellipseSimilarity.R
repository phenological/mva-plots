test_that("manual jaccardRatio scores plot works", {

  pca <- PCA(data = mtcars[,1:11],
             rank = 3,
             plot = FALSE)

  #a single plot
  ps<- plotScores(model = pca,
                  optns = list(color = as.factor(cars$vs),
                               ellipse ="color",
                               PCi = 2,
                               PCj = 3))


  # points representing two ellipses
  df<-ggplot_build(ps)$data[[4]]
  df$colour <- as.factor(df$colour)
  unique_factors <- unique(df$colour)

  #use sf package to find the overlap

  z1 <- df[which(df$colour == unique_factors[[1]]),c(2,3)]
  if (!identical(z1[1, ], z1[nrow(z1), ])) {
    # Add the first point to the end to close the polygon
    z1 <- rbind(z1, z1[1, ])
  }

  z2 <- df[which(df$colour == unique_factors[[2]]),c(2,3)]
  if (!identical(z2[1, ], z2[nrow(z2), ])) {
    # Add the first point to the end to close the polygon
    z2 <- rbind(z2, z2[1, ])
  }

  s1 = st_polygon(list(as.matrix(z1)))
  s2 = st_polygon(list(as.matrix(z2)))
  #And we can intersect them:

  s12intersect = st_intersection(s1,s2)
  st_area(s12intersect)
  st_area(s1)
  st_area(s2)

  plot(s1)
  plot(s2, add=TRUE)

  plot(s12intersect, add=TRUE, col="red")

})


test_that("oplsda with 2 groups object works",{
  pls <- oplsda(X = mtcars[,1:11],
                Y = as.factor(mtcars$vs),
                type = "OPLS",
                optns = list())

  plsps <- plotScores(model = pls,
                      optns = list(ellipse = "color"))

t <- ellipseSimilarity(ps = plsps, type = "jaccard")
t <- as.data.frame(t)

#should have a 2 by 2
expect_equal(dim(t), c(2, 2))

#all but the 1st row 2nd column are NAs
expect_true(all(is.na(t[-1, 2])))

t <- ellipseSimilarity(ps = plsps, type = "coefficient")
t <- as.data.frame(t)

#should have no NA
expect_false(any(is.na(t)))

#diagonal should be 1
expect_equal(diag(as.matrix(t)), rep(1, nrow(t)))

})

test_that("oplsda with 3 groups object works",{
  pls <- oplsda(X = mtcars[,1:11],
                Y = as.factor(mtcars$gear),
                type = "PLS",
                optns = list())

  plsps <- plotScores(model = pls,
                      optns = list(ellipse = "color"))

t <-  ellipseSimilarity(ps = plsps, type = "jaccard")
t <- as.data.frame(t)

#should have a 3 by 3
expect_equal(dim(t), c(3, 3))

#diagonal should be NAs
expect_equal(diag(as.matrix(t)), rep(NA_real_, nrow(t)))

t <- ellipseSimilarity(ps = plsps, type = "coefficient")
t <- as.data.frame(t)
#diagonal should be 1
expect_equal(diag(as.matrix(t)), rep(1, nrow(t)))

#groups 3 and 5 should have no overlap (positions 1,3 and 3,1 should be 0)

expect_equal(object = (t[1, 3]), expected = 0)

})


test_that("PCA  with 2 groups object works",{

#2 groups, grid
pca <- PCA(data = mtcars[,1:7],
           rank = 4,
           plot = FALSE)
psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$vs),
                                           ellipse ="color"))
t <- ellipseSimilarity(ps = psg, type = "jaccard")

#should have 6 entries
expect_equal(object = length(t), expected = 6)

#should be 2 by 2
expect_equal(object = dim(t[[1]]), expected = c(2,2))

#should have all but 1 as NA
expect_true(all(is.na(t[[1]][-1, 2])))

t <- ellipseSimilarity(ps = psg, type = "coefficient")
#should have 6 entries
expect_equal(object = length(t), expected = 6)

#should be 2 by 2
expect_equal(object = dim(t[[1]]), expected = c(2,2))

#should have no NA
expect_false(any(is.na(t[[1]])))

#2 groups single
psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$vs),
                                           ellipse ="color",
                                           PCi = 1,
                                           PCj = 2))

t <- ellipseSimilarity(ps = psg, type = "jaccard")
t <- ellipseSimilarity(ps = psg, type = "coefficient")
})

test_that("PCA with 3 groups object works",{
  pca <- PCA(data = mtcars[,1:7],
             rank = 4,
             plot = FALSE)
#3 groups single
psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$gear),
                                           ellipse ="color",
                                           PCi = 1,
                                           PCj = 2))

t <- ellipseSimilarity(ps = psg, type = "jaccard")

#column names should match the groups the ellipses are assigned (3, 4, 5)
expect_equal(object = colnames(t[[1]]), expected = c("3", "4", "5"))
#should be 3 by 3
expect_equal(object = dim(t[[1]]), expected = c(3,3))
#diagonal should be NA
expect_equal(object = as.vector(diag(as.matrix(t[[1]]))), expected = rep(NA_real_, 3))

t <- ellipseSimilarity(ps = psg, type = "coefficient")
#diagonal should be 1
expect_equal(object = as.vector(diag(as.matrix(t[[1]]))), expected = rep(1, 3))

#3 groups grid
psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$gear),
                                           ellipse ="color"))

t <- ellipseSimilarity(ps = psg, type = "jaccard")
expect_equal(object = length(t), expected = 6)
expect_equal(object = dim(t[[1]]), expected = c(3,3))


t <- ellipseSimilarity(ps = psg, type = "coefficient")

#PC1 vs PC3 should have a coefficient of 1 for groups 3 and 5 (position 1,3 in the df of coefficients)
expect_equal(object = t[["PC1 vs PC3"]][1,3], expected = 1)
})





