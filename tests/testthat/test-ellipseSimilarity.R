

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

load("~/Downloads/missForest_imputed_clinchem_ntree10 (1).rda")

replacements <- c('F' = 'Pfizer - La Jolla',
                  'L' = 'Eli Lilly & Co.',
                  'N' = 'NovoNordisk',
                  'R' = 'Hoffmann–La Roche',
                  'S' = 'The Pharmacia Corporation',
                  'D' = 'Bristol-Myers-Squibb')

## Perform replacements
clinchem_df$Company <- replacements[clinchem_df$Company]

#PCA
pca <- PCA(data = clinchem_df[sapply(clinchem_df, is.double)])

companyPalette <- c("Bristol-Myers-Squibb" = "#0000FF",
                    "Eli Lilly & Co." = "#00FFFF",
                    "Pfizer - La Jolla" = "#9633FF",
                    "NovoNordisk" = "#AAFF55",
                    "Hoffmann–La Roche" = "#FFAA00",
                    "The Pharmacia Corporation" = "#CC0000")

companyPCA <- plotScores(model = pca,
                         optns = list(plotTitle = "a) PCA plot(n = 1114)",
                                      size = 1,
                                      alpha = 0.8,
                                      PCi = 1,
                                      PCj = 2,
                                      ellipse = "color",
                                      color = clinchem_df$Company,
                                      colorTitle = "Company",
                                      discretePalette = companyPalette,
                                      theme = theme(legend.position = "bottom")))


