test_that("df works", {
  ag <- analyteGraph(model = as.data.frame(new_lipidData),
                     stat = "pval",
                     filter = 1:10,
                     optns = list(control = "Control",
                                  factor = new_lipidMetadata$Timepoint,
                                  lipidStart = "DAG(16:1_20:2)",
                                  lipidEnd = "LPI(20:4)",
                                  method = "BH",
                                  size = 2))

  #there are the two groups observed
  expect_contains(object = unique(ag[["data"]][["Group"]]),
                  expected = c("COVID", "MISC"))

  #the filter has been appropriately applied, filter of 10, 2 groups so 20 entries
  expect_equal(object = nrow(ag[["data"]]),
               expected = 20)

})


test_that("PCA model works", {
  pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)
  ag <- analyteGraph(model = pca,
                     stat = "pval",
                     filter = "none",
                     optns = list(control = "Control",
                                  factor = new_lipidMetadata$Timepoint,
                                  lipidStart = "DAG(16:1_20:2)",
                                  lipidEnd = "LPI(20:4)",
                                  method = "BH",
                                  size = 2))
#does it contain the 1st analyte (they are ordered by their significance so wont be the 1st)
expect_contains(object = ag[["data"]][["id"]], expected = colnames(new_lipidData)[1])

#should not contain what was removed by lipidStart
expect_false(object = "DAG(16:1_20:2)" %in% ag[["data"]][["id"]])

})

test_that("oplsda model works", {
  op<- oplsda(X = new_lipidData, Y = new_lipidMetadata$Timepoint, type = "PLS")
  ag <- analyteGraph(model = op,
                     stat = "pval",
                     filter = 1.3,
                     optns = list(control = "Control",
                                  factor = new_lipidMetadata$Timepoint,
                                  lipidStart = "DAG(16:1_20:2)",
                                  lipidEnd = "LPI(20:4)",
                                  method = "BH",
                                  size = 2))


  #should not contain what was removed by lipidStart
  expect_false(object = "DAG(16:1_20:2)" %in% ag[["data"]][["id"]])



})

test_that("three or more groups to plot", {
  classes <- c("Control", "Treatment", "misc", "meds")
  # Generate random indices to select from the classes
  random_indices <- sample(length(classes), nrow(new_lipidMetadata), replace = TRUE)
  # Add the new column "Class" with random entries
  new_lipidMetadata$Class <- classes[random_indices]

  ag <- analyteGraph(model = as.data.frame(new_lipidData),
                     stat = "fc",
                     filter = 1.3,
                     optns = list(control = "Control",
                                  factor = new_lipidMetadata$Class,
                                  lipidStart = "DAG(16:1_20:2)",
                                  lipidEnd = "LPI(20:4)",
                                  method = "BH",
                                  size = 2))

  #there are the three groups observed
  expect_contains(object = unique(ag[["data"]][["Group"]]),
                  expected = c("Treatment", "misc", "meds"))




})
