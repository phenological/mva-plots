#plotScores

test_that("PCA is handled correctly", {
  # Generate some sample data
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])
  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)
  data$age = seq(from = 18, to = 60, length.out = 20)
  data$sex <- sample(c("female", "male"), 20, replace = TRUE)

  ##PCA object
  a<- PCA(data = data[,1:5], rank = 5)

  #####grid
  resultBasic <- plotScores(model = a)

  #correct class
  expect_s3_class(object= resultBasic[["plots"]][["pcaGrid"]], class = "ggmatrix")
  expect_equal(length(resultBasic), 2)
  expect_equal(length(resultBasic[["plots"]][["pcaGrid"]][["plots"]]), 9)

  #should be 5 variables in rawData
  expect_equal(length(resultBasic[["data"]][["rawData"]]),
               5)

  # resultBasic[["plots"]][["pcaGrid"]][["plots"]][[1]][["labels"]]

  resultComplex <- plotScores(model = a,
                              optns = list(color = data$sex,
                                           shape = data$sex,
                                           thresh = 3,
                                           ellipse = "hotellings",
                                           outlierLabels=row.names(data)))

  #should be 3 lists of outliers
  expect_equal(length(resultComplex[["data"]][["outliers"]]), 3)

  #Addition of shape, colour, ellipse and outlier labels should be evident in the following layer
  #overall length
  expect_contains(names(ggplot_build(resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]])[["plot"]][["labels"]])
                  ,c("colour","shape","label"))

  #colour
  expected_colour <- "optns$color"
  actual_colour <- ggplot_build(resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]])[["plot"]][["labels"]][["colour"]]
  expect_equal(actual_colour, expected_colour, info = "Labels colour should be optns$color")

  #shape
  expected_shape <- "optns$shape"
  actual_shape <- ggplot_build(resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]])[["plot"]][["labels"]][["shape"]]
  expect_equal(actual_shape, expected_shape, info = "Labels shape should be optns$shape")

  ##Why were these labels ever expected??? AB
  # #ellipse
  # # ymin and ymax should exist
  # expect_true(exists("ymin", where = resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]][["labels"]]),
  #             info = "Labels should have ymin")
  # expect_true(exists("ymax", where = resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]][["labels"]]),
  #             info = "Labels should have ymax")

  #outlier labels
  # resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]][["labels"]][["label"]]
  expected_label <- "outlierID"
  actual_label <- ggplot_build(resultComplex[["plots"]][["pcaGrid"]][["plots"]][[1]])[["plot"]][["labels"]][["label"]]
  expect_equal(actual_label, expected_label, info = "Labels shape should be outlierID")

  #####individual
})


test_that("opls object is handled correctly", {
  ##opls object
  exampleData <- mtcars

  colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

  exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
  exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
  rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

  model <- oplsda(Y = (exampleData[, "status"]),
                  X = exampleData[,1:5],
                  type = "OPLS")

  #make plotscores for oplsda model with ellipse and outliers included

  ps<- plotScores(model = model,
                  optns = list(
                    color = exampleData[,"status"],
                    ellipse = "hotellings",
                    outlierLabels = rownames(exampleData)
                  )
  )

  #is the plot appended to the model
  expect_true("ScoresPlot" %in% names(ps@suppLs), "ScoresPlot should exist in ps@suppLs")

  #is the outlier information appended to the model
  expect_true("outlierID" %in% names(ps@suppLs), "outlierID should exist in ps@suppLs")

})

test_that("can have a flat plotscore for O-PLS(DA) model", {
  exampleData <- mtcars

  colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

  exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
  exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
  rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

  #discriminant Analysis
  model <- oplsda(Y = (exampleData[, "status"]),
                  X = exampleData[,1:5],
                  type = "OPLS")

  ps <- plotScores(model = model, flat = TRUE,
                   optns = list(color = exampleData[,"status"]))


#has the plot appended to the model
  expect_contains(object = names(ps@suppLs), expected = "ScoresPlot")

#are there 2 colors present at different heights
  expect_equal(object = length(unique(ps@suppLs[["ScoresPlot"]][["data"]][["y"]])), expected = 2)

  #continuous Y
  model <- oplsda(Y = (exampleData[, "age"]),
                  X = exampleData[,1:5],
                  type = "OPLS")


  ps <- plotScores(model = model, flat = TRUE,
                   optns = list(color = exampleData[,"age"],
                                colorTitle = "age"))

  #has the plot appended to the model
  expect_contains(object = names(ps@suppLs), expected = "ScoresPlot")
  #is there only one height
expect_true(object = length(unique(ps@suppLs[["ScoresPlot"]][["data"]][["y1"]])) > 2)
})

test_that("you can change things via extra in optns",{
  # Generate some sample data
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])
  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)
  data$age = seq(from = 18, to = 60, length.out = 20)
  data$sex <- sample(c("female", "male"), 20, replace = TRUE)

  ##PCA object
  ###grid of plots
  a<- PCA(data = data[,1:5], rank = 5)

                    pm<- plotScores(model = a,
                              optns = list(color = data$sex,
                                           shape = data$sex,
                                           theme = theme(legend.position = "left"),
                              extra = scale_shape_manual(labels = c("female", "male"),values = c(8, 17))
                              )
                                           )
                    #did the correct shapes get used

                    shapes <- unique(ggplot_build(pm[["plots"]][["pcaGrid"]][["plots"]][[1]])$data[[3]]$shape)
                    expect_contains(object = shapes, expected = c(8,17))

###single plot
                    ps<- plotScores(model = a,
                               optns = list(PCi = 1,
                                            PCj = 2,
                                            color = data$sex,
                                            shape = data$sex,
                                            theme = theme(legend.position = "left"),
                                            extra = scale_shape_manual(labels = c("female", "male"),values = c(8, 17))
                               )
                    )
                    #did the correct shapes get used

                    shapes <- unique(ggplot_build(ps[["plots"]][["pcaSingle"]])$data[[1]]$shape)
                    expect_contains(object = shapes, expected = c(8,17))

#OPLSDA
                    oplsda <- oplsda(X = new_lipidData, Y = new_lipidMetadata$sample_batch, type = "OPLS")
                    ps<- plotScores(model = oplsda,
                                    optns = list(color = new_lipidMetadata$sample_batch,
                                                 shape = new_lipidMetadata$Timepoint,
                                                 theme = theme(legend.position = "left"),
                                                 extra = scale_shape_manual(labels = c("MISC", "COVID", "Control"),values = c(8, 17, 2))
                                    )
                    )

                    shapes <- unique(ggplot_build(ps@suppLs[["ScoresPlot"]])$data[[1]]$shape)
                    expect_contains(object = shapes, expected = c(8, 17, 2))
})

