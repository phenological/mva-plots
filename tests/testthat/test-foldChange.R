test_that("foldChange calculates log2 fold change correctly for PCA", {

  # Generate some sample data
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])

  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)

  # Do PCA function for list option
  result <- PCA(data[,1:5], optns = list(factor = data$fact))
  log2fclist <- foldChange(model = result,
                           optns = list(factor = data$fact,
                                        control = "control"))

  #manual calc
  result$data$rawData$factor <- as.numeric(as.factor(data$fact))
  df <- result$data$rawData

  idx<- which(df[,"factor"] == 1)
  control <- df[idx, -which(colnames(df) == "factor")]
  treatment <- df[-idx,-which(colnames(df) == "factor")]

  #logmeans
  c <- log2(apply(X = control, MARGIN = 2, FUN = mean))
  t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))

  #log2 fold change
  expected_log2fc <- as.data.frame(c-t)
  expected_log2fc <- as.data.frame(expected_log2fc[1:(nrow(expected_log2fc)),])

  # Check if the calculated log2 fold change matches the expected values
  expect_identical(log2fclist[,1], expected_log2fc[,1])
  #expected_log2fc[,1], log2fcopls[,1]

})

test_that("foldChange calculates log2 fold change correctly for opls", {

  # Do oplsda for opls option
  result2<- oplsda(X = mtcars[,1:5], Y = mtcars$vs, type = "OPLS")
  log2fcopls <-foldChange(model = result2, optns = list(factor = mtcars$vs, control = "0"))

  #manual calc
  model <- result2
  df <- as.data.frame(model@suppLs[["x"]])
  df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]), ref = 0)

  idx<- which(df[,"factor"] == 1)
  control <- df[idx, -which(colnames(df) == "factor")]
  treatment <- df[-idx,-which(colnames(df) == "factor")]

  #logmeans
  c <- log2(apply(X = control, MARGIN = 2, FUN = mean))
  t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))

  #log2 fold change
  expected_log2fc <- as.data.frame(c-t)
  expected_log2fc <- as.data.frame(expected_log2fc[1:(nrow(expected_log2fc)),])

  # Check if the calculated log2 fold change matches the expected values
  expect_identical(log2fcopls[,1], expected_log2fc[,1])
  #expected_log2fc[,1], log2fcopls[,1]

})

test_that("can use with dataframe with more than 2 groups",{

  # Generate some sample data
  set.seed(123)
  data1 <- as.data.frame(matrix(rnorm(200), ncol = 5))
  data1[data1 < 0] <- abs(data1[data1 < 0])

  data1$fact <- sample(c("control", "treatment", "misc1", "misc2"), 4, replace = FALSE)
  original<- foldChange(model = data1[,1:5], optns=list(control = "control", factor = data1$fact))

  data2 <- rbind(data1[which(data1$fact == "control"),], data1[which(data1$fact == "treatment"),])
  test<- foldChange(model = data2[,1:5], optns=list(control = "control", factor = data2$fact))

  #is the first column control
  expect_equal(object = colnames(original)[1], expected = "control")

  #check correct group was assigned by: is the foldchange for treatment the same when there is all 4 factors and when there is just control and treatment
  expect_equal(object = as.numeric(original[,"treatment"]), expected = as.numeric(unlist(test)))

  #check group assignment for misc1
  data2 <- rbind(data1[which(data1$fact == "control"),], data1[which(data1$fact == "misc1"),])
  test<- foldChange(model = data2[,1:5], optns=list(control = "control", factor = data2$fact))
  expect_equal(object = as.numeric(original[,"misc1"]), expected = as.numeric(unlist(test)))

  #check group assignment for misc2
  data2 <- rbind(data1[which(data1$fact == "control"),], data1[which(data1$fact == "misc2"),])
  test<- foldChange(model = data2[,1:5], optns=list(control = "control", factor = data2$fact))
  expect_equal(object = as.numeric(original[,"misc2"]), expected = as.numeric(unlist(test)))

  log2fcdf<- foldChange(model = data1[,1:5], optns=list(control = "control", factor = data1$fact))

  #should have 4 columns in result
  expect_equal(object = length(log2fcdf), expected = 4)

  #check correct assignment for PCA object (a list)
  result <- PCA(data1[,1:5], optns = list(factor = data1$fact))
  log2fclist <- foldChange(model = result,
                           optns = list(factor = data1$fact,
                                        control = "misc1"))

  data2 <- rbind(data1[which(data1$fact == "misc1"),], data1[which(data1$fact == "treatment"),])
  test<- foldChange(model = data2[,1:5], optns=list(control = "misc1", factor = data2$fact))
  expect_equal(object = as.numeric(log2fclist[,"treatment"]), expected = as.numeric(unlist(test)))


})

