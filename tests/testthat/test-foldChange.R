test_that("foldChange calculates log2 fold change correctly", {

  # Generate some sample data
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])

  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)

  # Do PCA function for list option
  result <- PCA(data[,1:5], optns = list(factor = data$fact))
  log2fclist <- foldChange(model = result, optns = list(factor = data$fact))

  # Do oplsda for opls option
  result2<- oplsda(X= data[,1:5], Y = data$fact, type = "OPLS")
  log2fcopls <-foldChange(model = result2, optns = list(factor = data$fact))

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
  expect_identical(log2fclist[,1], expected_log2fc[,1], log2fcopls[,1])

})
