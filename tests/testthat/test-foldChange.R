test_that("foldChange calculates log2 fold change correctly for PCA", {

  # Generate some sample data
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])

  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)

  # Do PCA function for list option
  result <- PCA(data[,1:5], optns = list(factor = data$fact))
  log2fclist <- foldChange(model = result, optns = list(factor = data$fact))

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
  log2fcopls <-foldChange(model = result2, optns = list(factor = mtcars$vs))

  #manual calc
  model <- result2
  df <- as.data.frame(model@suppLs[["x"]])
  df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))

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
  data <- as.data.frame(matrix(rnorm(200), ncol = 5))
  data[data < 0] <- abs(data[data < 0])

  data$fact <- sample(c("control", "treatment", "misc1", "misc2"), 4, replace = FALSE)

  model <- data[,1:5]
  optns<-list(control = "control", factor = data$fact)

  if(is(model)[1] == "data.frame"){
    df <- model
    df$factor <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
    # Initialize an empty data frame to store log2fc values
    log2fc_df <- data.frame(matrix(NA, nrow = ncol(model), ncol = 1))
  }

  # logmean for control
  idx<- which(df[,"factor"] == 1)
  control <- df[idx,]
  c <- log2(apply(X = control, MARGIN = 2, FUN = mean))

  # Dynamically assigning factors for one to one calculations
  unique_factors <- unique(df$factor)

  # Calculate log2fc, including for one to one calculations
  for (i in 2:length(unique_factors)) {

    # logmean treatment
    idx <- which(df[,"factor"] == i)
    treatment <- df[idx, ]
    t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))

    # log2 fold change
    log2fc <- as.data.frame(c - t)
    log2fc <- as.data.frame(log2fc[1:(nrow(log2fc) - 1), ])

    # Assign the log2fc as a column in the data frame
    col_name <- paste0("log2fc_", i)
    log2fc_df[, col_name] <- log2fc
  }

  #track the variable to the assigned factor, use optns$factor and df$factor to make the list

  # Create a mapping between numbers and words
  mapping <- setNames(unique(optns$factor), unique(df$factor))

  test<- as.data.frame(mapping)

  colnames(log2fc_df)<- test[[1]]

  if(length(unique_factors) == 2){
    log2fc_df <- as.data.frame(log2fc_df[,2])
  }




  #log2fcdf<- foldChange(model = data[,1:5], optns = list(factor = data$fact, control = "control"))

  #should have 4 columns in result
  #expect_equal(object = length(log2fcdf), expected = 4)


})

