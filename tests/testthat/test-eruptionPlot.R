
test_that("PCA produced model is handled appropriately", {
  set.seed(123)
  data <- as.data.frame(matrix(rnorm(100), ncol = 5))
  data[data < 0] <- abs(data[data < 0])
  data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)
  data$age = seq(from = 18, to = 60, length.out = 20)
  data$sex <- sample(c("female", "male"), 20, replace = TRUE)

  #list object
  a <- PCA(data=data[,1:5])

  #expect stop and error if more than 2 groups
  expect_error(object = suppressWarnings(eruptionPlot(model = a,
                                                      optns = list(factor=data[,"age"],
                                                                   method = "none",
                                                                   color = "pval"))),
               regexp = "Error: You have more than 2 levels in your factor")

  #expect warning when control not specified
  expect_warning(object = eruptionPlot(model = a,
                                       optns = list(factor=data[,"sex"],
                                                   method = "none",
                                                   color = "pval")),
                 "No control specified in optns for factor. The first entry was set as the control")

  p <-suppressWarnings(eruptionPlot(model = a,
                                    optns = list(factor=data[,"sex"],
                                                 method = "fdr",
                                                 color = "loadings")))

  #correct class
  expect_s3_class(object= p[["plots"]][["eruptionPlot"]], class = "gg")
  expect_equal(length(p), 2)
  expect_equal(length(p[["plots"]]), 4)
})


test_that("oplsda produced model is handled appropriately", {
  #opls object
  a <- oplsda(X=mtcars[,1:5], Y = mtcars$vs, type = "OPLS")

  #eruption
  p <- suppressWarnings(eruptionPlot(model = a, optns = list(factor=mtcars[,"vs"])))

  #is eruptionData appended
  expect_true("eruptionData" %in% names(p@suppLs),
              "'eruptionData' should be present in p@suppLs")

  #are all elements of eruptionData present
  elements_to_check <- c("cd", "fc", "pval", "pvalRaw", "loadings", "id", "corr")
  expect_true(all(elements_to_check %in% names(p@suppLs[["eruptionData"]])))

  #is eruptionPlot appended
  expect_true("eruptionPlot" %in% names(p@suppLs),
              "'eruptionPlot' should be present in p@suppLs")

  #is eruptionPlot a gg object
  tryCatch(
    expect_s3_class(p@suppLs[["eruptionPlot"]], "gg"),
    error = function(e) {
      stop("'eruptionPlot' should be a gg object")
    }
  )
})

test_that("externally provided pvalue works", {
  ########adjusted external p-value########
  model <- as.data.frame(new_lipidData)
  df <- as.data.frame(new_lipidData)

  optns <- list(factor = new_lipidMetadata$sample_batch,
                control = "Non-burn",
                color = "pval",
                method = "none")
  # #empty df
  pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
  pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

  df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
  unique_factors <- unique(df[,"factor"])

  for(j in 2: length(unique_factors)){

    df2 <- df[df[,"factor"] %in% c(1, j), ]

    pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
    for(i in 1:(ncol(df2)-1)){
      pval[i,]<-kruskal.test(df2[,i], df2[,"factor"])$p.value
    }

    pvalUnadjusted[,j] <- pval

    #rescaled and adjusted p-value
    for(i in 1:(ncol(df2)-1)){
      pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
    }
  }

  # Create a mapping between numbers and words, rename pval dataframe columns
  mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))

  testnames<- as.data.frame(mapping)
  testnames$rowName <- rownames(testnames)

  for (i in seq_len(nrow(testnames))) {
    col_number <- as.numeric(testnames[i, 2])
    new_col_name <- as.character(testnames[i, 1])
    names(pvalUnadjusted)[col_number] <- new_col_name
    names(pvalRescaled)[col_number] <- new_col_name
  }

  pvalExternal <- pval

  pca <- PCA(data = new_lipidData, plot = F, rank = 3)
  ep <-suppressWarnings(eruptionPlot(model = pca, optns = list(factor = new_lipidMetadata$sample_batch,
                                                               control = "Non-burn",
                                                               color = "pval",
                                                               method = "none",
                                                               external = pvalExternal$matrix.NA..nrow...ncol.df....1..ncol...1.)))

  expect_equal(object = max(ep[["data"]][["eruptionData"]][["pval"]]),
               expected = max(abs(log10(pvalExternal$matrix.NA..nrow...ncol.df....1..ncol...1.))))
})

