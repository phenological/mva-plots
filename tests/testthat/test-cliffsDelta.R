
test_that("cliffsDelta calculates correctly", {

# Generate some sample data
set.seed(123)
data <- as.data.frame(matrix(rnorm(100), ncol = 5))
data[data < 0] <- abs(data[data < 0])

data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)

# Do PCA function for list option
result <- PCA(data[,1:5], optns = list(factor = as.factor(data$fact)))
cdlist <- cliffsDelta(model = result, optns = list(factor = as.factor(data$fact)))

# Do oplsda for opls option
result2<- oplsda(X= data[,1:5], Y = data$fact, type = "OPLS")
cdopls <-cliffsDelta(model = result2, optns = list(factor = data$fact))

##expected cliffs delta using stats via calculating U statistic (W wilcoxon statistic)

n <- table(data$fact)[1]
m <- table(data$fact)[2]

 U <- list()
 for(i in 1:(ncol(data)-1)){
   formula <- as.formula(paste(colnames(data)[i]," ~ fact", sep=""))
   model <- wilcox.test(formula, data = data)
   U[[i]] <- model[["statistic"]]
 }
    U <- (unlist(U))
    expected_cd <- (U/(n*m)-0.5)*2
    expected_cd <-
   # expected_cd<- as.data.frame(expected_cd)

# Check if the calculated cliff's deltas matches the expected values, use the absolute values since Wilcoxon U statistic might not necessarily match the direction of the effect
expect_identical(unname(round(abs(cdlist[,1]),12)),
                        unname(round(abs(expected_cd),12)),
                               unname(round(abs(cdopls[,1]),12)))

})

