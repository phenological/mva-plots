
test_that("cliffsDelta calculates correctly for PCA", {

# Generate some sample data
set.seed(123)
data <- as.data.frame(matrix(rnorm(100), ncol = 5))
data[data < 0] <- abs(data[data < 0])

data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)

# Do PCA function for list option
result <- PCA(data[,1:5], optns = list(factor = as.factor(data$fact)))
cdlist <- cliffsDelta(model = result, optns = list(factor = as.factor(data$fact), control = "control"))

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

   # expected_cd<- as.data.frame(expected_cd)

# Check if the calculated cliff's deltas matches the expected values, use the absolute values since Wilcoxon U statistic might not necessarily match the direction of the effect
expect_identical(unname(round(abs(cdlist[,1]),12)),
                        unname(round(abs(expected_cd),12)))

})

test_that("cliffDelta calculates correctly for opls", {

  # Do oplsda for opls option
  result2<- oplsda(X = mtcars[,1:5], Y = mtcars$vs, type = "OPLS")
  cdopls <-cliffsDelta(model = result2, optns = list(factor = mtcars$vs, control = 0))

  model = result2
  optns = list(factor = mtcars$vs, control = 0)
  if(is(model)[1] == "opls"){
    df <- as.data.frame(model@suppLs[["x"]])
    df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    cd_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }


  #manual calc
  model <- result2
  df <- as.data.frame(model@suppLs[["x"]])
  df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))

  #control
  idx<- which(df[,"factor"] == 1)
  control <- df[idx,]
  c<-list()
  for(i in 1:(ncol(control)-1)){
    c[[i]] <- sort(control[,i])
  }

  treatment <- df[-idx,]
  t<-list()
  for(i in 1:(ncol(treatment)-1)){
    t[[i]] <- sort(treatment[,i])
  }

  #dominance matrix
  d<-list()
  for(i in 1:(ncol(df)-1)){
    d[[i]] <-sign(outer(t[[i]], c[[i]], FUN="-"))
  }

  #cliffs delta
  cd<-list()
  for(i in 1:(ncol(df)-1)){
    cd[[i]]<-mean(d[[i]])
  }

  #if rescale is required
  n <- table(df$factor)[1]
  m <- table(df$factor)[2]
  rescale.factor = (n*m-1)/(n*m)

  for(i in 1:(ncol(df)-1)){
    ifelse(test = abs(cd[[i]]) == 1,
           yes = cd[[i]]*rescale.factor,
           no = cd)
  }
  unlist(cd)
  expected_cd<-as.vector(t(as.data.frame(cd)))

  # Check if the calculated cd matches the expected values
  expect_identical(as.numeric(cdopls[,1]), expected_cd)

})

test_that("can use with dataframe with more than 2 groups",{
# Generate some sample data
set.seed(123)
data1 <- as.data.frame(matrix(rnorm(200), ncol = 5))
data1[data1 < 0] <- abs(data1[data1 < 0])

data1$fact <- sample(c("control", "treatment", "misc1", "misc2"), 4, replace = FALSE)
original<- cliffsDelta(model = data1[,1:5], optns=list(control = "control", factor = data1$fact))

data2 <- rbind(data1[which(data1$fact == "control"),], data1[which(data1$fact == "treatment"),])
test<- cliffsDelta(model = data2[,1:5], optns=list(control = "control", factor = data2$fact))

#is the first column control
expect_equal(object = colnames(original)[1], expected = "control")

#is the cliff's delta for treatment the same when there is all 4 factors and when there is just control and treatment
expect_equal(object = as.numeric(original[,"treatment"]), expected = as.numeric(unlist(test)))

cliffDf<- cliffsDelta(model = data1[,1:5], optns=list(control = "control", factor = data1$fact))

#should have 4 columns in result
expect_equal(object = length(cliffDf), expected = 4)

})


