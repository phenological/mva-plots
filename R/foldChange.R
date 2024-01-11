#' Fold Change
#'

foldChange <- function(model = model, optns = optns){

  if(is(model)[1] == "list"){
    model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
    df <- model$data$rawData
    # Initialize an empty data frame to store log2fc values
    log2fc_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }

  if(is(model)[1] == "opls"){
    df <- as.data.frame(model@suppLs[["x"]])
    df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    # Initialize an empty data frame to store log2fc values
    log2fc_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }

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

  return(log2fc_df)
}


# foldChange  <- function(model, optns = optns){
#
#   if(is(model)[1] == "list"){
#     model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
#     df <- model$data$rawData
#   }
#
#   if(is(model)[1] == "opls"){
#     df <- as.data.frame(model@suppLs[["x"]])
#     df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
#   }
#
#   if(is(model)[1] == "data.frame"){
#     df <- model
#     df[,"factor"] <- as.numeric(as.factor(optns$factor))
#   }
#
#   idx<- which(df[,"factor"] == 1)
#   control <- df[idx,]
#   treatment <- df[-idx,]
#
#   #logmeans
#   c <- log2(apply(X = control, MARGIN = 2, FUN = mean))
#   t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))
#
#   #log2 fold change
#   log2fc <- as.data.frame(c-t)
#   log2fc <- as.data.frame(log2fc[1:(nrow(log2fc)-1),])
#   return(log2fc)
# }

# #Volcano plot
# results = cbind(LogFC, p_adj)
# results = as.data.frame(results)
# results$liponame <- colnames(df3[1:112])
#
# library(ggplot2)
# library(ggrepel)
# ggplot(data=results, aes(x=LogFC, y=p_adj)) + geom_point() + theme_minimal() +
# geom_hline(yintercept=-log10(0.05), col="red") +
#   geom_vline(xintercept=c(-0.6, 0.6), col="red")+
# geom_label_repel(aes(label = liponame), size = 2) +
# labs(title = "Lipoprotein Univariate Anova Analysis", x="log2FoldChange", y="-log10 adj.p-val")
