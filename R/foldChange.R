#' Fold Change
#'
#' The log2 fold change. if you're calculating the fold change from control to
#' treatment, log2fc is log2(treatment) - log2(control).
#'
#' @param model A data frame, PCA or oplsda object from mva.plots.
#' @param optns A list passed to foldChange with additional arguments.
#' \itemize{
#'    \item{fun} {Either mean or median for the foldchange calculation. The
#'    default is mean.}
#'    \item{factor} {An object the same length as the data in the model supplied
#'    that must have at least 2 unique groups such as treatment and control. More
#'    than 2 is allowable.}
#'    \item{control} {Character to set which group with the supplied factor you wish
#'    to set as the control for comparison to all other groups. If your factor is
#'    already numeric, when specifying the control, supply it as a character, for
#'    example, control = "0". If not manually set, the unique factor assigned one
#'    from the supplied factor will be automatically selected}
#' }
#' @return Creates a dataframe of foldchanges. For more than 2 groups, pairwise
#' foldchange to the selected control will be returned.
#' @export


foldChange <- function(model = model, optns = optns){

  if(!"fun" %in% names(optns)){
    optns$fun <- "mean"
  }

  if (!"control" %in% names(optns)) {
    optns$control <- 1
    # #print warning
    # warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
  }

  if(is(model)[1] == "list"){
    #model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
    df <- model$data$rawData
    df$factor <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
    # Initialize an empty data frame to store log2fc values
    log2fc_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }

  if(is(model)[1] == "opls"){
    df <- as.data.frame(model@suppLs[["x"]], check.names = F)
    #df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    df$factor <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))
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
  idx<- which(df$factor == 1)
  control <- df[idx,]
  c <- log2(apply(X = control, MARGIN = 2, FUN = optns$fun))

  # Dynamically assigning factors for one to one calculations
  unique_factors <- unique(df$factor)

  # Calculate log2fc, including for one to one calculations
  for (i in 2:length(unique_factors)) {

    # logmean treatment
    idx <- which(df[,"factor"] == i)
    treatment <- df[idx, ]
    t <- log2(apply(X = treatment, MARGIN = 2, FUN = optns$fun))

    # log2 fold change
    log2fc <- as.data.frame(t - c, check.names =F)
    log2fc <- as.data.frame(log2fc[1:(nrow(log2fc) - 1), ], check.names = F)

    # Assign the log2fc as a column in the data frame
    col_name <- paste0("log2fc_", i)
    log2fc_df[, col_name] <- log2fc
  }

  #track the variable to the assigned factor, use optns$factor and df$factor to make the list

  # Create a mapping between numbers and words, rename log2fc dataframe columns
  mapping <- setNames(unique(optns$factor), unique(df$factor))

  testnames<- as.data.frame(mapping)
  testnames$rowName <- rownames(testnames)

  for (i in seq_len(nrow(testnames))) {
    col_number <- as.numeric(testnames[i, 2])
    new_col_name <- as.character(testnames[i, 1])
    names(log2fc_df)[col_number] <- new_col_name
  }

#if only 2 factors, just need the second column, since first is all NA (control to control)
  if(length(unique_factors) == 2){
    #log2fc_df <- as.data.frame(log2fc_df[,2])
    log2fc_df <- log2fc_df[, 2, drop = FALSE]
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
