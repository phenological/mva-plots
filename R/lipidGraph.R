#' Lipid Graph
#'
#' Lipid plot of class vs side chain.
#'
#' @param model A model from PCA or oplsda function, or a data frame of lipids.
#' @param stat The statistic you would like to use. Either log2 fold change "fc",
#' Cliff's Delta "cd", correlation "corr" or "external". "fc" and "cd" are
#' calculated automatically. "external" is an externally supplied statistic that
#' can be, for example, a p-value from lmm and needs to be provided in the
#' \code{optns}.
#' @param optns A list for additional options:
#'   \itemize{
#'    \item{external} {If stat is set to external supply the chosen column from
#'    data frame here. Must be the same as the number of lipids.}
#'    \item{lipidStart} {A character of the name of the first lipid in the range
#'    you wish to include. If you have more than lipids in your model, it is essential
#'    to provide this and lipidEnd. Use the same syntax as you see in your model.}
#'    \item{lipidEnd} {A character of the name of the last lipid in the range
#'    you wish to include. Must be provided if lipidStart is provided.}
#'    \item{color} {If there is one Group to be plotted, this can be changed to
#'    "Direction" which will use colors according to if the stat is positive or
#'    negative.}
#'    \item{discretePalette} {Color palette for discrete values, you can assign colors to
#'    specific factors, example:
#'    discretePalette = c("control" = "purple", "treatment" = "orange"). Or supply
#'    a concatenated list, example (and the default):
#'    discretePalette = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
#'    "#92C5DE", "#4393C3", "#2166AC"). Hexadecimal or color names accepted.}
#'    \item{shape} {A character of the shape, Default shape is "circle".}
#'    \item{alpha} {A numeric of the alpha desired. Default size is 0.3.}
#'    \item{theme} {Personalize the plot theme you would like applied as you would
#'    using theme() in ggplot. Example:
#'    theme = theme(legend.position = "left", text=element_text(size=5)).}
#'    \item{guides} {Personalize the plot legend/guide how you would like applied as
#'    you would using guides() in ggplot. Example:
#'    guides = guides(color = guide_legend(title = "Color Legend"), size = "none")}
#'    \item{factor} {An object the same length as the data in the model supplied
#'    that must have at least 2 unique groups such as treatment and control. More
#'    than 2 is allowable.}
#'    \item{control} {Character to set which group with the supplied factor you wish
#'    to set as the control for comparison to all other groups.}
#' }
#'
#' @examples
#' \dontrun{
#' # Load lipid data and metadata
#' lipid_data <- readRDS(system.file("extdata", "lipidData.rds", package = "mva.plots"))
#' lipid_metadata <- readRDS(system.file("extdata", "lipidMetadata.rds", package = "mva.plots"))
#'
#' # Generate a lipid graph
#' lg<- lipidGraph(model = lipidData,
#'                 stat = "cd",
#'                 optns = list(factor = (lipidMetadata$Timepoint),
#'                              control = "MISC"))
#' # View the graph
#' print(graph)
#'
#' #To plot only one Group
#' lg<- lipidGraph(model = lipidData,
#'                 stat = "cd",
#'                 optns = list(factor = (lipidMetadata$Timepoint),
#'                              control = "Control",
#'                              columns_to_plot = "COVID",
#'                              color = "Direction"
#'                              discretePalette = c("positive" = "purple", "negative" = "orange")))
#' }
#' @import stats
#' @import ggplot2
#' @export

lipidGraph <- function(model, stat = "fc", filter = "none", optns = list()){

  #check factors to see if any group is empty
  if (any(is.na(optns$factor))) {
    stop("One of your factors is NA, please change this before running lipidGraph")
  }

  if (!"control" %in% names(optns)) {
    optns$control <- 1
    #print warning
    warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
  }

  if("method" %in% names(optns)){
    method = optns$method
  }else{
    method <- "bonferroni"
  }

  #if factor is a data table, it needs to be changed to work with cd and fc
  if(is(optns$factor)[1] == "data.table" | is(optns$factor)[1] == "tbl_df"){
    optns$factor <- unlist(optns$factor)
  }



  #PCA
  if(is(model)[1] == "list"){
    #id <- as.data.frame(colnames(model$data$rawData))
    id <- colnames(model$data$rawData)
    df <- model$data$rawData

    # #empty df
    pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))

    #pcLoadings<-as.data.frame(abs(model$data$loadings[,PC]))
  }

  #opls
  if(is(model)[1] == "opls"){
    #id <- as.data.frame(colnames(as.data.frame(model@suppLs[["x"]])))
    id <-names(model@suppLs[["x"]])
    df <- as.data.frame(model@suppLs[["x"]])

    # #empty df
    pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))

    #PC <-1
    #pcLoadings<-as.data.frame(abs(model@loadingMN[,PC]))
  }

  #df
  if(is(model)[1] == "data.frame"){
    id <- colnames(model)
    df <- model

    # #empty df
    pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))

    #no loadings as it's not a model
    #pcLoadings <- data.frame(matrix(0, nrow = ncol(model), ncol = 1))
  }


  unique_factors <- unique(df$factor)
  #unique_factors <- unique(df[,"factor"])

  if(!("discretePalette" %in% names(optns))){
    optns$discretePalette <- c("#66C2A5",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A6D854",
                               "#FFD92F",
                               "#E5C494",
                               "#B3B3B3")

  }

  #shape
  if(!("shape" %in% names(optns))){
    optns$shape = "circle"}

  #alpha
  if(!("alpha" %in% names(optns))){
    optns$alpha = 0.3}

  #theme
  if(!("theme" %in% names(optns))){
    theme <- theme()
  } else{theme <- optns$theme}

  #guides
  if(!("guides" %in% names(optns))){
    guides <- guides(color = guide_legend(title = "Group"),
                     size = guide_legend(title = paste0("|",stat,"|")))
  } else{guides <- optns$guides}

  ########cliffs delta##########

  if(stat == "cd"){
    cd <- cliffsDelta(model = model, optns = optns)
    if(length(cd) == 1){
      cd <- cbind(NA, cd)
    }
    statistic <- cd
  }

  ##########correlations between scaled + centered original data and scores######
  if(stat == "corr"){
    if(is(model)[1] == "list"){
      corr <- abs(t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC))))
    }

    if(is(model)[1] == "opls"){
      corr <- abs(t(as.data.frame(cor(model@scoreMN[,PC], model@suppLs[["xModelMN"]]))))
    }

    if(is(model)[1] == "data.frame"){
      #no correlation because there's no scores (not a model)
      corr <- data.frame(matrix(0, nrow = ncol(model), ncol = 1))
    }

    statistic <- corr
  }

  ##########Fold change#########

  if(stat == "fc"){
    fc <- foldChange(model = model, optns = optns)
    if(length(fc) == 1){
      fc <- cbind(NA, fc)
    }
    statistic <- fc
  }

  ########external########
  if(stat == "external"){
    #add check that there is the correct length of this stat
    if(nrow(optns$external) == length(id)){

      if(length(optns$external) == 1){
        external <- cbind(NA, optns$external)
      }
      statistic <- optns$external
      significance <- optns$external
    }else {stop("length of externally provided stat does not match the number of lipids") }
  }

  ########Calculate internal adjusted p-value########
  #only calculate it if an external p-value has not been provided
  if(!"method" %in% names(optns)){
    optns$method <- "bonferroni"
  }


  if(!stat == "external"){
    #create the pvalues
    for(j in 2: length(unique_factors)){

      df2 <- df[df$factor %in% c(1, j), ]
      df2<- as.data.frame(df2)
      pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
      for(i in 1:(ncol(df2)-1)){
        pval[i,] <- kruskal.test(df2[,i], df2$factor)$p.value
      }

      pvalUnadjusted[,j] <- pval

      #rescaled and adjusted p-value
      for(i in 1:(ncol(df2)-1)){
        pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
      }
    }

    df<-as.data.frame(df)
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

    #if only 2 factors, just need the second column, since first is all NA (control to control)
    if(length(unique_factors) == 2){
      # pvalUnadjusted <- as.data.frame(pvalUnadjusted[,2])
      # pvalRescaled <- as.data.frame(pvalRescaled[,2])
      # pvalRescaled <- cbind(NA, pvalRescaled)
      pvalUnadjusted[,1] <- NA
      pvalRescaled[,1] <- NA
    }

    significance <- pvalRescaled
    colnames(significance) <- paste0("sig_", colnames(significance))

    if(stat == "pval"){
      statistic<- pvalRescaled
    }

  }


  #########shorten###################
  if("lipidStart" %in% names(optns)){
    if(!"lipidEnd" %in% names(optns)){
      stop("lipidEnd has not been supplied")
    }else{
      start_col_index <- which((id) == optns$lipidStart)
      end_col_index <- which((id) == optns$lipidEnd)
      id <- id[start_col_index:end_col_index]
      statistic <- statistic[start_col_index:end_col_index,]
      significance <- significance[start_col_index:end_col_index,]
    }
  }

  #########lipid data frame############
  #if the labels us . instead of - and :
  if (any(grepl("\\.", id))) {
    # Apply substitution
    id <- sub("\\(P\\.", "(P-", id)
    id <- sub("\\(O\\.", "(O-", id)
    id <- gsub("\\.", ":", id)
  }

  #make class and sc info
  lipids <- id
  lmc <- strsplit(lipids, "\\(")
  lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])),
                how = "replace")
  r <- list()
  for (i in 1:length(lmc)) {
    struc <- strsplit(lmc[[i]][2], "_")[[1]]
    if (length(struc) == 1 | lmc[[i]][1] == "TAG") {
      totalCarbon <- gsub("[a-zA-Z\\-]+", "", strsplit(struc,
                                                       ":")[[1]][1])
      if (lmc[[i]][1] == "TAG") {
        sideChain <- gsub("[a-zA-Z\\-]+", "", strsplit(lmc[[i]][2],
                                                       "_")[[1]][2])
        unsat <- 1
      }
      else {
        unsat <- gsub("[a-zA-Z\\-]+", "", strsplit(struc,
                                                   ":")[[1]][2])
        sideChain <- paste0(totalCarbon, ":", unsat)
      }
    }
    else {
      t <- strsplit(struc, ":")
      sc <- unlist(lapply(t, function(x) as.numeric(gsub("[a-zA-Z\\-]+",
                                                         "", x[1]))))
      unsatSc <- unlist(lapply(t, function(x) as.numeric(gsub("[a-zA-Z\\-]+",
                                                              "", x[2]))))
      totalCarbon <- sum(sc)
      sideChain <- paste0(sc, ":", unsatSc)
      unsat <- sum(unsatSc)
    }
    r[[i]] <- c(lmc[[i]][1], totalCarbon, unsat, sideChain)
  }
  l <- max(unlist(lapply(r, function(x) length(x))))
  r <- lapply(r, function(x) c(x, rep(NA, l - length(x))))
  lipidClass <- data.frame(do.call("rbind", r))

  if(length(lipidClass)<5){
    lipidClass$X5<- NA
  }

  colnames(lipidClass) <- c("class", "nC", "r", "sc1", "sc2")

  ld<-cbind(statistic, id, lipidClass, significance)

  #which groups will be plotted
  if("columns_to_plot" %in% names(optns)){
    columns_to_plot <- optns$columns_to_plot
  } else{columns_to_plot <- colnames(ld)[2:(which(colnames(ld) == "id") - 1)]}

  if(length(columns_to_plot) > length(optns$discretePalette)) {
    warning("You have more groups to plot than colors supplied, the default is 8 colors. Please supply the same or more number of colors as number of groups to plot.")
  }

  if(!"external" %in% names(optns)){
    significance_columns_to_plot <- paste0("sig_", columns_to_plot)

    #significance_columns_to_plot <- colnames(ld)[grep("Significance", colnames(ld))]
  }


  # Filter the data frame to include only the relevant columns
  plot_data <- ld[, c("class", "sc1", "id", columns_to_plot, significance_columns_to_plot)]

  # Reshape the data frame to long format manually
  plot_data_long <- reshape(plot_data,
                            idvar = c("class", "sc1", "id"),
                            varying = list(columns_to_plot, significance_columns_to_plot),
                            v.names = c("Value", "sig"),
                            times = columns_to_plot,
                            direction = "long")


  colnames(plot_data_long) <- c("class", "sc1", "id", "Group", "Value", "Sig")

  ####filtering#####
  if(filter == "none"){
    plot_data_long <-  plot_data_long
  }

  if(is(filter)[1] == "numeric"){
    plot_data_long <- plot_data_long[plot_data_long$Sig > filter, ]

  }

  if(is(filter)[1] == "integer"){

  }
  #only allow color to be set as pos and neg when there is one group and change guide title
  if(length(unique(plot_data_long$Group)) == 1){

    if("color" %in% names(optns)){
      if(optns$color == "Direction"){
        optns$color <- ifelse(plot_data_long$Value >= 0, "positive", "negative")
        if("guides" %in% names(optns)){
          guides <- optns$guides
        }else{guides <- guides(color = guide_legend(title = "Sign"),
                               size = guide_legend(paste0("|",stat,"|")))}
      }
    }else{optns$color <- plot_data_long$Group}

  }else{optns$color <- plot_data_long$Group}


  ########### Create the ggplot###########
  lipidGraph <- ggplot(data = plot_data_long, aes(x = class,
                                                  y = sc1,
                                                  color = optns$color)) +
    geom_jitter(aes(size = abs(Value)),
                position = position_jitter(height = 0,
                                           width = 0.3),
                alpha = optns$alpha,
                shape = optns$shape) +
    xlab("Lipid Class") +
    ylab("Side-Chain Length") +
    theme_bw() +
    theme(panel.grid.major = element_line(color = "gray95"),
          panel.grid.minor = element_blank()
    ) +
    scale_color_manual(values  = optns$discretePalette,
                       na.value = "grey50" ) +
    theme +
    guides



  return(lipidGraph)
}


