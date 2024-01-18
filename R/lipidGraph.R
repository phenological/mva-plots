#' Lipid Graph
#'
#' Lipid plot of class vs side chain.
#'
#' @param stat The statistic you would like to use. Either log2 fold change "fc",
#' Cliff's Delta "cd", correlation "corr" or "external". "fc" and "cd" are
#' calculated automatically. "external" is an externally supplied statistic that
#' can be, for example, a p-value from lmm and needs to be provided in the
#' \code{optns}.
#' @param external If stat is set to external, set this parameter in the
#' \code{optns} list. Must be the same as the number of lipids.
#' @param model A model from PCA or oplsda function, or a dataframe of lipids.
#' @param color A parameter for the \code{optns} list. Color palette for discrete
#' values, you can assign colors to specific factors, example:
#' discretePalette = c("control" = "purple", "treatment" = "orange"). Or supply
#' a concatenated list, example (and the default): discretePalette =
#' c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE",
#'  "#4393C3", "#2166AC"). Hexadecimal or color names accepted.
#' @param shape A parameter for the \code{optns} list. A character of the shape
#' desired. Default shape is "circle".
#' @param alpha A parameter for the \code{optns} list. A numeric of the alpha
#' desired. Default size is 0.3.
#' @param theme A parameter for the \code{optns} list. Personalize the plot
#' theme you would like applied as you would using theme() in ggplot. Example:
#' theme = theme(legend.position = "left", text=element_text(size=5)).
#' @param guides A parameter for the \code{optns} list. Personalize the plot
#' legend/guide how you would like applied as you would using guides() in
#' ggplot. Example: guides = guides(color = guide_legend(title = "Color Legend"),
#' size = "none")
#' @param factor A parameter for the \code{optns} list. An object the same
#' length as the data in the model supplied that must have at least 2 unique
#' groups such as treatment and control. More than 2 is allowable.
#' @param control A parameter for the \code{optns} list to set which group with
#' the supplied factor you wish to set as the control for comparison to all
#' other groups.
#' @import stats
#' @import ggplot2
#' @examples
#' \dontrun{
#' # Load lipid data and metadata
#' lipid_data <- readRDS(system.file("extdata", "lipidData.rds", package = "mva.plots"))
#' lipid_metadata <- readRDS(system.file("extdata", "lipidMetadata.rds", package = "mva.plots"))
#'
#' # Generate a lipid graph
#' lg<- lipidGraph(model = lipidData,
#' stat = "cd",
#' optns = list(factor = (lipidMetadata$Class),
#'             control = "MISC"))
#' # View the graph
#' print(graph)
#' }
#'



lipidGraph <- function(model, stat = "fc", optns = list()){

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

  #PCA
  if(is(model)[1] == "list"){
    #id <- as.data.frame(colnames(model$data$rawData))
    id <- colnames(model$data$rawData)
    df <- model$data$rawData

    # #empty df
    # pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    # pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))

    #pcLoadings<-as.data.frame(abs(model$data$loadings[,PC]))
  }

  #opls
  if(is(model)[1] == "opls"){
    #id <- as.data.frame(colnames(as.data.frame(model@suppLs[["x"]])))
    id <-names(model@suppLs[["x"]])
    df <- as.data.frame(model@suppLs[["x"]])

    # #empty df
    # pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    # pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))

    #PC <-1
    #pcLoadings<-as.data.frame(abs(model@loadingMN[,PC]))
  }

  #df
  if(is(model)[1]== "data.frame"){
    id <- colnames(model)
    df <- model

    # #empty df
    # pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
    # pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

    df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))

    #no loadings as it's not a model
    #pcLoadings <- data.frame(matrix(0, nrow = ncol(model), ncol = 1))
  }

  unique_factors <- unique(df[,"factor"])


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
                     size = guide_legend(title = stat))
  } else{guides <- optns$guides}

########cliffs delta##########

  if(stat == "cd"){
    cd <- cliffsDelta(model = model, optns = optns)
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
  statistic <- fc
}

########external########
  if(stat == "external"){
    #add check that there is the correct length of this stat
    if(nrow(optns$external) == length(id)){
      statistic <- optns$external
    }else {stop("length of externally provided stat does not match the number of lipids") }
  }

########adjusted p-value########

  # if(!"method" %in% names(optns)){
  #   optns$method <- "bonferroni"
  # }

  # if(stat == "pval"){
  #   for(j in 2: length(unique_factors)){
  #
  #     df2 <- df[df[,"factor"] %in% c(1, j), ]
  #
  #      pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  #      for(i in 1:(ncol(df2)-1)){
  #        pval[i,]<-kruskal.test(df2[,i], df2[,"factor"])$p.value
  #       }
  #
  #     pvalUnadjusted[,j] <- pval
  #
  #     #rescaled and adjusted p-value
  # for(i in 1:(ncol(df2)-1)){
  #   pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
  # }
  #   }
  #
  #   # Create a mapping between numbers and words, rename pval dataframe columns
  #   mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))
  #
  #   testnames<- as.data.frame(mapping)
  #   testnames$rowName <- rownames(testnames)
  #
  #   for (i in seq_len(nrow(testnames))) {
  #     col_number <- as.numeric(testnames[i, 2])
  #     new_col_name <- as.character(testnames[i, 1])
  #     names(pvalUnadjusted)[col_number] <- new_col_name
  #     names(pvalRescaled)[col_number] <- new_col_name
  #   }
  #
  #   #if only 2 factors, just need the second column, since first is all NA (control to control)
  #   if(length(unique_factors) == 2){
  #     pvalUnadjusted <- as.data.frame(pvalUnadjusted[,2])
  #     pvalRescaled <- as.data.frame(pvalRescaled[,2])
  #   }
  #
  #   statistic <- pvalRescaled
  # }
  #



#########lipid data frame############

#make class and sc info
  lipids <- id
  lmc <- strsplit(lipids, "\\(")
  lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")

  r <- list()
  for (i in 1:length(lmc)) {
    struc <- strsplit(lmc[[i]][2], "_")[[1]]
    if (length(struc) == 1 | lmc[[i]][1] == "TAG") {
      totalCarbon <- gsub("[a-zA-Z\\-]+",
                          "",
                          strsplit(struc, ":")[[1]][1])
      if (lmc[[i]][1] == "TAG") {
        sideChain <- gsub("[a-zA-Z\\-]+",
                          "",
                          strsplit(lmc[[i]][2], "_")[[1]][2])
      } else {
        unsat <- gsub("[a-zA-Z\\-]+",
                      "",
                      strsplit(struc, ":")[[1]][2])
        sideChain <- paste0(totalCarbon, ":", unsat)
      }
    } else {
      t <- strsplit(struc, ":")
      sc <- unlist(lapply(t, function(x)
        as.numeric(
          gsub("[a-zA-Z\\-]+",
               "",
               x[1])
        )))
      unsatSc <- unlist(lapply(t, function(x)
        as.numeric(
          gsub("[a-zA-Z\\-]+",
               "",
               x[2])
        )))
      totalCarbon <- sum(sc)
      sideChain <- paste0(sc, ":", unsatSc)
      unsat <- sum(unsatSc)
    }
    r[[i]] <- c(lmc[[i]][1], totalCarbon, unsat, sideChain)
  }

  #find the max number of segments r has been split into for any single entry in the list
  l <- max(unlist(lapply(r, function(x) length(x))))

  #make all elements of r the length you determined as l with NA's in the empty slots
  r <- lapply(r, function(x) c(x, rep(NA, l-length(x))))

  #make r into data frame with a col for class, totalCarbon, unsat and sidechain
  lipidClass <- data.frame(do.call("rbind", r))
  colnames(lipidClass) <- c("class", "nC", "r", "sc1", "sc2")

ld<-cbind(statistic, id, lipidClass)

#which groups will be plotted
if("columns_to_plot" %in% names(optns)){
  columns_to_plot <- optns$columns_to_plot
} else{columns_to_plot <- colnames(ld)[2:(which(colnames(ld) == "id") - 1)]}

if(length(columns_to_plot) > length(optns$discretePalette)) {
  warning("You have more groups to plot than colors supplied, the default is 8 colors. Please supply the same or more number of colors as number of groups to plot.")
}

  # Filter the data frame to include only the relevant columns
  plot_data <- ld[, c("class", "sc1", "id",columns_to_plot)]

  # Reshape the data frame to long format manually
  plot_data_long <- reshape(plot_data,
                            idvar = c("class", "sc1", "id"),
                            varying = list(columns_to_plot),
                            v.names = c("Value"),
                            times = columns_to_plot,
                            direction = "long")


  colnames(plot_data_long) <- c("class", "sc1", "id", "Group", "Value")

########### Create the ggplot###########
  lipidGraph <- ggplot(plot_data_long, aes(x = class,
                                           y = sc1,
                                           color = Group)) +
                geom_jitter(aes(size = abs(Value)),
                            position = position_jitter(height = 0.1,
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



