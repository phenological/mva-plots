#' #' analyteGraph
#' #'
#' #' Analyte versus statistic.
#' #'
#' #' @param model A model from PCA or oplsda function, or a data frame of lipids.
#' #' @param stat The statistic you would like to use. Either log2 fold change "fc",
#' #' Cliff's Delta "cd", p-value "pval" or "external". "fc", "pval" and "cd" are
#' #' calculated automatically. The p-value is the log 10 kruskal wallis p-value
#' #' adjusted by "bonferroni". You may select other p-value adjustments using method
#' #' in the optns. "external" is an externally supplied statistic that can be, for
#' #' example, a p-value from lmm and needs to be provided in the optns list.
#' #' @param filter Either "none", a range (1:10) or a numeric (1.3) for significance.
#' #' Based on internally calculated log 10 kruskal wallis p-value. A range will
#' #' provide only the top significant lipids (eg 1:10 will graph only top 10). A
#' #' numeric will filter out anything below this significance (eg 1.3, anything
#' #' with log 10 p-value less than 1.3 is excluded). "none" is the default and will
#' #' apply no filter. This is the only option available with externally provided stats.
#' #' \code{optns}.
#' #' @param optns A list for additional options:
#' #'   \itemize{
#' #'   \item{method} {adjustment for the internally calculated p-value. Default is
#' #'   "bonferroni", but can be set to "holm", "hochberg", "hommel", "bonferroni",
#' #'   "BH", "BY", "fdr" or "none".}
#' #'    \item{external} {If stat is set to external supply the chosen column from
#' #'    data frame here. Must be the same as the number of lipids. Cannot filter
#' #'    when providing an external stat. Internal p-value will not be calculated.}
#' #'    \item{lipidStart} {A character of the name of the first lipid in the range
#' #'    you wish to include. If you have more than lipids in your model, it is essential
#' #'    to provide this and lipidEnd. Use the same syntax as you see in your model.}
#' #'    \item{lipidEnd} {A character of the name of the last lipid in the range
#' #'    you wish to include. Must be provided if lipidStart is provided.}
#' #'    \item{color} {If there is one Group to be plotted, this can be changed to
#' #'    "Direction" which will use colors according to if the stat is positive or
#' #'    negative.}
#' #'    \item{discretePalette} {Color palette for discrete values, you can assign colors to
#' #'    specific factors, example:
#' #'    discretePalette = c("control" = "purple", "treatment" = "orange"). Or supply
#' #'    a concatenated list, example (and the default):
#' #'    discretePalette = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
#' #'    "#92C5DE", "#4393C3", "#2166AC"). Hexadecimal or color names accepted.}
#' #'    \item{shape} {A character of the shape, Default shape is "circle".}
#' #'    \item{alpha} {A numeric of the alpha desired. Default size is 0.3.}
#' #'    \item{theme} {Personalize the plot theme you would like applied as you would
#' #'    using theme() in ggplot. Example:
#' #'    theme = theme(legend.position = "left", text=element_text(size=5)).}
#' #'    \item{guides} {Personalize the plot legend/guide how you would like applied as
#' #'    you would using guides() in ggplot. Example:
#' #'    guides = guides(color = guide_legend(title = "Color Legend"), size = "none")}
#' #'    \item{factor} {An object the same length as the data in the model supplied
#' #'    that must have at least 2 unique groups such as treatment and control. More
#' #'    than 2 is allowable.}
#' #'    \item{control} {Character to set which group with the supplied factor you wish
#' #'    to set as the control for comparison to all other groups.}
#' #' }
#' #' @import stats
#' #' @import ggplot2
#' #' @export
#'
#'
#' analyteGraph <- function(model, stat = "fc", filter = "none", optns = list()){
#'
#'   AnnoR <- AnnoR[match(expV$sampleID, AnnoR$sampleID),]
#'   idx <- which(AnnoR$Class == "Other")
#'
#'   AnnoR <- AnnoR[-idx,]
#'
#'   expV2 <- expV[,3:ncol(expV)]
#'   expV2 <- apply(expV2, 2, function(x) as.numeric(as.character(x)))
#'   expV2 <- as.data.frame(expV2)
#'
#'   model <- as.data.frame(x)
#'   stat <- "fc"
#'   filter <- 1.3
#'   optns <- list(control = "Control",
#'                 factor = AnnoR$Class,
#'                 lipidStart = "CE(14:0)",
#'                 lipidEnd = "TAG(60:11_FA22:6)")
#'
#'   #check factors to see if any group is empty
#'   if (any(is.na(optns$factor))) {
#'     stop("One of your factors is NA, please change this before running lipidGraph")
#'   }
#'
#'   if (!"control" %in% names(optns)) {
#'     optns$control <- 1
#'     #print warning
#'     warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
#'   }
#'
#'   if(!"method" %in% names(optns)){
#'     optns$method <- "bonferroni"
#'   }
#'
#'   #if factor is a data table, it needs to be changed to work with cd and fc
#'   if(is(optns$factor)[1] == "data.table" | is(optns$factor)[1] == "tbl_df"){
#'     optns$factor <- unlist(optns$factor)
#'   }
#'   #PCA
#'   if(is(model)[1] == "list"){
#'     id <- colnames(model$data$rawData)
#'     df <- model$data$rawData
#'
#'     # #empty df
#'     pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'     pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'
#'     df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
#'   }
#'
#'   #opls
#'   if(is(model)[1] == "opls"){
#'     id <-names(model@suppLs[["x"]])
#'     df <- as.data.frame(model@suppLs[["x"]])
#'
#'     # #empty df
#'     pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'     pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'
#'     df[,"factor"] <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))
#'   }
#'
#'   #df
#'   if(is(model)[1] == "data.frame"){
#'     id <- colnames(model)
#'     df <- model
#'
#'     # #empty df
#'     pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'     pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#'
#'     df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
#'
#'   }
#'
#'   unique_factors <- unique(df$factor)
#'
#'   if(!("discretePalette" %in% names(optns))){
#'     optns$discretePalette <- c("#66C2A5",
#'                                "#FC8D62",
#'                                "#8DA0CB",
#'                                "#E78AC3",
#'                                "#A6D854",
#'                                "#FFD92F",
#'                                "#E5C494",
#'                                "#B3B3B3")
#'
#'   }
#'
#'   #shape
#'   if(!("shape" %in% names(optns))){
#'     optns$shape = "circle"}
#'
#'   #alpha
#'   if(!("alpha" %in% names(optns))){
#'     optns$alpha = 1}
#'
#'   #size
#'   if(!("size" %in% names(optns))){
#'     optns$size = 1}
#'
#'   #theme
#'   if(!("theme" %in% names(optns))){
#'     theme <- theme()
#'   } else{theme <- optns$theme}
#'
#'   #guides
#'   if(!("guides" %in% names(optns))){
#'     guides <- guides(color = guide_legend(title = "Group"),
#'                      size = guide_legend(title = paste0("|",stat,"|")))
#'   } else{guides <- optns$guides}
#'
#'   ########cliffs delta##########
#'
#'   if(stat == "cd"){
#'     cd <- cliffsDelta(model = model, optns = optns)
#'     if(length(cd) == 1){
#'       cd <- cbind(NA, cd)
#'     }
#'     statistic <- cd
#'   }
#'
#'   ##########Fold change#########
#'
#'   if(stat == "fc"){
#'     fc <- foldChange(model = model, optns = optns)
#'     if(length(fc) == 1){
#'       fc <- cbind(NA, fc)
#'     }
#'     statistic <- fc
#'   }
#'
#'   ########Calculate internal adjusted p-value########
#'   #only calculate it if an external p-value has not been provided
#'   if(!stat == "external"){
#'     #create the pvalues
#'     for(j in 2: length(unique_factors)){
#'
#'       df2 <- df[df$factor %in% c(1, j), ]
#'       df2<- as.data.frame(df2)
#'       pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
#'       for(i in 1:(ncol(df2)-1)){
#'         pval[i,] <- kruskal.test(df2[,i], df2$factor)$p.value
#'       }
#'
#'       pvalUnadjusted[,j] <- pval
#'
#'       #rescaled and adjusted p-value
#'       for(i in 1:(ncol(df2)-1)){
#'         pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
#'       }
#'     }
#'
#'     df<-as.data.frame(df)
#'     # Create a mapping between numbers and words, rename pval dataframe columns
#'     mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))
#'
#'     testnames<- as.data.frame(mapping)
#'     testnames$rowName <- rownames(testnames)
#'
#'     for (i in seq_len(nrow(testnames))) {
#'       col_number <- as.numeric(testnames[i, 2])
#'       new_col_name <- as.character(testnames[i, 1])
#'       names(pvalUnadjusted)[col_number] <- new_col_name
#'       names(pvalRescaled)[col_number] <- new_col_name
#'     }
#'
#'     #if only 2 factors, just need the second column, since first is all NA (control to control)
#'     if(length(unique_factors) == 2){
#'       pvalUnadjusted[,1] <- NA
#'       pvalRescaled[,1] <- NA
#'     }
#'
#'     significance <- pvalRescaled
#'     colnames(significance) <- paste0("sig_", colnames(significance))
#'
#'     if(stat == "pval"){
#'       statistic<- pvalRescaled
#'     }
#'
#'   }
#'
#'   #########shorten###################
#'   if("lipidStart" %in% names(optns)){
#'     if(!"lipidEnd" %in% names(optns)){
#'       stop("lipidEnd has not been supplied")
#'     }else{
#'       start_col_index <- which((id) == optns$lipidStart)
#'       end_col_index <- which((id) == optns$lipidEnd)
#'       id <- id[-(start_col_index:end_col_index)]
#'       statistic <- statistic[-(start_col_index:end_col_index),]
#'       significance <- significance[-(start_col_index:end_col_index),]
#'     }
#'   }
#'
#'   ad<-cbind(statistic, id, significance)
#'   test <- ad[, colSums(is.na(ad)) != nrow(ad)]
#'
#'
#'
#'   ##### groups to be plotted####
#'   if("columns_to_plot" %in% names(optns)) {
#'     # if ("external" %in% names(optns)) {
#'     #   sorted_unique_factor <- sort(unique(optns$factor))
#'     #   sorted_external_names <- sort(names(optns$external))
#'     #
#'     #   # Check if the sorted vectors are equal
#'     #   if (all(sorted_unique_factor == sorted_external_names)) {
#'     #     columns_to_plot <- optns$columns_to_plot
#'     #   } else{
#'     #     columns_to_plot <- colnames(optns$external)
#'     #   }
#'     # } else{
#'       columns_to_plot <- optns$columns_to_plot
#'     # }
#'
#'   } else{
#'     columns_to_plot <- colnames(ad)[2:(which(colnames(ad) == "id") - 1)]
#'   }
#'
#'   if(length(columns_to_plot) > length(optns$discretePalette)) {
#'     warning("You have more groups to plot than colors supplied, the default is 8 colors. Please supply the same or more number of colors as number of groups to plot.")
#'   }
#'
#'   significance_columns_to_plot <- paste0("sig_", columns_to_plot)
#'
#'   plot_data <- ad[, c("id", columns_to_plot, significance_columns_to_plot)]
#'
#'   var <- list(columns_to_plot,
#'               significance_columns_to_plot)
#'
#'   # Reshape the data frame to long format manually
#'   plot_data_long <- reshape(plot_data,
#'                             idvar = c("id"),
#'                             varying = var,
#'                             v.names = c("Value", "sig"),
#'                             times = columns_to_plot,
#'                             direction = "long")
#'
#'   colnames(plot_data_long) <- c("id", "Group", "Value", "Sig")
#'
#'   ####filtering#####
#'   if(is(filter)[1] == "character"){
#'     plot_data_long2 <-  plot_data_long
#'   }
#'
#'   if(is(filter)[1] == "numeric"){
#'     plot_data_long2 <- plot_data_long[plot_data_long$Sig > filter, ]
#'
#'   }
#'
#'   if(is(filter)[1] == "integer"){
#'     ordered_df <- plot_data_long[order(plot_data_long$Sig, decreasing = TRUE), ]
#'
#'     # Retain the top x rows
#'     plot_data_long2 <- ordered_df[filter, , drop = FALSE]
#'   }
#'
#'   #put back matching group values
#'   idx <- unique(plot_data_long2$id)
#'
#'   matched_rows <- plot_data_long[plot_data_long$id %in% idx, ]
#'
#'   # Calculate the absolute values
#'   matched_rows$abs_Value <- abs(matched_rows$Value)
#'
#'   # Initialize an empty data frame to store the results
#'   result <- data.frame(analyte = character(), Value = numeric())
#'
#'   # Loop through each unique analyte
#'   for (ana in unique(matched_rows$analyte)) {
#'     # Subset the data for the current analyte
#'     subset_data <- matched_rows[matched_rows$analyte == ana, ]
#'
#'     # Find the row with the maximum absolute Value
#'     max_row <- subset_data[which.max(subset_data$abs_Value), ]
#'
#'     # Append the row to the result data frame
#'     result <- rbind(result, max_row)
#'   }
#'
#'   # Remove the abs_Value column
#'   matched_rows$abs_Value <- NULL
#'
#'   # result will now contain the analyte and the corresponding Value with the highest absolute Value
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'   matched_rows <- matched_rows[order(matched_rows$Value, decreasing = TRUE), ]
#'   matched_rows$order
#'
#'
#'   matched_rows$plot_order <- 1:nrow(matched_rows)
#'
#'
#'   ggplot(matched_rows, aes(x = Value, y = id)) +
#'     geom_segment(aes(x = 0,
#'                      xend = Value,
#'                      y = id,
#'                      yend = id,
#'                      color = Group),
#'                  size = optns$size) +
#'     geom_point(aes(color = Group),
#'                size = optns$size,
#'                shape = optns$shape) +
#'     geom_vline(xintercept = 0,
#'                linetype = "dashed",
#'                color = "black",
#'                linewidth = 1) +
#'     geom_vline(xintercept = c(-2, -1, 1, 2),
#'                linetype = "dashed",
#'                color = "gray",
#'                linewidth = 1) +
#'     xlab(paste0(stat)) +
#'     theme_bw() +
#'     theme(axis.title.y = element_blank()) +
#'     labs(tag = "", caption =  paste0("*Calculated with respect to ", optns$control)) +
#'     theme(legend.position = "none")
#'
#'
#'
#'   ggplot(data = plot_data_long, aes(x = Value, color = Group)) +
#'     geom_hline(yintercept = 0,
#'                linetype = "dashed",
#'                color = "black",
#'                linewidth = 1) +
#'     geom_hline(yintercept = c(-2, -1, 1, 2),
#'                linetype = "dashed",
#'                color = "gray",
#'                linewidth = 1) +
#'     coord_flip() +
#'     geom_linerange(data = statT_noLip,
#'                    aes(ymin = minMisc,
#'                        ymax = `MIS-C`),
#'                    color = "dodgerblue",
#'                    size = 2) +
#'     geom_point(data = statT_noLip,
#'                aes(x = var, y = `MIS-C`),
#'                color = "dodgerblue") +
#'     geom_linerange(data = statT_noLip,
#'                    aes(ymin = minCovid, ymax = `Covid-19 (Children)`),
#'                    color = "firebrick3", size = 2) +
#'     geom_point(data = statT_noLip,
#'                aes(x = var, y = `Covid-19 (Children)`),
#'                color = "firebrick3") +
#'     labs(tag = "", caption = "*Fold Change is calculated with respect to controls") +
#'     ylab("log2(Fold Change)") +
#'     xlab("") +
#'     theme_bw() +
#'     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#'
#'
#'   fig3B <- ggplot(data = statT_noLip, aes(x = var)) +
#'     geom_hline(yintercept = 0, linetype="dashed",
#'                color = "black", size=1) +
#'     geom_hline(yintercept = c(-2, -1, 1, 2), linetype="dashed",
#'                color = "gray", size=1) +
#'     coord_flip() +
#'     geom_linerange(data = statT_noLip,
#'                    aes(ymin = minMisc, ymax = `MIS-C`),
#'                    color = "dodgerblue", size = 2) +
#'     geom_point(data = statT_noLip,
#'                aes(x = var, y = `MIS-C`),
#'                color = "dodgerblue") +
#'     geom_linerange(data = statT_noLip,
#'                    aes(ymin = minCovid, ymax = `Covid-19 (Children)`),
#'                    color = "firebrick3", size = 2) +
#'     geom_point(data = statT_noLip,
#'                aes(x = var, y = `Covid-19 (Children)`),
#'                color = "firebrick3") +
#'     labs(tag = "", caption = "*Fold Change is calculated with respect to controls") +
#'     ylab("log2(Fold Change)") +
#'     xlab("") +
#'     theme_bw() +
#'     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#' }
