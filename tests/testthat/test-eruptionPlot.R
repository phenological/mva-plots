
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

# ########adjusted p-value########
# model <- as.data.frame(new_lipidData)
# df <- as.data.frame(new_lipidData)
#
# optns <- list(factor = new_lipidMetadata$sample_batch,
#               control = "Non-burn",
#               color = "pval",
#               method = "none")
# # #empty df
# pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
# pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
#
# df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
# unique_factors <- unique(df[,"factor"])
#
# for(j in 2: length(unique_factors)){
#
#   df2 <- df[df[,"factor"] %in% c(1, j), ]
#
#   pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
#   for(i in 1:(ncol(df2)-1)){
#     pval[i,]<-kruskal.test(df2[,i], df2[,"factor"])$p.value
#   }
#
#   pvalUnadjusted[,j] <- pval
#
#   #rescaled and adjusted p-value
#   for(i in 1:(ncol(df2)-1)){
#     pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
#   }
# }
#
# # Create a mapping between numbers and words, rename pval dataframe columns
# mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))
#
# testnames<- as.data.frame(mapping)
# testnames$rowName <- rownames(testnames)
#
# for (i in seq_len(nrow(testnames))) {
#   col_number <- as.numeric(testnames[i, 2])
#   new_col_name <- as.character(testnames[i, 1])
#   names(pvalUnadjusted)[col_number] <- new_col_name
#   names(pvalRescaled)[col_number] <- new_col_name
# }
#
# pvalExternal <- pval
#
# #####test#######
# pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)
#
# eruptionPlot(model = pca,
#              optns = list(factor = new_lipidMetadata$sample_batch,
#                           method = "none",
#                           color = "pval",
#                           control = "Non-burn"
#                           ))
#
# eruptionPlot(model = pca,
#              optns = list(factor = new_lipidMetadata$sample_batch,
#                           method = "fdr",
#                           color = "pval"
#              ))
#
# model <- pca
# optns <- list(factor = (new_lipidMetadata$sample_batch),
#               control = "Non-burn",
#               color = "pval",
#               method = "none")
#
#
# eruptionPlot <- function(model, optns = list()){
#
# id <- colnames(model)
# df <- model
#
# if (!"control" %in% names(optns)) {
#   optns$control <- 1
#   #print warning
#   warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
# }
#
# if("plotTitle" %in% names(optns)){
#   plotTitle = optns$plotTitle
# }else{
#   plotTitle <- "Eruption Plot"
# }
#
# #palettes
# if(!("continuousPalette" %in% names(optns))){
#   optns$continuousPalette <- c(
#     "#0000CC",
#     "#0000FF",
#     "#0055FF",
#     "#00AAFF",
#     "#00FFFF",
#     "#2BFFD5",
#     "#55FFAA",
#     "#80FF80",
#     "#AAFF55",
#     "#D4FF2B",
#     "#FFFF00",
#     "#FFAA00",
#     "#FF5500",
#     "#FF0000",
#     "#CC0000")
# }
#
# ###for pvalue adjustment, options the same as listed in stats::p.adjust c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
# if("method" %in% names(optns)){
#   method = optns$method
# }else{
#   method <- "bonferroni"
# }
#
# #choose which principal component loadings to plot (default PC1) and which PC scores to calculate correlation with
# if("PC" %in% names(optns)){
#   PC <- optns$PC
# }else{
#   PC <- 1
# }
#
# if(is(model)[1] == "list"){
#   id <- as.data.frame(colnames(model$data$rawData))
#   df <- model$data$rawData
#   df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
#   df <- as.data.frame(df)
#   pcLoadings<-as.data.frame(abs(model$data$loadings[,PC]))
#
# }
#
# if(is(model)[1] == "opls"){
#   id <- as.data.frame(colnames(as.data.frame(model@suppLs[["x"]])))
#   df <- as.data.frame(model@suppLs[["x"]])
#   df[,"factor"] <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))
#   pcLoadings<-as.data.frame(abs(model@loadingMN[,PC]))
# }
#
# #stop if there are more than 2 groups in factor
# if(length(unique(df[,"factor"])) > 2){
#   stop("Error: You have more than 2 levels in your factor")
# }
#
# #ensure "factor" isn't included in id
# if(any(id == "factor")){
#   idx <- which(id == "factor")
#   id <- as.data.frame(id[-idx,])
# }
#
# ########cliffs delta##########
# cd <- cliffsDelta(model = model, optns = optns)
#
# ##########correlations between scaled + centered original data and scores######
# if(is(model)[1]== "list"){
#   corr <- abs(t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC))))
# }
#
# if(is(model)[1]=="opls"){
#   corr <- abs(t(as.data.frame(cor(model@scoreMN[,PC], model@suppLs[["xModelMN"]]))))
# }
#
# ##########Fold change#########
# fc <- foldChange(model = model, optns = optns)
#
# ########p-value########
#
# if("external" %in% names(optns)){
#   pval <- unlist(optns$external)
#   pvalUnadjusted <- (as.data.frame(pval))
# } else{
#   pval<-list()
#   for(i in 1:(ncol(df)-1)){
#     pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
#   }
#   unlist(pval)
#   pvalUnadjusted <- t(as.data.frame(pval))
# }
#
# ########p-value adjustment########
# #keep for external provided p-value
#
# pvalAdjusted <- p.adjust((pval), method = method)
# pvalRescaled <- abs(log10(pvalAdjusted))
# pvalRescaled <- as.data.frame(pvalRescaled)
#
# #eruption data frame
# ed<-cbind(cd, fc, pvalRescaled, pvalUnadjusted, pcLoadings, id, corr)
# if(length(which(is.infinite(ed$pval)))>0){
#   idx <- which(is.infinite(ed$pval))
#   ed[idx, "pval"] <- NA
# }
#
# colnames(ed)<-c("cd", "fc", "pval", "pvalRaw", "loadings", "id", "corr")
#
# #for graph labs
# labels<-list("Cliff's Delta", "Log2FC", "|log10pval|", "|loadings|", "id", "|corr|")
# names(labels)<-c("cd", "fc", "pval", "loadings", "id", "corr")
#
# if("color" %in% names(optns)) {
#   color <- ed[, optns$color]
#
# } else{
#   color <- ed$corr
#   optns$color <- "corr"
# }
#
# if (optns$color == "pval") {
#   color_breaks <- c(0, 1.3, max(na.omit(ed[,"pval"])))
#   values <- rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, max(na.omit(ed[,"pval"]))))
#   limits <- c(0, max(na.omit(ed[,"pval"])))
# }else{
#   color_breaks <- c(0, 0.5, 1)
#   values <- NULL
#   limits <- c(0, 1)
# }
#
# if("x" %in% names(optns)){
#   x <- ed[,optns$x]
# }else{x <- ed$cd
# optns$x <-"cd"}
#
# if("y" %in% names(optns)){
#   y <- ed[,optns$y]
# }else{y <- ed$loadings
# optns$y <- "loadings"}
#
# ############plot#########
# eruptionPlot <- ggplot(data = ed, aes(x = x,
#                                       y = y,
#                                       color = color)) +
#   labs(x = labels[optns$x],
#        y = labels[optns$y],
#        color = labels[optns$color]) +
#   geom_label_repel(aes(label = id),
#                    colour = "black",
#                    min.segment.length = 0.001) +
#   geom_point(size = 3,
#              shape = 16,
#              alpha = 0.3) +
#   theme_bw() +
#   scale_x_continuous(limits = c(-1, 1)) +
#   scale_color_gradientn(
#     values = values,
#     colours = optns$continuousPalette,  # Use the custom color palette,
#     breaks = color_breaks,  # Specify the breaks
#     labels = number_format(accuracy = 0.01), #2 decimal points on colourbar scale
#     limits = limits,
#     trans = "identity",
#     na.value = "grey50",
#     guide = "colourbar"
#   ) +
#   ggtitle(plotTitle) +
#   theme(panel.grid.minor = element_blank(),
#         plot.tag = element_text(face = "bold",
#                                 size = 25),
#         legend.position = "none",
#         legend.direction = "vertical")
#
# ########p-value legend##########
# #makes a separate plot with evenly spread colourbar
#
# if("color" %in% names(optns)){
#
#   if(optns$color == "pval"){
#
#     if(length(which(is.infinite(ed$pval))) > 0){
#       idx <- which(is.infinite(ed$pval))
#       ed[idx, "pval"] <- NA
#     }
#
#     ma<- round(max(na.omit(ed$pval)),1)
#     # color_breaks <- c(0, 1.3, ma)
#     # values <- rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, ma))
#     # limits <- c(0, ma)
#
#     data <- as.data.frame(matrix(rnorm(100), ncol = 5))
#     data$col<- 1:20
#     plot1 <- ggplot(data = data,
#                     aes(x = V1,
#                         y = V2,
#                         color = col)) +
#       geom_point() +
#       scale_color_gradientn(
#         colours = optns$continuousPalette,  # Use the custom color palette,
#         breaks = c(1,15, 20),  # Specify the breaks
#         labels=c("0", "1.3", ma),
#         limits = c(1,20),
#         na.value = "grey50",
#         guide = "colourbar"
#       ) +
#       labs(color = "|log10pval|") +
#       theme
#
#     leg_grob <- grab_legend(plot1)
#
#     # Combine eruptionPlot and leg_grob using grid.arrange
#     combined_plot <- grid.arrange(eruptionPlot, legend = leg_grob, ncol = 2 )
#     #leg <- get_legend(plot1)
#
#     # combined_plot <- grid.arrange(eruptionPlot, legend = leg, ncol = 2)
#     #
#     #
#     # combined_plot <- arrangeGrob(
#     #   grobs = list(eruptionPlot, leg),
#     #   ncol = 1,
#     #   heights = c(3, 1)  # Adjust the height ratio as needed
#     # )
#
#     eruptionPlot<- eruptionPlot +
#       labs( caption = paste0("p-value adjustment method: ", method) )
#
#     # eruptionPlot<-ggarrange(plotlist = list(eruptionPlot),
#     #                         legend.grob = leg,
#     #                        legend = "right")
#
#     # eruptionPlot <- ggarrange(plots = list(eruptionPlot),
#     #                           legend.grob = (get_legend(plot1)),
#     #                           legend = "right")
#
#   }
#
# }
#
# print(eruptionPlot)
# #########append#############
#
# #append to data and plots
# if(is(model)[1] == "list"){
#   model$plots <- append(model$plots, list(eruptionPlot = eruptionPlot))
#   model$data <- append(model$data, list(eruptionData = ed))
# }
#
# if(is(model)[1] == "opls"){
#   model@suppLs[["eruptionData"]] <- append(x = data.frame(), values = ed)
#   model@suppLs[["eruptionPlot"]] <- eruptionPlot
# }
# invisible(model)
# }
#
#
#
# data <- as.data.frame(matrix(rnorm(100), ncol = 5))
# data$col<- 1:20
# plot1 <- ggplot(data = data,
#                 aes(x = V1,
#                     y = V2,
#                     color = col)) +
#   geom_point() +
#   scale_color_gradientn(
#     colours = optns$continuousPalette,  # Use the custom color palette,
#     breaks = c(1,15, 20),  # Specify the breaks
#     labels=c("0", "1.3", ma),
#     limits = c(1,20),
#     na.value = "grey50",
#     guide = "colourbar"
#   ) +
#   labs(color = "|log10pval|")
#
#
# leg_grob <- grab_legend(plot1)
#
# # Combine eruptionPlot and leg_grob using grid.arrange
# combined_plot <- grid.arrange(eruptionPlot, legend = leg_grob, ncol = 1)
#
# leg <- get_legend(plot1)
