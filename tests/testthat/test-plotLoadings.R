#plotLoadings
test_that("plotLoadingGrid is a gg object",{
  data("iris")
  a <- PCA(data = iris[,1:4])
  p <- plotLoadings(model = a, optns = list())
  expect_s3_class(object= p[["plots"]][["plotLoadingGrid"]], class = "gg")
})


test_that("opls object is handled correctly", {
  ##opls object
  exampleData <- mtcars

  colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")

  exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
  exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
  rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))

  model <- oplsda(Y = (exampleData[, "status"]),
                  X = exampleData[,1:5],
                  type = "OPLS")

  #make plotloadings for oplsda model with ellipse and outliers included

  pl<- plotLoadings(model = model)

  #is the plot appended to the model
  expect_true("LoadingsPlot" %in% names(pl@suppLs), "LoadingsPlot should exist in pl@suppLs")


})

# exampleData <- mtcars
#
# colnames(exampleData) <- c("metab1", "metab2", "metab3", "metab4", "metab5", "metab6", "age", "status", "sex")
#
# exampleData$status <- ifelse(exampleData$status == 1, "treatment", "control")
# exampleData$sex <- ifelse(exampleData$sex == 1, "male", "female")
# rownames(exampleData) <- paste0("subject", 1:nrow(exampleData))
#
# model <- oplsda(Y = (exampleData[, "status"]),
#                 X = exampleData[,1:5],
#                 type = "OPLS")
#
# plotLoadings(model = model)
#
# #want flat plotloadings and plotscores for OPLS, since you don't want the orthogonal component graphed.
# #O-PLS-DA currently only handles 2 groups in Y so would allow positions of each group at 1 and 2 on y axis (for example) for the plotscores. doesn't matter for the loadings.
# #O-PLS driven by a continuous Y, perhaps don't need to position them separately via "group" still have cont colour just appear along the same line.
# plotLoadings <- function(model, optns=list()){
#   #plot title (working)
#   if("plotTitle" %in% names(optns)){
#     plotTitle = optns$plotTitle
#   }else{
#     plotTitle <- "Loadings Plot"
#   }
#
#   #theme
#   if(!("theme" %in% names(optns))){
#     theme <- theme()
#   } else{theme <- optns$theme}
#
#   #########ropls objects##################
#   if(is(model)[1]=="opls"){
#
#     if(grepl("O", model@typeC) == TRUE){
#       df <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN))
#       gl <- labs(x = paste0('p1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
#                  y = paste0('po1'))
#     }else{
#       df <- as.data.frame(model@loadingMN)
#       gl <- labs(x = paste0('p1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
#                  y = paste0('p2 (', round(model@modelDF[["R2X"]][2]*100, 1), '%)'))
#     }
#
#     PCi <- 1
#     PCj <- 2
#
#     onePlot <- ggplot(data = df,
#                       aes(x = df[,PCi], y = df[,PCj])) +
#       ggtitle(plotTitle) +
#       gl +
#       geom_point(color= "blue",
#                  size = 1) +
#       geom_text_repel(aes(label = rownames(df)),
#                       size = 3.5) +
#       geom_hline(yintercept = 0, colour = "gray70") +
#       geom_vline(xintercept = 0, colour = "gray70") +
#       theme_bw() +
#       theme
#
#     print(onePlot)
#
#     model@suppLs[["LoadingsPlot"]] <- onePlot
#     return(model)
#   }
#
#   #########PCA objects##################
#   if(is(model)[1]=="list"){
#
#     df<- as.data.frame(model$data$loadings)
#
#     #number of pcas (working)
#     if("thresh" %in% names(optns)){
#       thresh = optns$thresh[1]
#     }else{thresh = model$data$threshold}
#
#     #Loop for creating titles of "PC(explained variance %)"
#     title <- list()
#     for (i in 1:thresh) {
#       title[[i]] <- paste0('PC', i, ' (', round(model$data$pcSum$`Proportion of Variance`[i], 1), '%)')
#     }
#     title<-unlist(title)
#
#     if("PCi" %in% names(optns)){
#       PCi <- optns$PCi
#       PCj <- optns$PCj
#       gl <- labs(x = title[PCi], y = title[PCj])
#
#       onePlot <- ggplot(data = df,
#                         aes(x = df[,PCi], y = df[,PCj])) +
#         ggtitle(plotTitle) +
#         gl +
#         geom_point(color= "blue",
#                    size = 1) +
#         geom_text_repel(aes(label = rownames(df)),
#                         size = 3.5) +
#         geom_hline(yintercept = 0, colour = "gray70") +
#         geom_vline(xintercept = 0, colour = "gray70") +
#         theme_bw() +
#         theme
#
#       return(onePlot)
#     }
#
#     if(!("PCi" %in% names(optns))){
#       plotLoadingGrid <- ggpairs(data = df[,1:thresh],
#                                  columnLabels = c(title),
#                                  title = plotTitle,
#                                  diag="blank",
#                                  upper="blank",
#                                  #upper=list(continuous =my_fn1),
#                                  lower=list(continuous =myFn2),
#                                  progress = F,
#                                  switch="both") +
#         geom_point(color= "blue",
#                    size = 1) +
#         geom_text_repel(aes(label = rownames(df)),
#                         size = 3.5) +
#         theme_bw() +
#         theme(strip.background = element_rect(fill = "white"),
#               axis.text.x = (element_text(size = rel(0.7),
#                                           angle = 0)),
#               axis.text.y = (element_text(size = rel(0.7),
#                                           angle = 0)),
#               panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.border = element_rect(fill = NA,
#                                           colour = "grey35")) +
#         theme
#
#       plotLoadingGrid <- gPairsLower(plotLoadingGrid)
#
#       model$plots <- append(model$plots, list(plotLoadingGrid = plotLoadingGrid))
#
#       print(plotLoadingGrid)
#       invisible(model)
#     }
#   }
#
# }

