#' Dendrogram
#'
#' Dendrogram of group separation of plotscores color assigned groups.
#'
#' @param ps An object from the plotScores function.
#' @param p number of principal components to include. 2 will include PC1 and PC2,
#' 3 will include PC1, PC2 and PC3.
#' @param breaks Number of longest edges to add breaks for easier visualization.
#' Default is 1. Providing 2 will add breaks the the top two longest edges.
#' @return A phylogenic tree with pvalues at each merge based on the number of
#' principal components provided. Each leaf will have the same name and coloring
#' as the plot score graph.
#' @examples
#'
#' pca <- PCA(data = iris[,1:4])
#' ps <- plotScores(model = pca,
#'                 optns = list(color = iris$Species,
#'                              ellipse = "color"))
#' dendrogram(ps = ps, p = 2, breaks = 2)
#'
#' @importFrom ape as.phylo plotBreakLongEdges
#' @import stats
#' @importFrom dendextend as.ggdend partition_leaves
#' @importFrom ggplot2 ggplot_build
#' @export


dendrogram <- function(ps, p = 2, breaks = 1){

  if(is(ps)[1] == "list"){
    df <- ps[["plots"]][["pcaGrid"]][["data"]]
    scorePlot <- ps[["plots"]][["pcaGrid"]][["plots"]][[1]]
    colorAssignment <- cbind(ggplot_build(scorePlot)$data[[3]]$colour,
                             df[,"assignment"])
    colorAssignment <- unique(colorAssignment)
  }

  if(is(ps)[1] == "opls"){
    df <- ps@suppLs[["ScoresPlot"]][["data"]]
    df <- df[sapply(names(df), function(x) is.numeric(df[[x]]) || x == "assignment")]
    scorePlot <- ps@suppLs[["ScoresPlot"]]
    colorAssignment <- cbind(ggplot_build(scorePlot)$data[[1]]$colour,
                             df[,"assignment"])

    colorAssignment <- unique(colorAssignment)
  }

  if(is(ps)[1] == "gg"){
    df <- ps[["data"]]
    scorePlot <- ps
    colorAssignment <- cbind(ggplot_build(scorePlot)$data[[1]]$colour,
                             df[,"assignment"])

    colorAssignment <- unique(colorAssignment)
  }

  ####make the dendogram####
  mahalanobis_dist_squared <- function(df){
    #make individual dfs for each group
    # Get unique species
    unique_assignment <- unique(df$assignment)

    # Initialize a list to store dataframes for each species
    assignment_dfs <- list()

    # Loop over unique species
    for (ass in unique_assignment) {
      # Subset dataframe for the current species
      subset_df <- df[df$assignment == ass, ]
      subset_df <- subset(subset_df, select = -c(assignment))
      # Store the subset dataframe in the list
      assignment_dfs[[ass]] <- subset_df[,1:p]
    }

    #Calculate Mean Vectors: Calculate the mean vectors for each group. (p variate sample means)
    means<- list()
    for(ass in unique_assignment){
      means[[ass]] <- colMeans(assignment_dfs[[ass]])
    }

    #Calculate Covariance Matrices: Calculate the covariance matrices for each group.
    covMatrix<- list()
    for(ass in unique_assignment){
      covMatrix[[ass]] <- cov(assignment_dfs[[ass]])
    }

    #calc Sp, the pooled p by p variance covariance matrix (a weighted average of the cov matrices from each group)
    # Calculate sample sizes
    n<- list()
    for(ass in unique_assignment){
      n[[ass]] <- nrow(assignment_dfs[[ass]])
    }

    # Compute mahalanobis_dist_squared
    mahalanobis_dist_squared <- matrix(NA, nrow = length(unique_assignment), ncol = length(unique_assignment))
    for(i in 1:length(unique_assignment)){
      for(j in 1:length(unique_assignment)){
        if(i > j){
          pooled_cov <- ((n[[i]] - 1) * covMatrix[[i]] + (n[[j]] - 1) * covMatrix[[j]]) / (n[[i]] + n[[j]] - 2)
          mean_diff <- means[[i]] - means[[j]]
          mahalanobis_dist_squared[i,j] <- t(mean_diff) %*% solve(pooled_cov) %*% mean_diff

        }
      }
    }

    #rename the matrix
    rownames(mahalanobis_dist_squared)<- unique_assignment
    colnames(mahalanobis_dist_squared)<- unique_assignment
    return(mahalanobis_dist_squared)
  }

  mds <- mahalanobis_dist_squared(df)
  dis <- as.dist(mds)
  hca <- hclust(dis, method = "average")

  ##get the merge groups for left and right####
  merge_df <- as.data.frame(cbind(hca$merge, hca$height), check.names = F)

  #make ggdend object
  dendo <- as.ggdend(as.dendrogram(hca))
  #change to list
  dendo[["nodes"]] <- lapply(dendo[["nodes"]], as.list)
  #extract what groups are in each edge
  subtrees <- partition_leaves(as.dendrogram(hca))
  #attach the groups info into the ggdend object
  dendo[["nodes"]][["subtrees"]] <- subtrees

  merge_list<-list()

  for(i in 1:nrow(merge_df)){
    #which nodes in the dendo match the height of the merges
    idx <- which(dendo[["nodes"]][["height"]] == merge_df[i,3])

    #all groups in the node
    all <- dendo[["nodes"]][["subtrees"]][[idx]]

    #left of the node
    left <- dendo[["nodes"]][["subtrees"]][[idx+1]]

    #right of the node
    right <- setdiff(all, left)

    merge_list[["left"]][[i]]<-left
    merge_list[["right"]][[i]]<-right
  }


  #####pvalues####
  calculate_p_value <- function(mds, leftN, rightN, p) {
    degFree <- leftN + rightN + p - 1
    total_parameters <- p * (leftN + rightN - 2)
    T2 <- (leftN * rightN) / (leftN + rightN) * mds
    xf <- degFree / total_parameters * T2
    p_value <- pf(xf, p, leftN + rightN - p - 1, lower.tail = FALSE)
    return(p_value)
  }

  pval<-list()
  for(i in 1:nrow(merge_df)){

    #make df for groups left and right of a node
    leftdf <- df[df[["assignment"]] %in% merge_list[["left"]][[i]], ]
    rightdf <- df[df[["assignment"]] %in% merge_list[["right"]][[i]], ]

    #calc means
    leftMean<- colMeans(leftdf[,1:p])
    rightMean<- colMeans(rightdf[,1:p])

    #Calculate Covariance Matrices: Calculate the covariance matrices for each group.
    leftCovMatrix <- cov(leftdf[,1:p])
    rightCovMatrix <- cov(rightdf[,1:p])

    #calc Sp, the pooled p by p variance covariance matrix (a weighted average of the cov matrices from each group)
    # Calculate sample sizes
    leftN <- nrow(leftdf)
    rightN <- nrow(rightdf)

    # Compute mahalanobis_dist_squared
    pooled_cov<- ((leftN - 1) * leftCovMatrix + (rightN - 1) * rightCovMatrix) / (leftN + rightN - 2)
    mean_diff <- leftMean - rightMean
    mds <- t(mean_diff) %*% solve(pooled_cov) %*% mean_diff
    pval[[i]]<- calculate_p_value(mds, leftN, rightN, p = p)
  }

  #store pvalues in he hclust object
  hca[["pval"]] <- unlist(pval)

  ####plot####
  phylo <- as.phylo(hca)
  pvals <- as.numeric(format(hca[["pval"]],
                             scientific = TRUE,
                             digits = 3))
  phylo$node.label <- pvals

  #assign the same colours as the plotscores
  geo <- factor(colorAssignment[,2])
  mycol <- colorAssignment[,1][geo]

  # ggtree_obj <- ggtree(phylo) +
  #               geom_tiplab(align = TRUE,
  #                           color = mycol) +
  #               geom_nodelab(nudge_x = 1)

  # # dendroPlot <- plot.phylo(phylo,
  # #                    tip.color = mycol,
  # #                    show.node.label = TRUE,
  # #                    cex = c(0.8))
  # #
  # test <- ()

  # t<-plot_tree(physeq = phylo,
  #              label.tips = "taxa_names",
  #              color = "taxa_names")
  #
  # t<-plot_tree(plot.phylo(phylo,
  #                                     tip.color = mycol,
  #                                        show.node.label = TRUE,
  #                                        cex = c(0.8)))
  #
  # t <- as.grob(plotBreakLongEdges(phylo,
  #                            tip.color = mycol,
  #                            show.node.label = TRUE,
  #                            cex = c(0.8),
  #                            n = breaks))

  return(plotBreakLongEdges(phylo,
                            tip.color = mycol,
                            show.node.label = TRUE,
                            cex = c(0.8),
                            n = breaks))

   # return(ggtree_obj)



}

####end####


# #######euclidean#######
# num_bootstraps <- 100
#
# # 3. Generate bootstrapped datasets for control and treatment groups
# bootstrapped_datasets_control <- lapply(1:num_bootstraps, function(i) {
#   sample_indices <- sample(1:nrow(control[,1:2]), replace = TRUE)
#   bootstrapped_data <- control[sample_indices, 1:2]
# })
#
# bootstrapped_datasets_treatment <- lapply(1:num_bootstraps, function(i) {
#   sample_indices <- sample(1:nrow(treatment[,1:2]), replace = TRUE)
#   bootstrapped_data <- treatment[sample_indices, 1:2]
# })
#
# # 4. Calculate Euclidean distances for each bootstrapped dataset within each group
# euclidean_distances_control <- lapply(bootstrapped_datasets_control, function(bootstrapped_data) {
#   group_means_boot <- colMeans(bootstrapped_data)
#   dist(group_means_boot)
# })
#
# euclidean_distances_treatment <- lapply(bootstrapped_datasets_treatment, function(bootstrapped_data) {
#   group_means_boot <- colMeans(bootstrapped_data)
#   dist(group_means_boot)
# })
#
# # 5. Generate trees using UPGMA algorithm for control and treatment groups
# trees_control <- lapply(euclidean_distances_control, function(distances) {
#   # Perform UPGMA clustering
#   tree <- hclust(distances, method = "average")
# })
#
# trees_treatment <- lapply(euclidean_distances_treatment, function(distances) {
#   # Perform UPGMA clustering
#   tree <- hclust(distances, method = "average")
# })
#
# # 6. Calculate bootstrap probabilities for each tree for control and treatment groups
# bootstrap_probabilities_control <- lapply(trees_control, function(tree) {
#   # Calculate bootstrap probabilities using control group
#   bootstrap_probabilities <- pvclust(tree, data = as.dist(control[,1:2]), method.dist = "euclidean", method.hclust = "average")
# })
#
# bootstrap_probabilities_treatment <- lapply(trees_treatment, function(tree) {
#   # Calculate bootstrap probabilities using treatment group
#   bootstrap_probabilities <- pvclust(tree, data = treatment[,1:2], method.dist = "euclidean")
# })
#
#
# bootstrap_probabilities_control <- lapply(trees_control, function(tree) {
#   tryCatch({
#     # Bootstrap resampling
#     boot_indices <- sample(1:nrow(control), replace = TRUE)
#     boot_data <- control[boot_indices, 1:2]  # Assuming the PCA scores are in the first two columns
#
#     # Calculate hierarchical clustering
#     boot_tree <- hclust(dist(boot_data), method = "average")
#
#     # Calculate bootstrap probabilities using pvclust
#     bootstrap_probabilities <- pvclust(boot_tree, data = as.dist(boot_data), method.dist = "euclidean", method.hclust = "average")
#   },
#   error = function(e) {
#     return(NULL)  # Return NULL if an error occurs
#   })
# })
#
# dendro <- as.dendrogram(hca)
# plot(dendro)
#

#
# # Define a function to add p-values as labels to the dendrogram
# add_p_values <- function(dendro, p_values) {
#   for (i in 1:length(p_values)) {
#     if (!is.na(p_values[i])) {
#       dendro[[i]] <- paste0(dendro[[i]], "\n(p-value = ", round(p_values[i], digits = 4), ")")
#     }
#   }
#   dendro
# }
#
# # Replace NA p-values with empty strings
# p_values <- ifelse(is.na(attr(hca, "p_values")), "", attr(hca, "p_values"))
#
# # Add p-values as labels to the dendrogram
# dendro_with_p_values <- add_p_values(dendro, p_values)
#
# # Plot the modified dendrogram with p-values
# plot(dendro_with_p_values)
#
#
# tree_data <- dendro_data(hca)
#
# pvals <- as.numeric(format(hca[["pval"]], scientific = TRUE, digits = 3))
#
# # Create a vector to store p-values interleaved with NA values
# pvals_with_na <- numeric(length(pvals) * 4 - 3)
#
# # Insert p-values at every fourth position
# pvals_with_na[seq(1, length(pvals_with_na), by = 4)] <- pvals
# pvals_with_na[pvals_with_na == 0] <- NA
# pvals_with_na <- append(pvals_with_na, rep(NA, 3))
# #pvals_with_na <- format(pvals_with_na, scientific = TRUE, digits = 3)
# tree_data[["segments"]][["test"]] <- (pvals_with_na)
#
# ggplot(segment(tree_data)) +
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = .1,
#                colour = "black", alpha = 1) +
#   scale_size("n") +
#   geom_text(data = label(tree_data),
#             aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
#   geom_text(data = segment(tree_data),
#             aes(x = x, y = y, label = test), vjust = 0.5, size = 3) +
#   theme_dendro()
#
# df <-ps[["plots"]][["pcaGrid"]][["data"]]

