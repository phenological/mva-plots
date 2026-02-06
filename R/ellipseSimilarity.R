#' Ellipse Similarity
#'
#' Either the Jaccard index or overlap coefficient. The closer the result is to
#' one, the more overlap there is between the ellipses. For more than two ellipses,
#' there will be a pairwise comparison.
#'
#' @param ps an object containing (or is) a plotScores graph. Can be an oplsda
#' object or a PCA object (singular and grid both accepted).
#' @param type a character, either "jaccard" for the jaccard index or "coefficient"
#' for the overlap coefficient. The default is "jaccard". The jaccard index is the
#' size of the intersection of set A and set B (i.e. the number of common elements)
#' over the size of the union of set A and set B. The overlap coefficient (or
#' Szymkiewicz–Simpson coefficient), is the size of the intersection of set A and
#' set B over the size of the smaller set between A and B. This function also
#' provides case the intersection over the size of the larger set. The overlap
#' coefficient is often proffered when the sizes of the sets differ.
#' @return a list with ellipse similarity for each graph
#' @examples
#' # oplsda object
#' data(mtcars)
#' pls <- oplsda(X = mtcars[,1:11],
#'               Y = as.factor(mtcars$vs),
#'               type = "OPLS",
#'               optns = list())
#' plsps <- plotScores(model = pls,
#'                     optns = list(ellipse = "color"))
#' result <- ellipseSimilarity(ps = plsps, type = "jaccard")
#'
#' #PCA object (grid)
#' pca <- PCA(data = mtcars[,1:7],
#'           rank = 3,
#'           plot = FALSE)
#' psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$vs),
#'                                            ellipse ="color"))
#' ellipseSimilarity(ps = psg, type = "jaccard")
#'
#'
#' @import sf
#' @import ggplot2
#' @export

#importFrom sf
ellipseSimilarity<- function(ps, type = "jaccard"){
  # points representing two ellipses

  #PCA
  if(is(ps)[1] == "list"){
    if ("plots" %in% names(ps)){
      if ("pcaGrid" %in% names(ps[["plots"]])){
        scplots<- ps[["plots"]][["pcaGrid"]][["plots"]]
      } else{
        if ("pcaSingle" %in% names(ps[["plots"]])){
          scplots <- list()
          scplots[[1]] <- ps[["plots"]][["pcaSingle"]]
        } else{
          stop("No scores plot found in argument ps")
        }
      }
    } else{
      stop("No plots found in argument ps")
    }
  } else{
    #OPLSDA
    if(is(ps)[1] == "opls"){
      scplots <- list()
      if ("ScoresPlot" %in% names(ps@suppLs)){
        scplots[[1]] <- ps@suppLs[["ScoresPlot"]]
      } else{
        stop("No scores plot found in argument ps")
      }
    } else{
      #Neither
      stop("Argument ps is not a valid model")
    }
  }

  # #single PCA scores plot
  # if(is(ps)[1] == "gg"){
  #   plot <- list()
  #   plot[[1]] <- ps
  # }

  test <- list()

  for (k in 1:length(scplots)){
    #go through each plot, if class(scplots) contains "ggmatrix_blank", exclude it from the process
    if (!("ggmatrix_blank" %in% class(scplots[[k]]))) {
      # Ignore it or do nothing
      #ratio <- NULL
    # } else {
      x <- ggplot_build(scplots[[k]])[["plot"]][["labels"]][["x"]]
      y <- ggplot_build(scplots[[k]])[["plot"]][["labels"]][["y"]]
      df <- ggplot_build(scplots[[k]])$data[[4]]
      df$colour <- as.factor(df$colour)

      unique_factors <- unique(df$colour)

      #make ellipses into polygon objects (z), do so for each ellipse of different colors
      z<- list()
      for(i in 1:length(unique_factors)){
        temp <- df[which(df$colour == unique_factors[[i]]), c("x","y")]

        # Add the first point to the end to close the polygon
        if (!identical(temp[1, ], temp[nrow(temp), ])) {
          temp <- rbind(temp, temp[1, ])
        }

        z[[i]] <- st_polygon(list(as.matrix(temp)))
      }

      #create an empty matrix with the size depending on the number of ellipses present
      if(length(unique_factors)>2){
        n <- length(unique_factors)
        ratio <- matrix(NA, nrow = n, ncol = n)
        # n <- length(unique_factors) -1
        # m <- (n * (n + 1) / 2)
        # ratio <- matrix(NA, nrow = m, ncol = m)
      } else{ratio <- matrix(NA, nrow = 2, ncol = 2)

      }

      colnames(ratio) <- 1:length(unique_factors)
      rownames(ratio) <- 1:length(unique_factors)
      # Extract color and group information from the data

      #change depending on type
      #PCA
      if(is(ps)[1] == "list"){
        #No need to check again for "plots" existence
        if ("pcaGrid" %in% names(ps[["plots"]])){
          color_info <- ggplot_build(scplots[[1]])$data[[3]]
          color_info$assignment <- ps[["plots"]][["pcaGrid"]][["data"]][["assignment"]]
        } else{
          #No need to check again for "pcaSingle" existence
          color_info <- ggplot_build(scplots[[1]])$data[[1]]
          color_info$assignment <- scplots[[1]][["data"]][["assignment"]]
        }
      } else{
        #No need to check again for opls object
        color_info <- ggplot_build(scplots[[1]])$data[[1]]
        color_info$assignment <- scplots[[1]][["data"]][["y1"]]
      }

      # #single PCA scores plot
      # if(is(ps)[1] == "gg"){
      #   color_info <- ggplot_build(scplots[[1]])$data[[1]]
      #   color_info$assignment <- scplots[[1]][["data"]][["assignment"]]
      # }

      

      dup <- function(color_info) duplicated(cbind(pmin(color_info[,"colour"])))
      color_info <- (color_info[!dup(color_info),])

      for(n in 1:length(unique_factors)) {
        # Find the corresponding value in the 'group' column of the dataframe
        idx <- which(color_info$group == n)
        idy <- color_info[idx, "assignment"]
        rownames(ratio)[n] <- as.character(idy)
        colnames(ratio)[n] <- as.character(idy)
      }

      #calculate the relevant Ratios using the polygon objects (z)
      for (i in 1:length(unique_factors)) {
        for (j in 1:length(unique_factors)) {
          #jaccard
          if(type == "jaccard"){
            if (j > i) {
              overlapArea <- st_area(st_intersection(z[[i]], z[[j]]))
              # if (length(unique_factors) > 2) {
                ratio[i, j] <- overlapArea / (st_area(z[[i]]) + st_area(z[[j]]) - overlapArea)
              # }
              # else {
              #   ratio[i] <- overlapArea / (st_area(z[[i]]) + st_area(z[[j]]) - overlapArea)
              # }
            }
          }

          #coefficient
          if(type == "coefficient"){
            overlapArea <- st_area(st_intersection(z[[i]], z[[j]]))
            # if (length(unique_factors) > 2) {
              ratio[i, j] <- overlapArea / (st_area(z[[i]]))
              ratio[j, i] <- overlapArea / (st_area(z[[j]]))
            # }
            # else {
            #   ratio[i,j] <- overlapArea / (st_area(z[[i]]) )
            #   ratio[j,i] <- overlapArea / (st_area(z[[j]]) )
            # }
          }

        }
      }
    }

    #store the Jaccard Ratios for each plot and label which plot they are for

    placeHolder <- paste0(x, " vs ", y)
    test[[k]] <- ratio
    names(test)[[k]]<- placeHolder

  }

  unique_names<- unique(names(test))
  unique_test <- lapply(unique(test), function(x) as.data.frame(unique(x)))
  names(unique_test) <- unique_names

  return(unique_test)

}

# #1 smaller
# st_area(s12intersect)/(st_area(s2))
#
# #2 total
# st_area(s12intersect)/(st_area(s1) + st_area(s2))
#
# #3 larger
# st_area(s12intersect)/st_area(s1)
#
# #4 Jaccard Ratio
# st_area(s12intersect)/(st_area(s1) + st_area(s2)-st_area(s12intersect))
#
#
# #5 Dice coefficient
#
# (2*st_area(s12intersect))/(st_area(s1) + st_area(s2))

# install.packages("ade4")
# library(ade4)
# m <- read.table("./similarity/Data_pres&abs.tab", header=TRUE)
# m1 <- read.table("./Similarity/Data_pres&abs_no_head.tab", header=F)
# d <- dist.binary(m1, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
# hc <- hclust(d)               # apply hierarchical clustering
# plot(hc, labels=m$ID)    # plot the dendrogram
