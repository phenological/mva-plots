#' Plot Scores.
#'
#' Permutation plot for oplsda (ropls) models.
#'
#' @param model An oplsda (ropls) model.
#' @param optns An empty list for aesthetic options.
#' @param plotTitle A parameter for the \code{optns} list. A character for the title of the plot.
#' @param colorQ A parameter for the \code{optns} list. A character of the color for Q2 (example "pink" or "#FF5500"). Default color is "red".
#' @param colorR A parameter for the \code{optns} list. A character of the color for R2Y (example "blue" or "#FF5500"). Default color is "blue".
#' @param shape A parameter for the \code{optns} list. A character (example "circle") or the number (example 21) of the shape desired . Default shape is "circle".
#' @param size A parameter for the \code{optns} list. A numeric of the size desired. Default size is 3.
#' @param alpha A parameter for the \code{optns} list. A numeric of the alpha desired for the permuted results. Default size is 0.4, actual values are always opaque.
#' @return Permutation plot of Q2 and R2Y. The caption denoted pQ2 and pR2Y which indicates how significantly different the real results are from the permuted. Example, pQ2=(1+n(permutedQ2>realQ2))/n(permutations)
#' @examples
#' #data(mtcars)
#' #a <- oplsda(X = mtcars[,1:7], Y = mtcars[,8], type = "OPLS", optns = list(permI = 50))
#' #permutationPlot(model = a, optns=list(shape="square", colorQ = "#FF5500", colorR = "pink", size = 5))
#'
permutationPlot <- function(model, optns = list()){
df <- as.data.frame(model@suppLs[["permMN"]])

#plot title
if("plotTitle" %in% names(optns)){
  plotTitle = optns$plotTitle
}else{
  plotTitle <- "Permutation Plot"
}

#color
if(!("colorQ" %in% names(optns)))
{optns$colorQ <- "red"}

if(!("colorR" %in% names(optns)))
{optns$colorR <- "blue"}

#alpha
if(!("alpha" %in% names(optns)))
{optns$alpha <- 0.4}

#shape
if(!("shape" %in% names(optns)))
{optns$shape <- "circle"}

#size
if(!("size" %in% names(optns)))
{optns$size <- 3}

 ggplot(data = df)

 colors <- c("Q2(cum)" = optns$colorQ, "R2Y(cum)" = optns$colorR)

 permPlot <-
   ggplot(df, aes(x = sim)) +
   geom_point(data = df[1,],aes(y = `Q2(cum)`, color = "Q2(cum)"),
              shape = optns$shape,
              size = optns$size) +
   geom_point(data = df[1,],aes(y = `R2Y(cum)`, color = "R2Y(cum)"),
              shape = optns$shape,
              size = optns$size) +
   geom_point(data = df[-1,],aes(y = `Q2(cum)`, color = "Q2(cum)"),
              alpha = optns$alpha,
              shape = optns$shape,
              size = optns$size) +
   geom_point(data = df[-1,],aes(y = `R2Y(cum)`, color = "R2Y(cum)"),
              alpha = optns$alpha,
              shape = optns$shape,
              size = optns$size) +
   geom_hline(yintercept = df[1, "Q2(cum)"], colour = optns$colorQ) +
   geom_hline(yintercept = df[1, "R2Y(cum)"], colour = optns$colorR) +
   theme_bw()+
   labs(x = expression(Similarity(bold(y), bold(y[perm]))),
        y = "Value",
        caption = paste0("pR2Y = ", model@summaryDF[, "pR2Y"],", pQ2 = ", model@summaryDF[, "pQ2"])) +
   scale_color_manual(values = colors) +
   theme(legend.title=element_blank(),
         legend.position = c(0.9, 0.1),
         legend.justification = c("right", "bottom")) +
   ggtitle(plotTitle)

 print(permPlot)
}
