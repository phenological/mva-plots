#' Fold Change

foldChange  <- function(model, optns = optns){

  if(is(model)[1] == "list"){
    model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
    df <- model$data$rawData
  }

  if(is(model)[1]=="opls"){
    df <- as.data.frame(model@suppLs[["x"]])
    df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
  }

  idx<- which(df[,"factor"] == 1)
  control <- df[idx,]
  treatment <- df[-idx,]

  #logmeans
  c <- log2(apply(X = control, MARGIN = 2, FUN = mean))
  t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))

  #log2 fold change
  log2fc <- as.data.frame(c-t)
  log2fc <- as.data.frame(log2fc[1:(nrow(log2fc)-1),])
  return(log2fc)
}

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
