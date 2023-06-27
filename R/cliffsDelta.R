#' Cliff's Delta


cliffsDelta <- function(model, factor) {

  model$data$rawData$factor <- as.numeric(as.factor(factor))

  df <- model$data$rawData

  n <- table(df$factor)[1]
  m <- table(df$factor)[2]

  # ##cliffs delta manually
  rescale.factor = (n*m-1)/(n*m)
  idx<- which(df[,"factor"] == 1)
  control <- df[idx,]
  treatment <- df[-idx,]

  #control
  c<-list()
  for(i in 1:(ncol(control)-1)){
    c[[i]] <- sort(control[,i])
  }

  #treatment
  t<-list()
  for(i in 1:(ncol(treatment)-1)){
    t[[i]] <- sort(treatment[,i])
  }

  #dominance matrix
  d<-list()
  for(i in 1:(ncol(df)-1)){
    d[[i]] <-sign(outer(t[[i]], c[[i]], FUN="-"))
  }

  #cliffs delta
  cd<-list()
  for(i in 1:(ncol(df)-1)){
    cd[[i]]<-mean(d[[i]])
  }

  for(i in 1:(ncol(df)-1)){
    ifelse(test = abs(cd[[i]]) == 1,
           yes = cd[[i]]*rescale.factor,
           no = cd)
  }
  unlist(cd)
  cd<-t(as.data.frame(cd))


  ##cliffs delta using stats via calculating U statistic (W wilcoxon statistic)

  # U <- list()
  # for(i in 1:(ncol(df)-1)){
  #   formula <- as.formula(paste(colnames(df)[i]," ~ factor", sep=""))
  #   model <- wilcox.test(formula, data = df)
  #   U[[i]] <- model[["statistic"]]
  # }
  #
  #
  #    U <- (unlist(U))
  #
  #    cd <- (U/(n*m)-0.5)*2

  return(cd)
}

