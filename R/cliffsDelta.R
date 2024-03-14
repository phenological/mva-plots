#' Cliff's Delta
#'
#' Cliff's delta, a measure of a non-parametric effect size measure used to
#' quantify the difference between two groups or conditions. It provides a
#' measure of the ordinal association between two variables. Ranges between (-1)
#' and (1).
#'
#' @param model A PCA or oplsda object from mva.plots or a data frame.
#' @param optns A list passed to cliffsDelta with additional arguments.
#'  \itemize{
#'    \item{factor} {An object the same length as the data in the model supplied
#'    that must have at least 2 unique groups such as treatment and control. More
#'    than 2 is allowable.}
#'    \item{control} {Character to set which group with the supplied factor you wish
#'    to set as the control for comparison to all other groups. If your factor is
#'    already numeric, when specifying the control, supply it as a character, for
#'    example, control = "0". If not manually set, the unique factor assigned one
#'    from the supplied factor will be automatically selected}
#' }
#' @return Creates a dataframe of Cliff's Detlas. For more than 2 groups, pairwise
#' calculations to the selected control will be returned.
#' @export

cliffsDelta <- function(model, optns = optns) {

  if (!"control" %in% names(optns)) {
    optns$control <- 1
    # #print warning
    # warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
  }

  if(is(model)[1] == "list"){

    #model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
    df <- model$data$rawData
    df$factor <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
    cd_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }

  if(is(model)[1] == "opls"){
    df <- as.data.frame(model@suppLs[["x"]], check.names = F)
    #df[,"factor"] <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    df$factor <- as.numeric(relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control))
    cd_df <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
  }

  if(is(model)[1] == "data.frame"){
    df <- model
    df$factor <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
    # Initialize an empty data frame to store cd values
    cd_df <- data.frame(matrix(NA, nrow = ncol(model), ncol = 1))
  }
  #if factor is a data table, it needs to be changed to work with cd and fc
  if(is(optns$factor)[1] == "data.table" | is(optns$factor)[1] == "tbl_df"){
    optns$factor <- unlist(optns$factor)
  }

  if(is(df)[1] == "data.table" | is(df)[1] == "tbl_df"){
    df <- as.data.frame(df, check.names = F)
  }

  # ##cliffs delta manually

  #control
  idx<- which(df[,"factor"] == 1)
  control <- df[idx,]
  c<-list()
  for(i in 1:(ncol(control)-1)){
    c[[i]] <- sort(control[,i])
  }


  # Dynamically assigning factors for one to one calculations
  unique_factors <- unique(df[,"factor"])

  for(j in 2:length(unique_factors)){

    #treatment
    idx<- which(df[,"factor"] == j)
    treatment <- df[idx,]
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

    #if rescale is required
    n <- length(which(df[,"factor"]==1))
    m <- length(which(df[,"factor"]==j))
    # n <- table(df[,"factor"])[1]
    # m <- table(df[,"factor"])[j]
    rescale.factor = (n*m-1)/(n*m)


    for (i in 1:(ncol(df) - 1)) {
      cd[[i]] <- ifelse(abs(cd[[i]]) == 1,
                        cd[[i]] * rescale.factor,
                        cd[[i]])
    }

    # Assign the log2fc as a column in the data frame
    col_name <- paste0("cd", j)
    cd_df[, col_name] <- unlist(cd)

    #
    # for(i in 1:(ncol(df)-1)){
    #   ifelse(test = (abs(cd[[i]]) == 1),
    #          yes = cd[[i]]*rescale.factor,
    #          no = cd)
    # }
    # unlist(cd)
    # cd<-as.vector(t(as.data.frame(cd)))

    # # Assign the log2fc as a column in the data frame
    # col_name <- paste0("cd", j)
    # cd_df[, col_name] <- cd

  }

  # Create a mapping between numbers and words, rename cd dataframe columns
  mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))

  testnames<- as.data.frame(mapping, check.names = F)
  testnames$rowName <- rownames(testnames)

  for (i in seq_len(nrow(testnames))) {
    col_number <- as.numeric(testnames[i, 2])
    new_col_name <- as.character(testnames[i, 1])
    names(cd_df)[col_number] <- new_col_name
  }

  #if only 2 factors, just need the second column, since first is all NA (control to control)
  if(length(unique_factors) == 2){
    #cd_df <- as.data.frame(cd_df[,2])
    cd_df <- cd_df[, 2, drop = FALSE]
  }
  return(cd_df)
}

