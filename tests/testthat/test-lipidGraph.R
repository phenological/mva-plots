# package_list <- c(
#   'tidyverse', 'janitor', 'qualV', 'kableExtra', 'fusion', 'readxl', 'ggforce',
#   'broom', 'ropls', 'fusion', 'gridExtra', 'ggforce', 'kableExtra', 'purrr',
#   'ggcorrplot', 'rstatix', 'ggpubr', 'meantables', 'viridis', 'metabom8',
#   'RColorBrewer', 'tidyverse', 'ggrepel', 'scales', 'htmlTable', 'cowplot',
#   'fusion', 'missForest', 'reshape2', 'car', 'fontawesome', 'ggpubr', 'UpSetR',
#   'RGCCA'
# )
#
# for (package in package_list) {
#   if (!require(package, character.only = TRUE, quietly = TRUE)) {
#     message(paste0(package, " is not installed. Please install it before continuing."))
#   }
# }
#
# # install.packages("devtools")
# devtools::install_github("phenological/nmr-parser")
#
# pathToData <- file.path(Sys.getenv()['DATASETS'], "covid19")
# load(file.path(pathToData, "harvardC2", "datasets", "HUMegaTBL.RDA"))
#
# expV <- megatbl
# expV <- expV %>%
#   relocate(cohort, .after = sampleID)
# resV <- AnnoR
#
# expV %>% select(-c(`Glutamic acid_IVDR`,
#                    Glutamine_IVDR,
#                    `1-methylhistidine`,
#                    `Homocysteine AccQTag`,
#                    `Cysteine-AccQTag`)) -> expV
#
# #lipo <- nmr.parser::getLipoTable()
#
# # create names for publication
# id <- colnames(expV)[-2]
# idx <- match(id, tolower(lipo$id))
# id[!is.na(idx)] <- lipo$abbr[idx[!is.na(idx)]]
# publiNameList <- data.frame(id = colnames(expV)[-2], name = id)
# publiNameList$name <- gsub("_IVDR", " (IVDr)", publiNameList$name)
#
# # publiNameList$name[164] <- "3-hydroxybutyric acid (IVDr)"
#
# resV$onset <- factor(resV$Class,
#                      levels = c("Control",
#                                 "COVID",
#                                 "MISC",
#                                 "Other"),
#                      labels = c("control",
#                                 "acute",
#                                 "acute",
#                                 "remove")
# )
#
# resV$group <- factor(paste0(resV$Class, resV$onset),
#                      levels = c("Controlcontrol",
#                                 "COVIDacute",
#                                 "MISCacute",
#                                 "NANA",
#                                 "Otherremove"),
#                      labels = c("Controls (Children)",
#                                 "Covid-19 (Children)",
#                                 "MIS-C",
#                                 "remove",
#                                 "remove")
# )
#
# resV$color <- factor(resV$group,
#                      labels = c("darkgreen",
#                                 "firebrick3",
#                                 "dodgerblue",
#                                 "black"
#                      ))
#
# severity <- rep("none", nrow(resV))
# severity[resV$severity == "Severe"] <- "severe"
# severity <- factor(severity, levels = c("none", "severe"), labels = c(20, 6))
#
# resV$shape <- severity
#
# fi <- grepl("remove", resV$group)
# resV <- resV[!fi,]
# expV <- expV[!fi,]
#
#
# # refactoring
# resV$onset <- factor(resV$onset)
# resV$group <- factor(resV$group)
# #check dimensions
# cat(crayon::bgMagenta("max number of analytes:", length(
#   c(tagLipo, tagspc, tagAA, tagLip, tagsm, tagTryp)
# )))
# cat(crayon::bgMagenta("data matrix:", length(expV[-2])))
#
# length_match <-
#   length(c(tagLipo, tagspc, tagAA, tagLip, tagsm, tagTryp)) == length(expV[-2])
#
# cat(crayon::green("Matrix and selection match: ", length_match))
#
# missing <-
#   setdiff(c(tagLipo, tagspc, tagAA, tagLip, tagsm, tagTryp), id)
#
# cat(crayon::green("Removed analytes from analysis: ", missing))
#
# cat(crayon::bgBlue("total samples:", nrow(expV)))
#
# cat(crayon::bgBlue(paste(names(table(
#   resV$group
# )), table(resV$group), "\n")))
# cat(crayon::bgBlue(paste(names(table(
#   resV$cohort
# )), table(resV$cohort), "\n")))
#


test_that("You can use a data frame", {

  #don't specify control, expect warning
  expect_warning(object = lipidGraph(model = lipidData,
                                     stat = "fc",
                                     optns = list(factor = (lipidMetadata$Class))),
                 regexp = "No control specified in optns for factor. The first entry was set as the control")

  #do specify control
  lg<- lipidGraph(model = lipidData,
                  stat = "fc",
                  optns = list(factor = (lipidMetadata$Class),
                               control = "Control"))

  expect_equal(object = unique(lg[["data"]][["Group"]])[1], expected = "COVID")
  expect_equal(object = unique(lg[["data"]][["Group"]])[2], expected = "MISC")


  #specify which group want graphed
  lg<- lipidGraph(model = lipidData,
                  stat = "fc",
                  optns = list(factor = lipidMetadata$Class,
                               control = "Control",
                               columns_to_plot = "MISC"))
  expect_equal(object = unique(lg[["data"]][["Group"]])[1], expected = "MISC")
  expect_equal(object = unique(lg[["data"]][["Group"]])[2], expected = as.character(NA))

  #if change control, do the correct groups show up? use cd instead of fc

  lg<- lipidGraph(model = lipidData,
                  stat = "cd",
                  optns = list(factor = (lipidMetadata$Class),
                               control = "MISC"))
  expect_length(object = unique(lg[["data"]][["Group"]]), n = 2)
  expect_contains(object = lg[["data"]][["Group"]], expected = "Control")
  expect_contains(object = lg[["data"]][["Group"]], expected = "COVID")

})

test_that("NA in factor will throw error",{
  #change one lipidMetadata Class entry to NA
  lipidMetadata2 <- lipidMetadata
  lipidMetadata2[1,"Class"] <- NA

  expect_error(object = lipidGraph(model = lipidData,
                                   stat = "cd",
                                   optns = list(factor = (lipidMetadata2$Class),
                                                control = "Control")),
               regexp = "One of your factors is NA, please change this before running lipidGraph")

})

test_that("external stat can be supplied, including with more than one group to be graphed",{

  #supply external stat

   ########adjusted p-value########
   model <- lipidData

   optns <- list(factor = as.vector(lipidMetadata$Class), control = "Control", method = "fdr")

   id <- colnames(model)
   df <- model

     # #empty df
      pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
      pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

     df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
     unique_factors <- unique(df[,"factor"])

     for(j in 2: length(unique_factors)){

       df2 <- df[df[,"factor"] %in% c(1, j), ]

       pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
       for(i in 1:(ncol(df2)-1)){
         pval[i,]<-kruskal.test(df2[,i], df2[,"factor"])$p.value
       }

       pvalUnadjusted[,j] <- pval

       #rescaled and adjusted p-value
       for(i in 1:(ncol(df2)-1)){
         pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
       }
     }

     # Create a mapping between numbers and words, rename pval dataframe columns
     mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))

     testnames<- as.data.frame(mapping)
     testnames$rowName <- rownames(testnames)

     for (i in seq_len(nrow(testnames))) {
       col_number <- as.numeric(testnames[i, 2])
       new_col_name <- as.character(testnames[i, 1])
       names(pvalUnadjusted)[col_number] <- new_col_name
       names(pvalRescaled)[col_number] <- new_col_name
     }

   lg<- lipidGraph(model = lipidData,
                   stat = "external",
                   optns = list(factor = lipidMetadata$Class,
                                control = "Control",
                                external = pvalRescaled))

   expect_length(object = unique(lg[["data"]][["Group"]]), n = 2)
   expect_contains(object = lg[["data"]][["Group"]], expected = "COVID")
   expect_contains(object = lg[["data"]][["Group"]], expected = "MISC")

})

   #use PCA model

  test_that("You can use PCA object", {
    lipidData <- readRDS(system.file("extdata", "lipidData.rds", package = "mva.plots"))
    lipidMetadata <- readRDS(system.file("extdata", "lipidMetadata.rds", package = "mva.plots"))

    pca<- PCA(data = lipidData, plot = FALSE, rank =3)

    lg<- lipidGraph(model = pca,
                    stat = "cd",
                    optns = list(factor = (lipidMetadata$Class),
                                 control = "Control"))

    #did the class, side chain and id turn out in the correct format? Use the first lipid to test
    expect_equal(object = lg[["data"]][["class"]][1], expected = "CE")
    expect_equal(object = lg[["data"]][["sc1"]][1], expected = "14:0")
    expect_equal(object = lg[["data"]][["id"]][1], expected = "CE(14:0)")
  })

   #use oplsda model
test_that("You can use oplsda object", {
  lipidData <- readRDS(system.file("extdata", "lipidData.rds", package = "mva.plots"))
  lipidMetadata <- readRDS(system.file("extdata", "lipidMetadata.rds", package = "mva.plots"))
  op<- oplsda(X = lipidData, Y = lipidMetadata$Class, type = "PLS")
  lg<- lipidGraph(model = op,
                  stat = "cd",
                  optns = list(factor = (lipidMetadata$Class),
                               control = "Control"))

  # expect_gt(length(lg[["data"]][["class"]]), 0)
  # expect_type(object = lg[["data"]][["class"]], type = "character")
  # expect_type(object = lg[["data"]][["sc1"]], type = "character")
  # expect_type(object = lg[["data"]][["Value"]], type = "double")
  #
  #did the class, side chain and id turn out in the correct format? Use the first lipid to test
  expect_equal(object = lg[["data"]][["class"]][1], expected = "CE")
  expect_equal(object = lg[["data"]][["sc1"]][1], expected = "14:0")
  expect_equal(object = lg[["data"]][["id"]][1], expected = "CE(14:0)")

})


# ################
# #old annotation.dae
# ANNO<-local(get(load("~/OneDrive - Murdoch University/datasets/covid19/mauritius/DataElements/covid19_mauritius_ANNO.daE")))
# ANNO<-ANNO@obsDescr[[1]]
#
# #Keep only essential info
# ANNO <- ANNO[,1:7]
# class(ANNO$sourceID) #character
# ANNO$sourceID <- as.integer(ANNO$sourceID)
#
# #Replace old info with new
# #install.packages("readxl")
# library(readxl)
# updated060623 <- read_excel("~/Downloads/mauritiusMeta1vs3Comparison2.0 (2).xlsx", sheet = "meta3")
# updated060623$`Survey No.`<- as.numeric(updated060623$`Survey No.`)
#
# #updated060623 <- read.csv("~/Documents/Mauritius/Deidentified_Metabolic_data_updated_06_06_23.csv")
# class(updated060623$`Survey No.`)
# colnames(updated060623)[which(names(updated060623) == "Survey No.")] <- "sourceID"
#
# common_columns <- intersect(names(updated060623), names(ANNO))
# ANNO <- merge(updated060623, ANNO, by = common_columns)
#
# #check the update happened
# ANNO[(which(ANNO$sourceID == 242)), "Profile No"]
# #DM before, is now NDM
#
# #split into ser and pla
# #SER_ANNO<-ANNO[which(ANNO$sampleMatrixType=="SER"),]
# PLA_ANNO<-ANNO[which(ANNO$sampleMatrixType=="PLA"),]
#
# rm(updated060623, ANNO)
#
# lipids <- local(get(load("~/OneDrive - Murdoch University/datasets/covid19/mauritius/DataElements/covid19_mauritius_PLA_Lipids.daE")))
# lipidsData <- lipids@.Data
# lipidsMeta <- as.data.frame(lipids@obsDescr)
# lipidsMeta$sourceID <- gsub(".*_(\\d+)_\\d+$", "\\1", lipidsMeta$path)
#
# #remove LTRs etc, using only sampleID with COV
# lipidsData<-lipidsData[grep("COV",lipidsMeta$sampleID),]
# lipidsMeta<-lipidsMeta[grep("COV",lipidsMeta$sampleID),]
#
# #change to data to df and attach sampleIDs
# lipidsData <-as.data.frame(lipidsData)
# lipidsData$sampleID <- lipidsMeta$sampleID
#
# #match pla anno with lipid annotation with sampleID
# lipidsMeta2 <- PLA_ANNO[match(lipidsMeta$sampleID, PLA_ANNO$sampleID),]
#
# #merge all anno
# lipidsMeta$sourceID <-as.integer(lipidsMeta$sourceID)
# common_columns <- intersect(names(lipidsMeta), names(lipidsMeta2))
#
# #the values from the second data frame will overwrite the values from the first data frame
# #lipidsMeta3 <- merge(lipidsMeta, lipidsMeta2, by = c("sampleID","projectName","cohortName","sampleMatrixType", "sourceID"))
# lipidsMeta3 <- merge(lipidsMeta, lipidsMeta2, by = common_columns)
#
# lipids <- merge(lipidsMeta3, lipidsData, by = "sampleID")
# lip <- lipids
#
# model = lip[,146:ncol(lip)]
# stat = "cd"
# optns = list(factor = lip$`Profile No`,
#              control = "NDM")
# #check factors to see if any group is empty
# if (any(is.na(optns$factor))) {
#   stop("One of your factors is NA, please change this before running lipidGraph")
# }
#
# if (!"control" %in% names(optns)) {
#   optns$control <- 1
#   #print warning
#   warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
# }
#
# if("method" %in% names(optns)){
#   method = optns$method
# }else{
#   method <- "bonferroni"
# }
#
# #df
# if(is(model)[1]== "data.frame"){
#   id <- colnames(model)
#   df <- model
#   df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
# }
#
# #if the labels us . instead of - and :
# if (any(grepl("\\.", id))) {
#   # Apply substitution
#   id <- sub("\\(P\\.", "(P-", id)
#   id <- sub("\\(O\\.", "(O-", id)
#   id <- gsub("\\.", ":", id)
# }
#
# unique_factors <- unique(df[,"factor"])
#
#
# if(!("discretePalette" %in% names(optns))){
#   optns$discretePalette <- c("#66C2A5",
#                              "#FC8D62",
#                              "#8DA0CB",
#                              "#E78AC3",
#                              "#A6D854",
#                              "#FFD92F",
#                              "#E5C494",
#                              "#B3B3B3")
#
# }
#
# #shape
# if(!("shape" %in% names(optns))){
#   optns$shape = "circle"}
#
# #alpha
# if(!("alpha" %in% names(optns))){
#   optns$alpha = 0.3}
#
# #theme
# if(!("theme" %in% names(optns))){
#   theme <- theme()
# } else{theme <- optns$theme}
#
# #guides
# if(!("guides" %in% names(optns))){
#   guides <- guides(color = guide_legend(title = "Direction"),
#                    size = guide_legend(title = paste0("|",stat,"|")))
# } else{guides <- optns$guides}
#
# ########cliffs delta##########
#
# if(stat == "cd"){
#   cd <- cliffsDelta(model = model, optns = optns)
#   if(length(cd) == 1){
#     cd <- cbind(NA, cd)
#   }
#   statistic <- cd
# }
#
# #########lipid data frame############
#
# #make class and sc info
# #make class and sc info
# lipids <- id
# lmc <- strsplit(lipids, "\\(")
# lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")
#
# r <- list()
# for (i in 1:length(lmc)) {
#   struc <- strsplit(lmc[[i]][2], "_")[[1]]
#   if (length(struc) == 1 | lmc[[i]][1] == "TAG") {
#     totalCarbon <- gsub("[a-zA-Z\\-]+",
#                         "",
#                         strsplit(struc, ":")[[1]][1])
#     if (lmc[[i]][1] == "TAG") {
#       sideChain <- gsub("[a-zA-Z\\-]+",
#                         "",
#                         strsplit(lmc[[i]][2], "_")[[1]][2])
#     } else {
#       unsat <- gsub("[a-zA-Z\\-]+",
#                     "",
#                     strsplit(struc, ":")[[1]][2])
#       sideChain <- paste0(totalCarbon, ":", unsat)
#     }
#   } else {
#     t <- strsplit(struc, ":")
#     sc <- unlist(lapply(t, function(x)
#       as.numeric(
#         gsub("[a-zA-Z\\-]+",
#              "",
#              x[1])
#       )))
#     unsatSc <- unlist(lapply(t, function(x)
#       as.numeric(
#         gsub("[a-zA-Z\\-]+",
#              "",
#              x[2])
#       )))
#     totalCarbon <- sum(sc)
#     sideChain <- paste0(sc, ":", unsatSc)
#     unsat <- sum(unsatSc)
#   }
#   r[[i]] <- c(lmc[[i]][1], totalCarbon, unsat, sideChain)
# }
#
# #find the max number of segments r has been split into for any single entry in the list
# l <- max(unlist(lapply(r, function(x) length(x))))
#
# #make all elements of r the length you determined as l with NA's in the empty slots
# r <- lapply(r, function(x) c(x, rep(NA, l-length(x))))
#
# #make r into data frame with a col for class, totalCarbon, unsat and sidechain
# lipidClass <- data.frame(do.call("rbind", r))
# colnames(lipidClass) <- c("class", "nC", "r", "sc1", "sc2")
#
# ld<-cbind(statistic, id, lipidClass)
#
# #which groups will be plotted
# if("columns_to_plot" %in% names(optns)){
#   columns_to_plot <- optns$columns_to_plot
# } else{columns_to_plot <- colnames(ld)[2:(which(colnames(ld) == "id") - 1)]}
#
# if(length(columns_to_plot) > length(optns$discretePalette)) {
#   warning("You have more groups to plot than colors supplied, the default is 8 colors. Please supply the same or more number of colors as number of groups to plot.")
# }
#
# # Filter the data frame to include only the relevant columns
# plot_data <- ld[, c("class", "sc1", "id",columns_to_plot)]
#
# # Reshape the data frame to long format manually
# plot_data_long <- reshape(plot_data,
#                           idvar = c("class", "sc1", "id"),
#                           varying = list(columns_to_plot),
#                           v.names = c("Value"),
#                           times = columns_to_plot,
#                           direction = "long")
#
#
# colnames(plot_data_long) <- c("class", "sc1", "id", "Group", "Value")
#
#   # Assuming your data frame is named plot_data_long
#   plot_data_long$Direction <- ifelse(plot_data_long$Value > 0, "positive", "negative")
#
#
# ########### Create the ggplot###########
# lipidGraph <- ggplot(plot_data_long, aes(x = class,
#                                          y = sc1,
#                                          color = Direction)) +
#   geom_jitter(aes(size = abs(Value)),
#               position = position_jitter(height = 0.1,
#                                          width = 0.3),
#               alpha = optns$alpha,
#               shape = optns$shape) +
#   xlab("Lipid Class") +
#   ylab("Side-Chain Length") +
#   theme_bw() +
#   theme(panel.grid.major = element_line(color = "gray95"),
#         panel.grid.minor = element_blank()
#   ) +
#   scale_color_manual(values  = c("negative" = "orange", "positive" = "green"),
#                      na.value = "grey50" ) +
#   theme() +
#     guides(color = guide_legend(title = "Direction"),
#            size = guide_legend(title = paste0("|",stat,"|"))) +
#     labs(caption = "NDM as the control")
#
#
#
#
# lp<- lipidGraph(model = lip[,146:ncol(lip)],
#            stat= "cd",
#            optns= list(factor = lip$`Profile No`,
#                        control = "NDM"))
