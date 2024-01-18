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
                  optns = list(factor = (lipidMetadata$Class),
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
                   optns = list(factor = (lipidMetadata$Class),
                                control = "Control",
                                external = pvalRescaled))

   expect_length(object = unique(lg[["data"]][["Group"]]), n = 2)
   expect_contains(object = lg[["data"]][["Group"]], expected = "COVID")
   expect_contains(object = lg[["data"]][["Group"]], expected = "MISC")

})

   #use PCA model

  test_that("You can use PCA object", {
    #lipidData <- readRDS(system.file("extdata", "lipidData.rds", package = "mva.plots"))
    #lipidMetadata <- readRDS(system.file("extdata", "lipidMetadata.rds", package = "mva.plots"))

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

