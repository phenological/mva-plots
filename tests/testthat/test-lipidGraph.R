# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
# # Loading In
#
# ## Library packages
#
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
# #devtools::load_all("~/Documents/Git/phenological /mva-plots/")
# devtools::install_github("phenological/nmr-parser")
# set.seed(123)
#
# ## Data
#
#
# pathToData <- file.path(Sys.getenv()['DATASETS'], "covid19")
# load(file.path(pathToData, "harvardC2", "datasets", "HUMegaTBL.RDA"))
#
#
# # Setting a backup
#
#
# expV <- megatbl
# expV <- expV %>%
#   relocate(cohort, .after = sampleID)
# resV <- AnnoR
#
#
# ## Cleaning for analysis
# # removing glutamine and glutamic from ivdr
# expV %>% select(-c(`Glutamic acid_IVDR`,
#                    Glutamine_IVDR,
#                    `1-methylhistidine`,
#                    `Homocysteine AccQTag`,
#                    `Cysteine-AccQTag`)) -> expV
#
# lipo <- nmr.parser::getLipoTable()
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
#
#
# ## Final cohort numbers
#
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
# ## Function to format OPLS eruption plot
#
#
# # printing table of most relevant variables
#
# formatEruptionTable <- function(eruption) {
#   # adjusting number format
#   er <- as.data.frame(eruption@suppLs[["eruptionData"]])
#   er$pval <- formatC(er$pval, format = "e", 3)
#   er$pval <- as.numeric(as.character(er$pval))
#   er$pvalRaw <- as.numeric(as.character(er$pvalRaw))
#
#   # printing
#   tb <- er %>%
#     rename(Cliffs_Delta = cd, adj.pVal = pval, FoldChange = fc, Correlation = corr, pVal = pvalRaw ) %>%
#     relocate(id, .before = everything()) %>%
#     relocate(loadings, .after = id) %>%
#     arrange(Cliffs_Delta) %>%
#     mutate_at(3:6, round, 3)
#
#   # matching and adding description
#   nmr.parser::getLipoTable()
#   idx <- match(tb$id, (lipo$ID))
#   tb$Description[!is.na(idx)] <- lipo$tag[idx[!is.na(idx)]]
#   tb$Description[is.na(idx)] <- tb$id[is.na(idx)]
#
#   tb <- tb %>%
#     filter(adj.pVal < 0.05) %>%
#     select(Description, Cliffs_Delta, pVal, adj.pVal, FoldChange, Correlation) %>%
#     mutate(Cliffs_Delta = format(round(Cliffs_Delta, 2), nsmall = 2),
#            `Adj. p-value` = signif(adj.pVal, digits = 3),
#            Correlation = format(round(Correlation, 3))) %>%
#     rename(`Cliff's Delta` = Cliffs_Delta)
# }
#
# reshapeTable <- function(table, n) {
#   nr <- ceiling(nrow(table) / n)
#   while (nrow(table) < nr * n) {
#     table <- rbind(table, rep("", ncol(table)))
#   }
#   new <- table[1:nr,]
#   for (i in 2:n) {
#     beg <- (i - 1) * nr + 1
#     end <- i * nr
#     chk <- table[beg:end,]
#     colnames(chk) <- paste0(colnames(table), "..", i)
#     new <- cbind(new, chk)
#   }
#   return(new)
# }
#
#
#
#
# #names of lipids appears as such: "TAG(60:10_FA22:6)" "TAG(60:11_FA22:5)" "TAG(60:11_FA22:6)" "PS(20:0_20:5)"     "PS(20:0_22:4)"     "PS(20:0_22:5)"     "PS(20:0_22:6)"     "SM(14:0)"
# #[473] "SM(16:0)"          "SM(18:0)"          "SM(18:1)" "PG(20:0_18:3)"     "PG(20:0_20:1)"     "PG(20:0_20:2)"     "PG(20:0_20:3)"     "PG(20:0_20:4)"     "PG(20:0_22:4)"     "PG(20:0_22:5)"
# #[409] "PG(20:0_22:6)"     "PI(14:0_18:1)"     "PI(16:0_16:0)"     "PI(16:0_16:1)"     "PI(16:0_18:0)"     "PI(16:0_18:1)"     "PI(16:0_18:2)"     "PI(16:0_18:3)"
# #[417] "PI(16:0_20:1)"     "PI(16:0_20:2)"  and so on
#
# #want to create lipid classes. first, split  "CE(14:0)"  into "CE"  and  "14:0)"
# lmc <- strsplit(tagLip[-1], "\\(")
# lmc <- strsplit(tagLip[-1], "\\(")
#
# #then remove last bracket to get "CE"  and  "14:0"
# lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")
# lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")
# #need the length of struc and the name from lmc
# #store just the numbers
# struc <- strsplit(lmc[[1]][2], "_")[[1]]
#
# struc <- list()
# for (i in 1:length(lmc)) {
#   struc[[i]] <- strsplit(lmc[[i]][2], "_")[[1]]}
#
# #the alphbetical part
# lmc[[1]][1]
# #depending on the length of struc and what the alphabetical part is, do something
# #length struc == 1 OR alphabetical is TAG
#
#
#
# lmc[[1]]
# #[1] "CE"   "14:0"
#
# totalCarbon <- gsub("[a-zA-Z\\-]+",
#                     "",
#                     strsplit(struc[[1]], ":")[[1]][1])
# #14
#
# unsat <- gsub("[a-zA-Z\\-]+",
#               "",
#               strsplit(struc[[1]], ":")[[1]][2])
# #0
#
# sideChain <- paste0(totalCarbon, ":", unsat)
# #"14:0"
#
# lmc[[640]]
# #[1] "TAG"         "51:3_FA17:0"
#
# totalCarbon <- gsub("[a-zA-Z\\-]+",
#                     "",
#                     strsplit(struc[[640]], ":")[[1]][1])
#
# sideChain <- gsub("[a-zA-Z\\-]+",
#                   "",
#                   strsplit(lmc[[640]][2], "_")[[1]][2])
#
#
# lmc[[123]]
# #"HCER"       "d18:0_18:0"
# length(struc[[123]])
# #2
#
# #separate into "d18", "0", "18" and "0"
#   t <- strsplit(struc[[123]], ":")
#
# #get numbers before ":" and make numeric
#   sc <- unlist(lapply(t, function(x)
#     as.numeric(
#       gsub("[a-zA-Z\\-]+",
#            "",
#            x[1])
#     )))
#
# #get the numbers after ":" and make numeric
#   unsatSc <- unlist(lapply(t, function(x)
#     as.numeric(
#       gsub("[a-zA-Z\\-]+",
#            "",
#            x[2])
#     )))
#
#
#   totalCarbon <- sum(sc)
#
#   sideChain <- paste0(sc, ":", unsatSc)
#
#   unsat <- sum(unsatSc)
#
# function(lipids, measurements, stat, group, optns = list()){
#
#   if(is(lipids[1] == "character")){
#
#   }
#
#   lmc <- strsplit(lipids, "\\(")
#   lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")
#   lmc <- lipids
#
#   #create lipid classes table
#
#   r <- list()
#   for (i in 1:length(lmc)) {
#     struc <- strsplit(lmc[[i]][2], "_")[[1]]
#     if (length(struc) == 1 | lmc[[i]][1] == "TAG") {
#       totalCarbon <- gsub("[a-zA-Z\\-]+",
#                           "",
#                           strsplit(struc, ":")[[1]][1])
#       if (lmc[[i]][1] == "TAG") {
#         sideChain <- gsub("[a-zA-Z\\-]+",
#                           "",
#                           strsplit(lmc[[i]][2], "_")[[1]][2])
#       } else {
#         unsat <- gsub("[a-zA-Z\\-]+",
#                       "",
#                       strsplit(struc, ":")[[1]][2])
#         sideChain <- paste0(totalCarbon, ":", unsat)
#       }
#     } else {
#       t <- strsplit(struc, ":")
#       sc <- unlist(lapply(t, function(x)
#         as.numeric(
#           gsub("[a-zA-Z\\-]+",
#                "",
#                x[1])
#         )))
#       unsatSc <- unlist(lapply(t, function(x)
#         as.numeric(
#           gsub("[a-zA-Z\\-]+",
#                "",
#                x[2])
#         )))
#       totalCarbon <- sum(sc)
#       sideChain <- paste0(sc, ":", unsatSc)
#       unsat <- sum(unsatSc)
#     }
#     r[[i]] <- c(lmc[[i]][1], totalCarbon, unsat, sideChain)
#   }
#
#   #find the max number of segments r has been split into for any single entry in the list
#   l <- max(unlist(lapply(r, function(x) length(x))))
#
#   #make all elements of r the length you determined as l with NA's in the empty slots
#   r <- lapply(r, function(x) c(x, rep(NA, l-length(x))))
#
#   #make r into data frame with a col for class, totalCarbon, unsat and sidechain
#   lipidClass <- data.frame(do.call("rbind", r))
#
#   colnames(lipidClass) <- c("class", "nC", "r", "sc1", "sc2")
# }
#
#
#
# #make desired statistics for size based on the chosen groups for color
#
#
# # statT_filtered <- statT[statT$`p-Value adj. > 1.3,] %>% arrange(`MIS-C)
# statT_filtered <- statT[statT$`p-Value adj. > 1.3,] %>% arrange(`MIS-C`)
# keep <- statT_filtered$var
#
# # getting proper names
# lipo <- nmr.parser::getLipoTable()
#
# idx <- match(tolower(statT_filtered$var), tolower(lipo$ID))
# statT_filtered$var[!is.na(idx)] <- lipo$tag[idx[!is.na(idx)]]
#
# bLip <- statT_filtered[statT_filtered$var %in% tagLip,]
# idx <- match(bLip$var, tagLip)
# bLip$class <- lipidClass$class[idx]
# bLip$sc <- lipidClass$sc1[idx]
#
# ########cliffs delta##########
# cd <- cliffsDelta(model = model, optns = optns)
#
# ##########Fold change#########
# fc <- foldChange(model = model, optns = optns)
#
# idx<- which(df[,"factor"] == 1)
# control <- df[idx,]
# treatment <- df[-idx,]
#
# #logmeans
# c <- log2(apply(X = control, MARGIN = 2, FUN = mean))
# t <- log2(apply(X = treatment, MARGIN = 2, FUN = mean))
#
# #log2 fold change
# log2fc <- as.data.frame(c-t)
# log2fc <- as.data.frame(log2fc[1:(nrow(log2fc)-1),])
#
# ########adjusted p-value########
# pval<-list()
# for(i in 1:(ncol(df)-1)){
#   pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
# }
#
# unlist(pval)
# pvalUnadjusted <- t(as.data.frame(pval))
# pvalAdjusted <- p.adjust(pval, method = method)
# pvalRescaled <- abs(log10(pvalAdjusted))
# pvalRescaled <- as.data.frame(pvalRescaled)
#
# #eruption data frame
# ed<-cbind(cd, fc, pvalRescaled, pvalUnadjusted)
#
# colnames(ed)<-c("cd", "fc", "pval", "pvalRaw")
#
# #make the graph
# plip1 <- ggplot(bLip, aes(x = class, y = sc)) +
#   # geom_point(size = bLip$`MIS-C (Kids)`, color = "firebrick3") +
#   # geom_point(size = bLip$`Controls (Kids)`, color = "dodgerblue")
#   geom_jitter(aes(size = abs(MIS-C)),
#               position = position_jitter(height = 0.1, width = 0.3),
#               alpha = 0.3,
#               color = "dodgerblue") +
#   geom_jitter(aes(size = abs(Covid-19 (Children))),
#               position = position_jitter(height = 0.1, width = 0.3),
#               alpha = 0.3,
#               color = "firebrick3") +
#   scale_size_continuous(range = c(0, 5),
#                         trans = scales::exp_trans(base = 1.3),
#                         breaks = c(1, 1.5, 2)) +
#   xlab("Lipid Class") +
#   ylab("Side-Chain Length") +
#   theme_bw() +
#   theme(panel.grid.major = element_line(color = "gray95"),
#         panel.grid.minor = element_blank(),
#         legend.position = "none")
#
# plip2 <- ggplot(bLip[bLip$class == "TAG",], aes(x = class, y = sc)) +
#   # geom_point(size = bLip$`MIS-C (Kids)`, color = "firebrick3") +
#   # geom_point(size = bLip$`Controls (Kids)`, color = "dodgerblue")
#   geom_jitter(aes(size = abs(MIS-C)),
#               position = position_jitter(height = 0.1, width = 0.3),
#               alpha = 0.3,
#               color = "dodgerblue") +
#   geom_jitter(aes(size = abs(Covid-19 (Children))),
#               position = position_jitter(height = 0.1, width = 0.3),
#               alpha = 0.3,
#               color = "firebrick3") +
#   scale_size_continuous(range = c(0, 5),
#                         trans = scales::exp_trans(base = 1.3),
#                         breaks = c(1, 1.5, 2)) +
#   xlab("") +
#   theme_bw() +
#   theme(panel.grid.major = element_line(color = "gray95"),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",
#         # axis.text.y=element_blank(),
#         axis.title.y=element_blank())



