
test_that("interactive t and F work", {
  data("NMR_1D_rat_urine_spectra")

  # use small sample, note the spectra is only the region 2 to 4 ppm
  Xsample <- X[1:10,]

  expect_no_error(object = matspec(X = Xsample,
                                   ppm = ppm,
                                   interactive = T))

  expect_no_error(object = matspec(X = Xsample,
                                   ppm = ppm,
                                   interactive = F))
})

test_that("non-interactive lower resolution works", {
  data("NMR_1D_rat_urine_spectra")

  # use small sample, note the spectra is only the region 2 to 4 ppm
  Xsample <- X[1:10,]

  expect_no_error(object = matspec(X = Xsample,
                                   ppm = ppm,
                                   interactive = F,
                                   resolution = "partial"))

})

# #spectra simulation
#
# # Function to simulate a Gaussian peak
# simulate_peak <- function(x, center, sigma, intensity) {
#   exp(-(x - center)^2 / (2 * sigma^2)) * intensity
# }
#
# # Function to generate example NMR spectrum data
# generate_example_spectrum <- function() {
#   # Create a range of chemical shifts (x-axis values)
#   chemical_shifts <- seq(0, 10, by = 0.1)
#
#   # Simulate two peaks
#   peak1 <- simulate_peak(chemical_shifts, center = 3, sigma = 0.5, intensity = 5)
#   peak2 <- simulate_peak(chemical_shifts, center = 7, sigma = 0.7, intensity = 8)
#
#   # Combine peaks to create the spectrum
#   spectrum <- peak1 + peak2
#
#   # Create a data frame with chemical shifts and intensities
#   spectrum_data <- data.frame(Chemical_Shift = chemical_shifts, Intensity = spectrum)
#
#   return(spectrum_data)
# }
#
# # Generate example spectrum data
# example_spectrum_data <- generate_example_spectrum()
#
# # Plot the example NMR spectrum
# ggplot(example_spectrum_data, aes(x = Chemical_Shift, y = Intensity)) +
#   geom_line(color = "blue") +
#   labs(title = "Example NMR Spectrum", x = "Chemical Shift", y = "Intensity")
#
# matspec(X = (example_spectrum_data$Chemical_Shift), ppm = t(example_spectrum_data$Chemical_Shift))

# library(metabom8)
#
#
#
# post_processing <- function(X, ppm, type = FALSE, bc = FALSE, pqn = FALSE) {
#   if (type == TRUE) {
#     X=calibrate(X,ppm, type='glucose')
#   }
#   ##    Remove water peak from the reduced spectra data X_reduced (4.6 to 4.85)
#   idx = get_idx(range = c(4.6,4.85),ppm) # change this part to 4.6 to 6.0 to remove both water and urea
#   Xc1 = X[,-idx]
#   ppm1 = ppm[-idx]
#
#   ##    Remove TPS and lower ppm range
#   idx = get_idx(range = c(min(ppm),0.4),ppm1)
#   Xc2 = Xc1[,-idx]
#   ppm2 = ppm1[-idx]
#
#   ##    Remove higher ppm range with no signal
#   idx = get_idx(range = c(9.5,max(ppm)),ppm2)
#   Xc3 = Xc2[,-idx]
#   ppm3 = ppm2[-idx]
#
#   ##    Baseline correction
#   if (bc == TRUE) {
#     Xc3 = bcor(Xc3)
#   }
#   ##    Normalisation (scaling)
#   if(pqn == TRUE){
#     Xc3 = pqn(Xc3, add_DilF = 'dquott', bin=NULL)
#   }
#
#   return(list("Xn" = Xc3, "ppm" = ppm3))
# }
#
# #working new
#
# matspec<-function (X, ppm, roi = c(0.5, 9.5), interactive = TRUE, ...)
# {
#   if (is.null(ppm)) {
#     ppm <- as.numeric(colnames(X))
#   }else {
#     if (length(ppm)!=ncol(X))
#       stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.")
#   }
#   if (!missing(roi)){
#     fi <- ppm >= roi[1] & ppm <= roi[2]
#   }else{
#     fi <- ppm >= 0 & ppm <= 9.5
#   }
#   if(interactive){
#     df <- reshape2::melt(X[, fi])
#     x <- list(title = "ppm", autorange = "reversed")
#     y <- list(title = "Intensity")
#     # cols <- suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(10,
#     #                                                                    "Set2"))(nrow(X)))
#
#     cols <- suppressWarnings(colorRampPalette(colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"))(nrow(X)))
#     df$col <- rep(cols, length(which(fi==TRUE)))
#
#     #rename variable as Var2 if variable is in df
#     if("variable" %in% names(df)){
#       df$variable <- as.numeric(df$variable)
#       names(df)[names(df) == "variable"] <- "Var2"
#     }
#
#     #create Var1 if it doesn't already exist
#     if(!("Var1" %in% names(df))){
#       unique_values <- unique(df$col)
#
#       # Create the 'Var1' column based on the pattern
#       df$Var1 <- rep(1:length(unique_values), length.out = nrow(df))
#     }
#
#     p <- suppressWarnings(plotly::plot_ly(data = df,
#                                           x = ~Var2,
#                                           y = ~value,
#                                           color = ~I(col),
#                                           name = ~Var1,
#                                           hovertemplate = "%{x} ppm<extra></extra>"))
#     p <- suppressWarnings(layout(p = p ,xaxis = x, yaxis = y))
#
#     p <- suppressWarnings(plotly::add_lines(p = p))
#
#     return(p)
#   }
#   matplot(ppm[fi], t(X[, fi]), type = "l", xlim = rev(range(ppm[fi])),
#           xlab = "ppm", ylab = "Intensity",...)
#
# }
#
#
#
# metmatspec <- function (X, ppm, shift = c(0, 9.5), interactive = TRUE, ...)
# {
#   if (is.null(ppm)) {
#     ppm <- as.numeric(colnames(X))
#   }
#   # else {
#   #   if (!.check_X_ppm(X, ppm))
#   #     stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.")
#   # }
#   idx <- get_idx(shift, ppm)
#   if (interactive) {
#     df <- melt(X[, idx])
#     x <- list(title = "ppm", autorange = "reversed")
#     y <- list(title = "Intensity")
#
#     #cols <- suppressWarnings(colorRampPalette(colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"))(nrow(X)))
#
#     # cols <- suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(10,
#     #                                                                    "Set2"))(nrow(X)))
#
#     cols <- suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(8,
#                                                          "Set2"))(nrow(X)))
#     df$col <- rep(cols, length(idx))
#     p <- suppressWarnings(plot_ly(data = df,
#                                   x = ~Var2,
#                                   y = ~value,
#                                   color = ~I(col),
#                                   name = ~Var1,
#                                   hovertemplate = "%{x} ppm<extra></extra>")
#     # )
#     #
#     # p <- suppressWarnings(layout(p = p ,xaxis = x, yaxis = y))
#     # p <- suppressWarnings(plotly::add_lines(p = p))
#                           %>%
#                             layout(xaxis = x, yaxis = y) %>%
#                             add_lines())
#     return(p)
#   }
#   matplot(ppm[idx],
#           t(X[, idx]),
#           type = "l",
#           xlim = rev(range(ppm[idx])),
#           xlab = "ppm",
#           ylab = "Intensity", ...)
# }
#
#
# #unprocessed example 1
# nmr<-local(get(load("~/OneDrive - Murdoch University/datasets/Colchicin/DataElements/hims_colchicin_PLA_HIMr02_PROF.PLASMA.CPMG.daE")))
#  ppm<-as.numeric(nmr@varName)
#  X<-nmr@.Data
# new<- matspec(X[1:3,],ppm = ppm ,roi = c(1,4.5),interactive = T)
# #old<- matspecOLD(X[1:2,],ppm,roi = c(3.0,4.5),interactive = T)
# met<- metmatspec(X[1:3,],ppm,shift = c(1,4.5),interactive = T)
#
# metabom8::matspec(X[1:3,],ppm,shift = c(1,4.5),interactive = T)
#
#
#
# #unprocessed example 2
#  noesy<-local(get(load("~/Documents/LucyDatasets/covid19_mauritius_SER_COVr22_PROF.PLASMA.NOESY.daE")))
#
#  SER_NOESY_data<-noesy@.Data
#  ppm<-as.numeric(noesy@varName)
#
#  matspec(X = SER_NOESY_data[1:3,], ppm = ppm, roi = c(3.0,4.5), interactive = T)
#  metmatspec(X = SER_NOESY_data[1:3,], ppm =ppm, shift = c(3.0,4.5), interactive = T)
# metabom8::matspec(X = SER_NOESY_data[1:3,], ppm =ppm, shift = c(3.0,4.5), interactive = T)
#
# #processed example 1
#  #ANNOTATION
#  #Take old annotation.dae and update it.
#
#  #old annotation.dae
#  ANNO<-local(get(load("~/OneDrive - Murdoch University/datasets/covid19/mauritius/DataElements/covid19_mauritius_ANNO.daE")))
#  ANNO<-ANNO@obsDescr[[1]]
#
#  #Keep only essential info
#  ANNO <- ANNO[,1:7]
#  class(ANNO$sourceID) #character
#  ANNO$sourceID <- as.integer(ANNO$sourceID)
#
#  #Replace old info with new
#  updated060623 <- read.csv("~/Documents/Mauritius/Deidentified_Metabolic_data_updated_06_06_23.csv")
#  class(updated060623$Survey.No.)
#  colnames(updated060623)[which(names(updated060623) == "Survey.No.")] <- "sourceID"
#
#  ANNO <- right_join(ANNO, updated060623, by = "sourceID")
#
#  #split into ser and pla
#  SER_ANNO<-ANNO[which(ANNO$sampleMatrixType=="SER"),]
#  PLA_ANNO<-ANNO[which(ANNO$sampleMatrixType=="PLA"),]
#
#  rm(updated060623)
#
#
#  #SER NMR NOESY
#
#  noesy<-local(get(load("~/Documents/LucyDatasets/covid19_mauritius_SER_COVr22_PROF.PLASMA.NOESY.daE")))
#
#  SER_NOESY_data<-noesy@.Data
#  ppm<-as.numeric(noesy@varName)
#
#  SER_NOESY_meta1<-noesy@obsDescr[[1]]
#  SER_NOESY_meta1$sourceID<-sapply(strsplit(SER_NOESY_meta1$UUID,"_"),"[",2)
#  SER_NOESY_info<-noesy@obsDescr[[5]]
#  #SER_NOESY_meta1<-merge(SER_NOESY_meta1,SER_NOESY_info, by = "path")
#  rm(SER_NOESY_info,noesy)
#
#  ##removals and matches
#
#  #post processing
#  res<-post_processing(SER_NOESY_data,ppm,type = TRUE, bc = TRUE, pqn = FALSE )
#  SER_NOESY_data<-res[[1]]
#  ppm<-res[[2]]
#  colnames(SER_NOESY_data)<-ppm
#
#  #remove LTRs
#  SER_NOESY_data<-SER_NOESY_data[grep("COV",SER_NOESY_meta1$sampleID),]
#  SER_NOESY_meta1<-SER_NOESY_meta1[grep("COV",SER_NOESY_meta1$sampleID),]
#
#  #change to data to df and attach sampleIDs
#  SER_NOESY_data <-as.data.frame(SER_NOESY_data)
#  SER_NOESY_data$sampleID <- SER_NOESY_meta1$sampleID
#
#  #deal with reruns
#  remove_id<-gsub(".1","",SER_NOESY_meta1$sampleID[grep(".",SER_NOESY_meta1$sampleID,fixed = TRUE)],fixed = TRUE)
#  SER_NOESY_data<-SER_NOESY_data[-which(SER_NOESY_meta1$sampleID %in% remove_id),]
#  SER_NOESY_meta1<-SER_NOESY_meta1[-which(SER_NOESY_meta1$sampleID %in% remove_id),]
#  SER_NOESY_meta1$sampleID<-gsub(".1","",SER_NOESY_meta1$sampleID,fixed = TRUE)
#  SER_NOESY_data$sampleID<-gsub(".1","",SER_NOESY_data$sampleID,fixed = TRUE)
#
#  #remove sample 299
#  SER_NOESY_data<-SER_NOESY_data[-which(SER_NOESY_meta1$sourceID=="299"),]
#  SER_NOESY_meta1<-SER_NOESY_meta1[-which(SER_NOESY_meta1$sourceID=="299"),]
#
#  ##Matching
#
#  #match serum anno with noesy annotation with sampleID
#  SER_NOESY_meta2 <- SER_ANNO[match(SER_NOESY_meta1$sampleID, SER_ANNO$sampleID),]
#
#  #merge all noesy anno
#  SER_NOESY_meta1$sourceID <-as.integer(SER_NOESY_meta1$sourceID)
#  SER_NOESY_meta <- merge(SER_NOESY_meta1, SER_NOESY_meta2, by=c("sampleID", "sourceID", "projectName", "cohortName", "sampleMatrixType"))
#
#  rm(SER_NOESY_meta1, SER_NOESY_meta2)
#
#  SER_NOESY_data<-SER_NOESY_data[, 1:38622]
#
#
#  matspec(X = SER_NOESY_data[1:3,], ppm = NULL, roi = c(0.5, 9), interactive = TRUE)
#
#
#
#  #wont work
#  metmatspec(X = SER_NOESY_data[1:3,], ppm = NULL, shift = c(0.5, 9), interactive = TRUE)
# #wont work
#  metabom8::matspec(X = SER_NOESY_data[1:3,], ppm = NULL, shift = c(0.5, 9), interactive = TRUE)
