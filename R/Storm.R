
#' Subset Optimization by reference matching (STORM)
#'
#'
#' Step 1) Subset selection
#' Step 2) STOCSY of subset
#'   - Pearson correlation of each subset spectrum with the reference (reference a.k.a driver, chemical shift)
#'   - calculate corresponding p-value by transforming the correlation into the t-statistics with n-2 degrees of freedoms
#'   - exclude spectra subsets that have negative correlation with the reference
#'   - from the remaining spectra subset with the lowest p-values are chosen to be in the subset
#' Step 3) reference updating
#'
#' @param n :expected number of spectra subset with hidden signal (signal of interest)
#' @param b :expected half signal width data points
#' @param q : predefined p-value threshold for refernce selection (0<q<=1, q<0.05 is significant).
#'
#'
#' Iteratively learns the true reference by repeating a procedure to find the most highly correlated spectra
#' and updating the reference multiple times.
#' It converges when a subset of samples has been found twice.
#'
#' @references Posma et.al (2012) dx.doi.org/10.1021/ac302360v| Anal.Chem. 2012, 84, 10694−10701
#'
#' @examples
#' storm(X = Xb, ppm = ppm_bin,roi = c(7.52,7.58),calibrate = TRUE)  # hippurate
#' storm(X = Xb, ppm = ppm_bin,  b=5, q=0.01, idx_ref=NULL,roi = c(4.02,4.04),calibrate = FALSE)
#' storm(X = Xb, ppm = ppm_bin,roi = c(1.13,1.16),calibrate = FALSE)
#'
#'
# devtools::load_all("~/git/phenological/mva-plots/")
# devtools::load_all("~/git/phenological/nmr-spectra-processing")
# devtools::load_all("~/git/phenological/fusion")
# da<-local(get(load("~/OneDrive - Murdoch University/datasets/gemma/DataElements/gemma_C1_URI_GMAr01_PROF.URINE.NOESY.daE")))
# ppm<-as.numeric(da@varName)
# An<-da@obsDescr[[1]]
# da<-da@.Data
# # remove LTRs
# da<-da[which(An$sampleType=="sample"),]
# An<-An[which(An$sampleType=="sample"),]
# colnames(da)<-ppm
# # No calibration/ alignment
# X<-da
# # remove urea (4.85 to 4.6 ppm)
# idx<-which(ppm>4.6 & ppm<4.85)
# Xc1 = X[,-idx]
# ppm1 = ppm[-idx]
#
# ##    Remove TPS and lower ppm range
# idx<-which(ppm1>=min(ppm) & ppm1<0.4)
# Xc1 = Xc1[,-idx]
# ppm1 = ppm1[-idx]
#
# ##    Remove higher ppm range with no signal
# idx<-which(ppm1>9.5 & ppm1<=max(ppm))
# X = Xc1[,-idx]
# ppm = ppm1[-idx]
# rm(idx,Xc1,ppm1)
# colnames(X)<-ppm
# # matspec(X,ppm,roi = c(5.2,5.3))
# # baseline correction
# X<-baselineCorrection(X)
# colnames(X)<-ppm
# data<-X

# binSpectra<-function (X, ppm, width = NULL, npoints = NULL){
#   if (is.null(ppm) && (is.matrix(X) | is.data.frame(X)) &&
#       !is.null(colnames(X))) {
#     ppm <- as.numeric(colnames(X))
#   }else {
#     if (length(ppm)!=ncol(X))
#       stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.\n")
#   }
#   if (!is.null(width) & !is.null(npoints)) {
#     stop("Please specify only one width or npoints not both.\n")
#   }
#   if (is.null(width) & is.null(npoints)) {
#     stop("Please define bin width in ppm or desired number of points.\n")
#   }
#   if (!is.null(width) & is.null(npoints)) {
#     if (width <= abs(diff(ppm[seq_len(2)]))) {
#       stop("Bin width must be greater than original ppm intervals (ppm[2]-ppm[1]).")
#     }
#     res = (ppm[2] - ppm[1])         # ppm interval == current resolution
#     new_res = width/round(width/res)
#     step = round(width/res)         # how many data points you need to create one new binned spectra data point
#     ppm_new = seq(min(ppm), max(ppm), by = new_res)
#     iid = floor(length(ppm_new)/step) # how many data points in total after binning
#     ybin = rep(seq(iid), each = step)
#     Xb <- t(apply(X, 1, function(x, ppmt = ppm_new, ppm_fres = ppm,
#                                  yb = ybin) {
#       sInter <- approxfun(ppm_fres, x)  # {stats} approxfun: Interpolation Functions
#       s = sInter(ppmt)
#       out = sapply(seq(max(yb)), function(i) {
#         iidx = which(yb == i)
#         sum(s[iidx])
#       })
#       return(out)
#     }))
#     ppm_bin = sapply(seq(max(ybin)), function(i) {
#       iidx = which(ybin == i)
#       mean(ppm_new[iidx])
#     })
#     colnames(Xb) <- ppm_bin
#     rownames(Xb) <- rownames(X)
#     return(Xb)
#   }
#   if (!is.null(npoints) & is.null(width)) {
#     if (npoints >= length(ppm)) {
#       stop("Input variable npoints cannot be larger or equal than length of ppm vector.")
#     }
#     ppm_bin <- seq(min(ppm), max(ppm), length.out = npoints)
#     iid = floor(length(ppm)/npoints)
#     ybin = rep(seq(npoints), each = iid)
#     Xb <- t(apply(X, 1, function(s, yb = ybin) {
#       out = sapply(seq(max(yb)), function(i) {
#         iidx = which(yb == i)
#         sum(s[iidx])
#       })
#       return(out)
#     }))
#     ppm_bin = sapply(seq(max(ybin)), function(i) {
#       iidx = which(ybin == i)
#       mean(ppm[iidx])
#     })
#     colnames(Xb) <- ppm_bin
#     rownames(Xb) <- rownames(X)
#     return(Xb)
#   }
#   return(NULL)
# }
# Xb<-binSpectra(X = X, ppm = ppm, width = 0.001)
# ppm_bin<-as.numeric(colnames(Xb))
#
# X = Xb
# ppm = ppm_bin

storm<-function(X, ppm, b=5, q=0.01, idx_ref=NULL, roi=NULL,calibrate = FALSE,message = TRUE,plot = TRUE){

  if(is.null(roi)){
    roi = c(1.13,1.16)
  }
  if(calibrate==TRUE){
    X <- calibrateSpectra(x = ppm, Y = X, rOref = roi, using = c(9.4,9.5))
  }
  # Xc : center Xb
  Xc <- scale(X,center = TRUE,scale = FALSE)
  #roi: pick the chemical shift range where you want to run STOCSY for (e.g., doublet ppm range)
  # roi<-c(1.13,1.16)

  #roi_idx: index of the ppm_bin that is close to the roi
  roi_idx <- which(ppm>roi[1] & ppm<roi[2])
  #idx_ref: reference spectra index
  idx_ref <- which.max(apply(Xc[,roi_idx],1,max))
  # reference spectra of the roi
  ref <- Xc[idx_ref,roi_idx]
  #Note: make sure the maximum of the ref is the driver you want to use for the STOCSY driver
  # b = 5  # b is number of point from the Stocsy driver to the base of the lorentzian fit
  # q = 0.05 # p-val threshold
  subset = 0
  subset1 = 1:nrow(Xc)
  i = 0
  while(length(which(!subset %in% subset1))>0){
    i<-i+1
    if(message==TRUE){
      cat("\n","Iteration",i,"\n","N = ",length(subset1),"\n")
    }

    subset = subset1
    Xr = Xc[subset,roi_idx] # create a subset of centered scale data
    r = cor(t(Xr),ref) # pearson correlation of the subset data with ref
    a = -abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval = 2 * pt(q = a,df = (length(r)-2))  # pt()The Student t Distribution from {stats}
    subset1 = subset[r>0]
    pval = pval[r>0]
    index = order(pval)
    subset1 = subset1[index]

    # stocsy with new subset1
    index = which.max(ref) #stocsy driver
    r = cor(Xc[subset1,(roi_idx[index]-(b+1)):(roi_idx[index]+(b+1))],Xc[subset1,roi_idx[index]])
    co = cov(Xc[subset1,(roi_idx[index]-(b+1)):(roi_idx[index]+(b+1))],Xc[subset1,roi_idx[index]])

    # update ref (reference spectrum) and roi_idx(reference index)
    a = -abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval = 2*pt(a,(length(r)-2))

    ref = co[c(r>0 & pval<q)]
    roi_idx = (roi_idx[index]-(b+1)):(roi_idx[index]+(b+1))
    roi_idx = roi_idx[c(r>0 & pval<q)]

    if(plot == TRUE){
      index = which.max(ref) #stocsy driver
      s0 <- stocsy(x = ppm, Y = Xc[subset1,], driver = ppm[roi_idx[index]])
      plot(s0)
    }

    # stocsy(x = ppm,Y = Xc[subset1,],roi = roi,driver = ppm_bin[roi_idx[index]])
    # stocsy(x = ppm,Y = Xc[subset1,],roi = c(3.5,4.6),driver = ppm_bin[roi_idx[index]])
    # stocsy(x = ppm,Y = Xc[subset1,],roi = c(7.0,8.5),driver = ppm_bin[roi_idx[index]])
  }
  return(subset1)
}


