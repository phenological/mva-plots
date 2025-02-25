
#' Subset Optimization by reference matching (STORM)
#'
#'
#' Step 1 Subset selection
#' Step 2 STOCSY of subset
#'   - Pearson correlation of each subset spectrum with the reference (reference a.k.a driver, chemical shift)
#'   - calculate corresponding p-value by transforming the correlation into the t-statistics with n-2 degrees of freedoms
#'   - exclude spectra subsets that have negative correlation with the reference
#'   - from the remaining spectra subset with the lowest p-values are chosen to be in the subset
#' Step 3) reference updating
#'
#' @param n expected number of spectra subset with hidden signal (signal of interest)
#' @param b expected half signal width data points
#' @param q predefined p-value threshold for refernce selection (0<q<=1, q<0.05 is significant).
#'
#'
#' Iteratively learns the true reference by repeating a procedure to find the most highly correlated spectra
#' and updating the reference multiple times.
#' It converges when a subset of samples has been found twice.
#'
#' @references Posma et.al (2012) dx.doi.org/10.1021/ac302360v| Anal.Chem. 2012, 84, 10694−10701
#'

#@examples
#storm(X = Xb, ppm = ppm_bin,roi = c(7.52,7.58),calibrate = TRUE)  # hippurate
#storm(X = Xb, ppm = ppm_bin,  b=5, q=0.01, idx_ref=NULL,roi = c(4.02,4.04),calibrate = FALSE)
#storm(X = Xb, ppm = ppm_bin,roi = c(1.13,1.16),calibrate = FALSE)


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


