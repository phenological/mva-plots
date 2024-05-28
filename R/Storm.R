#' STORM
#'
#' Subset Optimization by reference matching
#' Summary of STORM workflow
#' Step 1: Subset selection
#'
#' Step 2: STOCSY of subset
#'   - Pearson correlation of each subset spectrum with the reference (reference a.k.a driver, chemical shift)
#'   - calculate corresponding p-value by transforming the correlation into the t-statistics with n-2 degrees of freedoms
#'   - exclude spectra subsets that have negative correlation with the reference
#'   - from the remaining spectra subset with the lowest p-values are chosen to be in the subset
#'
#' Step 3: Reference updating
#'
#' Iteratively learns the true reference by repeating a procedure to find the most highly correlated spectra
#' and updating the reference multiple times.
#' It converges when a subset of samples has been found twice (subset==subset1).
#'
#' @param X  Numeric matrix of spectra data where each row represents samples and column a chemical shift variable (ppm)
#' @param ppm numeric array of chemical shift variables. The length must match the number of columns in X.
#' @param r_idx int Reference spectra index.
#' @param roi numeric array, chemical shift ranges where the signal of interest would be the highest intensity
#' @param b expected half signal width data points (default is 20). Slightly larger values are recommended
#' @param q predefined p-value threshold for reference selection (0<q<=1, q<0.05 is significant).
#' @examples
#' \dontrun{
#' devtools::load_all("../nmr-spectra-processing/")
#' load("../mva-plots/data/NMR_1D_rat_urine_spectra.rda")
#' X_cal<-calibrateSpectra(ppm,X,ref = c("tsp"),rOref = c(0.92,0.94),cshift = 0.93)
#' s1<-storm(X = X_cal, ppm = ppm, refSpec_idx = 5,b = 10,q = 0.001,roi = c(0.878,0.883))
#' s0<-stocsy(ppm,Xcal[s1,],driver = 0.881)
#' stocsy(ppm,X_cal[s1,],driver = 0.881,roi = c(0.7,1.8))
#' matspec(X,ppm,roi = c(0.85,0.89),interactive = F)
#' matspec(X_cal[s1,],ppm,roi = c(0.85,0.89),interactive = F)
#' }
#' @return Vector of row indices that define the optimal subset.
#' @importFrom stats pt cor cov
#' @references Posma et.al (2012) dx.doi.org/10.1021/ac302360v| Anal.Chem. 2012, 84, 10694−10701
#' @export

storm<-function(X = NA,ppm = NA,refSpec_idx = NA,b = 20, q = 0.05, method = c("pearson"),roi = c(7.52,7.58)){

  if (is.null(ppm)) {
    ppm <- as.numeric(colnames(X))
  } else {
    if (length(ppm)!=ncol(X))
      stop("length of ppm and column length of X does not match")
  }

  if (is.na(refSpec_idx) |nrow(X)<refSpec_idx) {
    stop("Please define the reference spectra index that are within the X row range")
  }

  # extract reference spectra at ppm range of interest
  ppm_idx=which(ppm>roi[1] & ppm<roi[2])
  if (length(ppm_idx)==0) {
    stop("Please defind the roi that is within the ppm you have provided")
  }
  refSpec=X[refSpec_idx, ppm_idx]

  # Step 1: initialize the subsets
  subset=0
  subset1=1:nrow(X)
  i=1

  # iteration of step 1 ~ 3 until the number of subset reach the minimum
  while(length(which(!subset %in% subset1))>0){

    print(length(subset1))

    # Step 2a: calculate the Pearson correlation of each subset spectra (Xr) with reference (refSpec)
    subset=subset1
    Xr=X[subset, ppm_idx]
    r=cor(t(Xr), refSpec,method = method)

    # Step 2b: transform the correlation (r) into the t-statistic with length(subset)-2 degrees of freedom
    a=-abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval=2*pt(a, (length(r)-2))

    # Step 2c: exclude the subset and p-valus that have negative correlation(r)
    subset1=subset[r>0]
    pval=pval[r>0]

    # Step 2d: selecting the subset with the lowest p-value
    index=order(pval)
    subset1=subset1[index]

    # Step 2e: Stocsy of the subset using ppm that are maximum intensity in chosen reference spectra within the roi as a driver
    index=which.max(refSpec)
    r=cor(X[subset1, (ppm_idx[index]-(b+1)):(ppm_idx[index]+(b+1))], X[subset1,ppm_idx[index]])
    co=cov(X[subset1, (ppm_idx[index]-(b+1)):(ppm_idx[index]+(b+1))], X[subset1,ppm_idx[index]])

    a=-abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval=2*pt(a,(length(r)-2))

    # Step 3: updating reference spectrum and reference index
    refSpec=co[pval<q & r>0]
    ppm_idx=(ppm_idx[index]-(b+1)):(ppm_idx[index]+(b+1))
    ppm_idx=ppm_idx[pval<q & r>0]

  }

  return(subset1)
}
