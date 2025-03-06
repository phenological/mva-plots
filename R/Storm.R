
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


#' Subset Optimization by Reference Matching
#' 
#' Identifies a subset of spectra presenting a feature, by iterative matching
#' starting from an approximate reference
#' 
#' @param ppm numeric vector, chemical shift scale
#' @param Y numeric matrix, spectra intensities
#' @param reference numeric vector, initial guess of the feature. This may come 
#' from a spectrum that presents the feature, a statistical model (e.g. loadings), 
#' possibly a signal model...
#' Note that only a small "fragment" containing the feature must be given
#' @param rOref numeric vector, endpoints of the chemical shift region spanned
#' by the reference
#' @param s number, threshold parameter for selecting a subset of spectra that
#' better matches the reference. This may be either a maximum number of spectra to select
#' (known as ns, see References), or a minimum correlation with the reference
#' @param q, number, significance threshold (p-value) to update the reference.
#' See References
#' @param b, broadness threshold for the reference, see References
#' @plot logical, should a \link{\code{stocsy}} of the subset (aka relic, see References) be printed? Default=TRUE
#' @details
#' After filtering points with stocsy correlation < q, gaps may appear.
#' In this implementation such gaps are filled by interpolation, to keep
#' a consistent chemical shift scale throughout the whole process.
#' @returns silently, a list containing: \describe{
#' \item{subset}{indices of the subset of spectra containing the feature}
#' \item{reference}{the optimized reference (intensities)}
#' \item{rOref}{the optimized rOref (chemical shift endpoints)}
#' \item{driver}{chemical shift of the summit of the optimized reference,
#' used for the relic}
#' 
#' }
#' @references \href{https://doi.org/10.1021/ac302360v}{Posma et.al (2012) dx.doi.org/10.1021/ac302360v| Anal.Chem. 2012, 84, 10694−10701}
#' @importFrom signal interp1

storm2 <- function(ppm,Y,reference,rOref,s=1/nrow(Y),q=0.05,b=diff(rOref) * 1.5,maxIt=100){
  fi <- crop(ppm,roi=rOref)
  ppmr <- ppm[fi]
  Yr <- Y[,fi]
  subsetl <- list()
  refl <- list()
  driverl <- list()
  rOrefl <- list()
  i <- 0
  repeat{
    i <- i + 1
    #Step 1: subset by correlation with the reference
    r <- apply(Yr,1,function(v) cor(v,reference,method="pearson"))
    if (s < 1){
      #Select r > s
      subset_i <- which(r > s)
    } else{
      #Select the s highest
      subset_i <- order(r,decreasing = TRUE)[1:s]
      #Filter out r < 0
      subset_i <- subset_i[r[subset_i] >= 0]
      # print(sort(r[subset_i]))
      # sort(r[-subset_i])
      # print(subset_i)
    }
    
    if (length(subset_i) == 0){
      smatplot(ppmr,Reduce(rbind,refl),main="References",label=1:i,legend="toplef")
      cat("Subset vanished")
      return(NULL)
    }
    
    #Step 2: stocsy the subset with the driver on the maximum of the reference
    #and roi = driver +-b. Done with cor.test rather than stocsy because we want p
    driver <- ppmr[which.max(reference)]
    roRef <- driver + b/2 * c(-1,1)
    ssy <- stocsy(ppm,Y[subset_i,],driver,roi=roRef,plot=F)
    #Step 3: filter points with p.cor < q to get updated reference
    fi <- ssy$p < q
    reference <- ssy$covar[fi]
    ppmr_gaps <- ssy$ppm[fi]
    rOref <- range(ppmr_gaps)
    #Step 3a: interpolate to fill gaps in the reference
    fi <- crop(ppm,roi=rOref)
    ppmr <- ppm[fi]
    Yr <- Y[,fi]
    # return(list(ppmr_gaps = ppmr_gaps,reference = reference,ppmr = ppmr))
    reference <- signal::interp1(ppmr_gaps,reference,ppmr,method="spline")
    
    print(subset_i)
    
    #Exit condition: subset previously observed
    if (i == maxIt | any(sapply(subsetl,function(x) setequal(subset_i,x)))){
      subsetl[[i]] <- subset_i
      refl[[i]] <- reference
      driverl[[i]] <- driver
      rOrefl[[i]] <- rOref
      #Tracing plots (for development?)
      smatplot(ppmr,Reduce(rbind,refl),main="References",label=1:i,legend="toplef")
      print(stocsy(ppm,Y[subset_i,],driver))
      # print(stocsy(ppm,Y[subset_i,],driver,roi=driver + b * c(-1,1)))
      return(list(subset=subset_i, reference=reference, rOref = rOref, driver = driver))
    }
    subsetl[[i]] <- subset_i
    refl[[i]] <- reference
    driverl[[i]] <- driver
    rOrefl[[i]] <- rOref
  }
}

#' Plot the relic from a STORM
#' 
#' @param ppm, numeric vector, chemical shift scale
#' @param Y, numeric matrix, spectra intensities
#' @param storm, list, \link{\{storm}} results
#' @returns A ggplot2 object
showRelic <- function(ppm,Y,storm,...){
  stocsy(ppm,Y[storm$subset,],storm$driver,...)
}
