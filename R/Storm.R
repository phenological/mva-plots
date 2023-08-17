
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
#' @param b :expected signal width (ideally full bandwidth of the metabolite NMR peak)
#' @param q : predefined p-value threshold for refernce selection.
#' 
#' 
#' Iteratively learns the true reference by repeating a procedure to find the most highly correlated spectra 
#' and updating the reference multiple times. 
#' It converges when a subset of samples has been found twice. 
#'
#' @references Posma et.al (2012) dx.doi.org/10.1021/ac302360v| Anal.Chem. 2012, 84, 10694−10701
#' 

# devtools::load_all("~/git/phenological/mva-plots/")
devtools::load_all("~/git/phenological/nmr-spectra-processing")
devtools::load_all("~/git/phenological/fusion")
da<-local(get(load("~/OneDrive - Murdoch University/datasets/gemma/DataElements/gemma_C1_URI_GMAr01_PROF.URINE.NOESY.daE")))
ppm<-as.numeric(da@varName)
An<-da@obsDescr[[1]]
da<-da@.Data
# remove LTRs
da<-da[which(An$sampleType=="sample"),]
An<-An[which(An$sampleType=="sample"),]
colnames(da)<-ppm
# No calibration/ alignment
X<-da
# remove urea (4.85 to 4.6 ppm)
idx<-which(ppm>4.6 & ppm<4.85)
Xc1 = X[,-idx]
ppm1 = ppm[-idx]

##    Remove TPS and lower ppm range
idx<-which(ppm1>=min(ppm) & ppm1<0.4)
Xc1 = Xc1[,-idx]
ppm1 = ppm1[-idx]

##    Remove higher ppm range with no signal
idx<-which(ppm1>9.5 & ppm1<=max(ppm))
X = Xc1[,-idx]
ppm = ppm1[-idx]
rm(idx,Xc1,ppm1)
colnames(X)<-ppm
# matspec(X,ppm,roi = c(5.2,5.3))
# baseline correction
X<-baselineCorrection(X)
colnames(X)<-ppm
data<-X
# matspec(X,ppm,roi = c(5.2,5.3))
# bin to 0.001 ppm width

binSpectra<-function (X, ppm, width = NULL, npoints = NULL){
  if (is.null(ppm) && (is.matrix(X) | is.data.frame(X)) && 
      !is.null(colnames(X))) {
    ppm <- as.numeric(colnames(X))
  }else {
    if (length(ppm)!=ncol(X)) 
      stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.\n")
  }
  if (!is.null(width) & !is.null(npoints)) {
    stop("Please specify only one width or npoints not both.\n")
  }
  if (is.null(width) & is.null(npoints)) {
    stop("Please define bin width in ppm or desired number of points.\n")
  }
  if (!is.null(width) & is.null(npoints)) {
    if (width <= abs(diff(ppm[seq_len(2)]))) {
      stop("Bin width must be greater than original ppm intervals (ppm[2]-ppm[1]).")
    }
    res = (ppm[2] - ppm[1])         # ppm interval == current resolution
    new_res = width/round(width/res)   
    step = round(width/res)         # how many data points you need to create one new binned spectra data point
    ppm_new = seq(min(ppm), max(ppm), by = new_res)
    iid = floor(length(ppm_new)/step) # how many data points in total after binning 
    ybin = rep(seq(iid), each = step) 
    Xb <- t(apply(X, 1, function(x, ppmt = ppm_new, ppm_fres = ppm, 
                                 yb = ybin) {
      sInter <- approxfun(ppm_fres, x)  # {stats} approxfun: Interpolation Functions
      s = sInter(ppmt)
      out = sapply(seq(max(yb)), function(i) {
        iidx = which(yb == i)
        sum(s[iidx])
      })
      return(out)
    }))
    ppm_bin = sapply(seq(max(ybin)), function(i) {
      iidx = which(ybin == i)
      mean(ppm_new[iidx])
    })
    colnames(Xb) <- ppm_bin
    rownames(Xb) <- rownames(X)
    return(Xb)
  }
  if (!is.null(npoints) & is.null(width)) {
    if (npoints >= length(ppm)) {
      stop("Input variable npoints cannot be larger or equal than length of ppm vector.")
    }
    ppm_bin <- seq(min(ppm), max(ppm), length.out = npoints)
    iid = floor(length(ppm)/npoints)
    ybin = rep(seq(npoints), each = iid)
    Xb <- t(apply(X, 1, function(s, yb = ybin) {
      out = sapply(seq(max(yb)), function(i) {
        iidx = which(yb == i)
        sum(s[iidx])
      })
      return(out)
    }))
    ppm_bin = sapply(seq(max(ybin)), function(i) {
      iidx = which(ybin == i)
      mean(ppm[iidx])
    })
    colnames(Xb) <- ppm_bin
    rownames(Xb) <- rownames(X)
    return(Xb)
  }
  return(NULL)
}

Xb<-binSpectra(X = X, ppm = ppm, width = 0.001)
ppm_bin<-as.numeric(colnames(Xb))



library(ggplot2)
matspec(Xb,ppm_bin,roi = c(1.5,2.5))


storm<-function(X, ppm, b=30, q=0.05, idx.refSpec=NULL, shift=c(1.117,1.25)){
  
  # center X, define reference index and reference spectrum
  Xc=scale_rcpp(Xb, center=TRUE, scale=0)
  Xc1<-scale(Xb,center = TRUE,scale = FALSE)
  plot(ppm_bin,Xb[1,])
  ref.idx=get.idx(range=shift, ppm)
  ref=X[idx.refSpec, ref.idx]
  
  # initialise
  subset=0
  subset1=1:nrow(X)
  i=1
  
  # perform storm
  while(length(which(!subset %in% subset1))>0){
    #print(table(subset %in% subset1))
    
    # correlation based subset selection
    subset=subset1
    Xr=X[subset, ref.idx]
    r=cor(t(Xr), ref)
    a=-abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval=2*pt(a, (length(r)-2))
    
    subset1=subset[r>0]
    pval=pval[r>0]
    index=order(pval)
    subset1=subset1[index]
    
    
    # Stocsy with driver=max intensity reference spectrum
    index=which.max(ref)
    r=cor(X[subset1, (ref.idx[index]-(b+1)):(ref.idx[index]+(b+1))], X[subset1,ref.idx[index]])
    co=cov(X[subset1, (ref.idx[index]-(b+1)):(ref.idx[index]+(b+1))], X[subset1,ref.idx[index]])
    
    # update reference spectrum and reference index
    a=-abs(r * sqrt((length(r)-2)/(1-r^2)))
    pval=2*pt(a,(length(r)-2))
    
    ref=co[pval<q & r>0]
    ref.idx=(ref.idx[index]-(b+1)):(ref.idx[index]+(b+1))
    ref.idx=ref.idx[pval<q & r>0]
    
  }
  
  return(subset1)
  
}