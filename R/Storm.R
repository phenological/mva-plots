
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

# devtools::load_all("~/git/phenological/mva-plots/R/matspec.R")
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
# matspec(X,ppm,roi = c(5.2,5.3))
# bin to 0.001 ppm width
seq_idx<-seq(ppm[1],ppm[length(ppm)],0.001)
bind_df<-list()
for(i in 1:c(length(seq_idx)-1)){
  idx<-which(ppm>=seq_idx[i] & ppm<seq_idx[i+1])
  if(length(idx)==1){
    df<-X[,idx]
  }else{
    df<-apply(X[,idx],1,median)
  }
  bind_df[[i]]<-df
  rm(idx,df)
}
rm(i)
X_bin<-do.call("cbind",bind_df)
ppm_bin<-seq_idx[-length(seq_idx)]
colnames(X_bin)<-ppm_bin
# matspec(X_bin,ppm_bin)
library(ggplot2)
ROI<-list(c(1.04,1.08),
          c(1.06,1.09))
for(i in length(ROI)){
  s0<-stocsy(x = ppm_bin,Y = X_bin,driver = 1.060,roi = ROI[[i]])
  
}

