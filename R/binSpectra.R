
#' Binning spectra
#'
#'
#' @param X full resolution spectra matrix
#' @param ppm parts per million
#' @param width interval in ppm. width must be larger than full resolution ppm interval
#' @param npoints total number of data points after binning npoints must be smaller than the length of ppm
#' You are required to specify either width or npoints NOT BOTH.
#'
#'
#' @examples
#' #Xb<-binSpectra(X = X, ppm = ppm, width = 0.001)
#' #ppm_bin<-as.numeric(colnames(Xb))
#'@export


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
