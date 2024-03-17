# (C) Andrews T. Anum (2024)

dist_xy <- function(xmean, ymean, xvar, yvar, xyvar, yxvar){
  
  dsq = sqrt( (xmean - ymean)^2 + xvar + yvar - xyvar - yxvar )
  
  return(dsq)
}

distance.mat <- function(X, use.FastImp = FALSE){
  p = ncol(X)
  out.mat = matrix(0, nrow = p, ncol = p, dimnames = list(colnames(X), colnames(X)) )
  
  if(use.FastImp){
    xysummary = list(center = colMeans(X, na.rm = TRUE),
                     cov = FastImputation::CovarianceWithMissing(X))
    
    for (i in 2:p) {
      for (j in 1:p) {
        if (i <= j)
        {
          next
        }
        out.mat[i, j] = dist_xy(xysummary$center[i],  xysummary$center[j], xysummary$cov[i,i],  xysummary$cov[j,j], xysummary$cov[i,j], xysummary$cov[j,i])
        out.mat[j, i] = out.mat[i, j]
      }
    }
    
  }else{
    xmeans = colMeans(X, na.rm = T)
    
    xcovs  = var(X, use = "pairwise.complete.obs")
    xcovs2  = var(X, na.rm = T)
    
    for (i in 2:p) {
      for (j in 1:p) {
        if (i <= j)
        {
          next
        }
        out.mat[i, j] = dist_xy(xmeans[i],  xmeans[j], xcovs2[i,i], xcovs2[j,j], xcovs[i,j], xcovs[j,i])
        out.mat[j, i] = out.mat[i, j]
      }
    }
  }
  return(out.mat)
}
