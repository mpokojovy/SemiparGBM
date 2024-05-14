# (C) Andrews T. Anum (2024)

regul.cov.mat <- function(S){
  
  d <- ncol(S)
  max.cond.k <- 50
  
  #T <- diag(d)
  T <- target.matrix(S)
  
  #regularize Target matrix
  decomp.T <- svd(T)
  D.onehalf <- (decomp.T$d)^(1/2)
  D.n.onehalf <- (decomp.T$d)^(-1/2)
  Q <- decomp.T$u
  Q.t <- decomp.T$v
  
  Su.matrix <- diag(D.n.onehalf)%*%Q.t%*%S%*%Q%*%diag(D.n.onehalf)
  eigen.vals <- eigen(Su.matrix)$values
  pos.eigen.vals <- abs(eigen.vals)
  
  rho.k <- (-max(pos.eigen.vals) + 50*min(abs(eigen.vals)))/( -49 + (-max(pos.eigen.vals)+  50*min(pos.eigen.vals)) )
  
  regmat <- (rho.k*T + (1 - rho.k)*S)
  
  
  return(regmat)
}


target.matrix <- function(xdata){
  
  robscale <- apply(xdata, 2, robustbase::Qn)
  #robscale <- sqrt(robscale)
  tar.mat <- diag(robscale)
  cond.num.T <- kappa(tar.mat, exact = TRUE) 
  
  if(cond.num.T > 50){
    threshd <- max(abs(robscale))/45
    robscale.update <- ifelse(robscale < threshd, threshd, robscale)
    tar.mat <- diag(robscale.update)
  }
  
  return(tar.mat)
}
