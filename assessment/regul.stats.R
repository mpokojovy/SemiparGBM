# (C) Andrews T. Anum (2024)

regul.cov.mat <- function(S, max.cond.k = 50){
  
  d <- ncol(S)
  #max.cond.k <- 50
  
  #T <- diag(d)
  T <- target.matrix(S, max.cond.k)
  
  #regularize Target matrix
  decomp.T <- svd(T)
  D.onehalf <- (decomp.T$d)^(1/2)
  D.n.onehalf <- (decomp.T$d)^(-1/2)
  Q <- decomp.T$u
  Q.t <- decomp.T$v
  
  Su.matrix <- diag(D.n.onehalf)%*%Q.t%*%S%*%Q%*%diag(D.n.onehalf)
  eigen.vals <- eigen(Su.matrix)$values
  pos.eigen.vals <- abs(eigen.vals)
  max.eigen <- max(pos.eigen.vals)
  min.eigen <- min(pos.eigen.vals)
  
  gridpoints = seq(from = 0, to = 1, length.out = 200)
  
  for (i in 1:length(gridpoints)) {
    con.num.k = (gridpoints[i] + (1-gridpoints[i])*max.eigen) / (gridpoints[i] + (1-gridpoints[i])*min.eigen)
    if(con.num.k <= max.cond.k){
      rho.k = gridpoints[i]
      #print(rho.k)
      break
    }
  }
  
  regmat <- (rho.k*T + (1 - rho.k)*S)
  
  
  return(regmat)
}


target.matrix <- function(xdata, max.cond.k){
  
  robscale <- apply(xdata, 2, robustbase::Qn)
  #robscale <- sqrt(robscale)
  tar.mat <- diag(robscale)
  cond.num.T <- kappa(tar.mat, exact = TRUE) 
  
  if(cond.num.T > max.cond.k){
    threshd <- max(abs(robscale))/(0.9*max.cond.k)
    robscale.update <- ifelse(robscale < threshd, threshd, robscale)
    tar.mat <- diag(robscale.update)
  }
  
  return(tar.mat)
}
