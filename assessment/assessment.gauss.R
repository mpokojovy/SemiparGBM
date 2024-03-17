# (C) Andrews T. Anum (2024)

assessment.gauss <- function(stock.data, imp.logdiff, portfolio, nrep = 1L){
  stocks.combined.df = stock.data
  
  UTC.time = stocks.combined.df$UTC.time
  stocks.combined.df  = stocks.combined.df[, -1]
  stocks.combined.mat = as.matrix(stocks.combined.df)
  
  imp.historic = imp.logdiff
  
  m = imp.historic$m
  p = ncol(imp.historic$data)
  n = nrow(imp.historic$data)
  
  dlog.S.historic = list()
  for (i in 1:m) {
    dlog.S.historic[[i]] = mice::complete(imp.historic, i)
  }
  
  rm(imp.historic)
  
  UTC.time.train = UTC.time[which(UTC.time < as.POSIXlt("2019-07-01 14:31:00", tz = "UTC"))]
  
  time.cut = length(UTC.time.train) # 2019-06-28 19:59:00 UTC
  
  UTC.time.train = UTC.time.train[which(diff(UTC.time.train, lag = 1L) <= 1)]
  
  UTC.time.pred  = seq(from = UTC.time[time.cut + 1], to = UTC.time[length(UTC.time)], by = "min")#can adjust to any delta
  
  n.time.pred = length(UTC.time.pred)
  
  UTC.time.rem = UTC.time[(time.cut + 1):length(UTC.time)]
  n.time.rem = length(UTC.time.rem)
  time.index = 0
  #n.time.rem should be less than length(UTC.time)
  
  MSEs.mat.normal  = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  MAPEs.mat.normal = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  
  S0 = stocks.combined.mat[time.cut, ]
  nu.i   = portfolio$weight2/S0
  
  
  S.pred = matrix(sapply(1:p, function(i) stocks.combined.mat[time.cut, i], simplify = TRUE),
                  nrow = nrep, ncol = p, byrow = TRUE)# nrep by p
  
  W.pred.t = numeric(length = nrep)
  
  
  p1 = ncol(stocks.combined.mat)
  n1 = nrow(stocks.combined.mat) - 1L
  
  dlog.S = matrix(0.0, nrow = n1, ncol = p1)
  
  for (i in 1:p) {
    dlog.S[, i] = diff(log(stocks.combined.mat[, i]), lag = 1L)
  }
  
  stats = list(center = colMeans(dlog.S, na.rm = TRUE),
               cov = FastImputation::CovarianceWithMissing(dlog.S))
  #stats = xysummary(dlog.S)
  
  Sigma.hat = stats$cov
  
  mean.hat  = stats$center
  
  decomp = svd(x = Sigma.hat, nu = min(n, p), nv = min(n, p))
  eigen.vals = decomp$d
  
  lambda = eigen.vals
  lambda = lambda[which(lambda > 0)]
  cum.lambda = cumsum(lambda)/sum(lambda)
  i.ast = min(which(cum.lambda >= 0.999))
  
  eigen.vals = pmax(eigen.vals, lambda[i.ast])
  D <- diag(sqrt(eigen.vals) )
  reg.sqrt.Sigma.hat = decomp$u %*% D %*% t(decomp$v)
  
  
  for (i in 2:(n.time.pred)) {
    
    predTimeSaved.ind = which(as.POSIXlt(stringr::str_split_i(UTC.time.pred[i], "UTC", 1), tz = "UTC" ) == UTC.time.rem)
    ident = identical(predTimeSaved.ind, integer(0))
    
    #nrep predictions at all times in UTC.time.pred
    for (j in 1:nrep) {
      #psamps = rnorm.samp(n = 1, p = p )
      psamps = matrix(rnorm(p), nrow = 1, ncol = p)
      data.norm = psamps%*%reg.sqrt.Sigma.hat + matrix(mean.hat, nrow = 1) #t(replicate(1, mean.hat, simplify = "vector")) 
      
      S.pred[j, ] = S.pred[j, ]*exp(data.norm) #predictions
      
    }
    
    if(!ident){
      time.index = time.index + 1
      
      S.obs   = stocks.combined.mat[(time.cut + time.index), ]
      W.obs.t = sum(nu.i*S.obs)
      
      for (j in 1:nrep) {
        
        W.pred.t[j] = sum(nu.i*S.pred[j, ])
      }
      
      MSEs.mat.normal[time.index] = MSE.score(pred = W.pred.t, obs = W.obs.t)
      MAPEs.mat.normal[time.index] = MAPE.score(pred = W.pred.t, obs = W.obs.t)
    }
    
  }
  
  save(MSEs.mat.normal, file = "MSEs.mat.normal.Rdata")
  save(MAPEs.mat.normal, file = "MAPEs.mat.normal.Rdata")
  
  return(list(UTC.time.rem = UTC.time.rem))
}
