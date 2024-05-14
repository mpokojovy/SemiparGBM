# (C) Andrews T. Anum (2024)

assessment.Laplace <- function(stock.data, imp.logdiff, portfolio, nrep = 1L){
  
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
  
  MSEs.mat.laplace  = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  MAPEs.mat.laplace = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  
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
  
  Sigma.hat = stats$cov
  
  mean.hat  = stats$center
  
  spec = svd(Sigma.hat, nu = min(n1, p1), nv = min(n1,p1))
  root.Sigma  = spec$u %*% diag(sqrt(spec$d)) %*% t(spec$v)
  
  
  for (i in 2:(n.time.pred)) {
    
    predTimeSaved.ind = which(as.POSIXlt(stringr::str_split_i(UTC.time.pred[i], "UTC", 1), tz = "UTC" ) == UTC.time.rem)
    ident = identical(predTimeSaved.ind, integer(0))
    
    #nrep predictions at all times in UTC.time.pred
    for (j in 1:nrep) {
      
      data.size = 1 #change to number of rows you want
      x  = matrix(rnorm(data.size * ncol(Sigma.hat)), nrow = data.size)
      x  = x %*% root.Sigma
      exp.rand  = sqrt(rexp(data.size, 1))
      x = sweep(x, MARGIN = 1, STATS = exp.rand, FUN = "*")
      data.lap = sweep(x, MARGIN = 2, STATS = mean.hat, FUN = "+")
      
      S.pred[j, ] = S.pred[j, ]*exp(data.lap) #predictions
      
    }
    
    if(!ident){
      time.index = time.index + 1
      
      S.obs   = stocks.combined.mat[(time.cut + time.index), ]
      W.obs.t = sum(nu.i*S.obs)
      
      for (j in 1:nrep) {
        
        W.pred.t[j] = sum(nu.i*S.pred[j, ])
      }
      
      MSEs.mat.laplace[time.index] = MSE.score(pred = W.pred.t, obs = W.obs.t)
      MAPEs.mat.laplace[time.index] = MAPE.score(pred = W.pred.t, obs = W.obs.t)
    }
    
  }
  
  save(MSEs.mat.laplace, file = "MSEs.mat.laplace.Rdata")
  save(MAPEs.mat.laplace, file = "MAPEs.mat.laplace.Rdata")
  
  return(list(UTC.time.rem = UTC.time.rem))
}