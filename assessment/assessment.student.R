# (C) Andrews T. Anum (2024)

assessment.student <- function(stock.data, imp.logdiff, portfolio, nrep = 1L){
  
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
  
  MSEs.mat.stu  = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  MAPEs.mat.stu = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  
  S0 = stocks.combined.mat[time.cut, ]
  nu.i   = portfolio$weight2/S0
  
  
  S.pred = matrix(sapply(1:p, function(i) stocks.combined.mat[time.cut, i], simplify = TRUE),
                  nrow = nrep, ncol = p, byrow = TRUE)# nrep by p
  
  W.pred.t = numeric(length = nrep)
  
  
  p1 = ncol(stocks.combined.mat)
  n1 = nrow(stocks.combined.mat) - 1L
  
  dlog.S = matrix(0.0, nrow = n1, ncol = p1)
  
  for (i in 1:p1) {
    dlog.S[, i] = diff(log(stocks.combined.mat[, i]), lag = 1L)
  }
  
  dlog.S.clean = na.omit(dlog.S)
  
  stats = list(center = colMeans(dlog.S, na.rm = TRUE),
               cov = FastImputation::CovarianceWithMissing(dlog.S))
  
  Sigma.hat = stats$cov
  
  mean.hat  = stats$center

  spec = svd(Sigma.hat, nu = min(n1, p1), nv = min(n1,p1))
  root.Sigma  = spec$u %*% diag(sqrt(spec$d)) %*% t(spec$v)
  
  #regularize cov matrix
  reg.Sigma.hat = regul.cov.mat(Sigma.hat)
  
  
  ##################################################################################
  #compute mah distances and perform ks test
  #Select best df (from ks test) to simulate student t samples
  
  mahal.ret = mahalanobis(dlog.S.clean, center = mean.hat, cov = reg.Sigma.hat, inverted = FALSE)
  
  test.ks = ks.fit(x = dlog.S.clean, mah = mahal.ret, p = c(2:p1))
 
  df_opt = test.ks$df
  p_opt = test.ks$p.opt
  
  ##################################################################################
  
  #simulate data with best df
  #make predictions
  for (i in 2:(n.time.pred)) {
    
    predTimeSaved.ind = which(as.POSIXlt(stringr::str_split_i(UTC.time.pred[i], "UTC", 1), tz = "UTC" ) == UTC.time.rem)
    ident = identical(predTimeSaved.ind, integer(0))
    
    #nrep predictions at all times in UTC.time.pred
    for (j in 1:nrep) {
      
      data.size = 1 #change to number of rows you want
      x  = matrix(rnorm(data.size * ncol(Sigma.hat)), nrow = data.size)
      normvec = sqrt(rowSums(x^2))
      x = sweep(x, MARGIN = 1, STATS = (normvec + 1E-9), FUN = "/")
      
      vec.scale = sqrt((df_opt - 2)/df_opt)*sqrt(p*rf(n = data.size, df1 = p_opt, df2 = df_opt))
      x = sweep(x, MARGIN = 1, STATS = vec.scale, FUN = "*")
      x  = x %*% root.Sigma
      
      data.stu = sweep(x, MARGIN = 2, STATS = mean.hat, FUN = "+")
      
      S.pred[j, ] = S.pred[j, ]*exp(data.stu) #predictions
      
    }
    
    if(!ident){
      time.index = time.index + 1
      
      S.obs   = stocks.combined.mat[(time.cut + time.index), ]
      W.obs.t = sum(nu.i*S.obs)
      
      for (j in 1:nrep) {
        
        W.pred.t[j] = sum(nu.i*S.pred[j, ])
      }
      
      MSEs.mat.stu[time.index] = MSE.score(pred = W.pred.t, obs = W.obs.t)
      MAPEs.mat.stu[time.index] = MAPE.score(pred = W.pred.t, obs = W.obs.t)
    }
    
  }
  
  save(MSEs.mat.stu, file = "MSEs.mat.stu.Rdata")
  save(MAPEs.mat.stu, file = "MAPEs.mat.stu.Rdata")
  
  return(list(UTC.time.rem = UTC.time.rem))
}
