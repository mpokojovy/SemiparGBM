# (C) Andrews T. Anum (2024)

assessment.semipar <- function(stock.data, imp.logdiff, portfolio, nrep = 1L){
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
  
  MSEs.mat  = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  MAPEs.mat = matrix(0.0, nrow =  n.time.rem, ncol = 1)
  
  
  S0 = stocks.combined.mat[time.cut, ]
  nu.i   = portfolio$weight2/S0
  
  S.pred = matrix(sapply(1:p, function(i) stocks.combined.mat[time.cut, i], simplify = TRUE),
                  nrow = nrep, ncol = p, byrow = TRUE)# nrep by p
  
  W.pred.t = numeric(length = nrep)#added
  
  for (i in 2:(n.time.pred)) {
    
    predTimeSaved.ind = which(as.POSIXlt(stringr::str_split_i(UTC.time.pred[i], "UTC", 1), tz = "UTC" ) == UTC.time.rem)
    ident = identical(predTimeSaved.ind, integer(0))
    
    #nrep predictions at all times in UTC.time.pred
    for (j in 1:nrep) {
      i.train = sample.int(n, size = 1L, replace = TRUE)
      i.m = sample.int(m, size = 1L)
      i.m.dlog.S = as.numeric(dlog.S.historic[[i.m]][i.train, ]) 
      
      S.pred[j, ] = S.pred[j, ]*exp(t(replicate(1, i.m.dlog.S, simplify = "vector"))) #predictions
    }
    
    
    if(!ident){
      time.index = time.index + 1
      
      S.obs   = stocks.combined.mat[(time.cut + time.index), ]
      W.obs.t = sum(nu.i*S.obs)
      
      
      for (j in 1:nrep) {
        
        W.pred.t[j] = sum(nu.i*S.pred[j, ])
        
      }
      
      MSEs.mat[time.index] = MSE.score(pred = W.pred.t, obs = W.obs.t)
      MAPEs.mat[time.index] = MAPE.score(pred = W.pred.t, obs = W.obs.t)
    }
    
  }
  save(MSEs.mat,  file = "MSEs.mat.Rdata")
  save(MAPEs.mat, file = "MAPEs.mat.Rdata")
  
  return(list(UTC.time.rem = UTC.time.rem))
}
