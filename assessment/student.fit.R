# (C) Michael Pokojovy and Andrews T. Anum (2024)

ks.fit <- function(x, mah, p = NULL, df.search = c(seq(from = 2.01,  to = 30.0,   length.out = 500L),
                                                   seq(from = 40.0,  to = 100.0,  length.out = 10L),
                                                   seq(from = 200.0, to = 1000.0, length.out = 10L))) {
  if (min(df.search <= 2.0)) {
    stop("Candidate df values need to be greater than 2.0.")
  }
  
  dists = rep(0.0, length(df.search))
  n.p = length(p)
  
  min.dist = rep(0.0, n.p)
  
  df.cand = rep(0.0, n.p)
  
  for (j.p in 1:n.p) {
    pj = p[j.p]
    
    for (i.df in 1:length(df.search)) {
      df = df.search[i.df]
      dists[i.df] = ks.test(df/(df - 2)*mah/pj, "pf", df1 = pj, df2 = df, alternative = "two.sided")$statistic
      
    }
    
    min.dist[j.p] = min(dists)
    
    df.cand[j.p] = df.search[which.min(dists)]
  }
  
  opt.dist.ind = which.min(min.dist)
  
  df = df.cand[opt.dist.ind]
  p.opt = p[opt.dist.ind]
  dist = min(min.dist)
  
  return(list(df = df, p.opt = p.opt, dist = dist))
}


mahal <- function(x, mu, iroot.Sigma){
  rowSums((sweep(x, 2L, mu, check.margin = TRUE) %*% iroot.Sigma)^2)
}


cvm.fit <- function(x, mah, p = NULL, df.search = c(seq(from = 2.01,  to = 30.0,   length.out = 500L),
                                           seq(from = 40.0,  to = 100.0,  length.out = 10L),
                                           seq(from = 200.0, to = 1000.0, length.out = 10L))) {
  if (min(df.search <= 2.0)) {
    stop("Candidate df values need to be greater than 2.0.")
  }
  
  
  dists = rep(0.0, length(df.search))
  n.p = length(p)
  
  min.dist = rep(0.0, n.p)
  
  df.cand = rep(0.0, n.p)
  
  for (j.p in 1:n.p) {
    pj = p[j.p]
    
    for (i.df in 1:length(df.search)) {
      df = df.search[i.df]
      dists[i.df] = goftest::cvm.test(df/(df - 2)*mah/pj, "pf", df1 = pj, df2 = df)$statistic     
    }
    
    min.dist[j.p] = min(dists)
    
    df.cand[j.p] = df.search[which.min(dists)]
  }
  
  opt.dist.ind = which.min(min.dist)
  
  df = df.cand[opt.dist.ind]
  p.opt = p[opt.dist.ind]
  dist = min(min.dist)
  
  return(list(df = df, p.opt = p.opt, dist = dist))
}
