# (C) Michael Pokojovy (2024)
setwd(???)

library("mice")
library("randomForest")

load("../stocks.combined.RData")

UTC.time = stocks.combined.df[, 1]

stocks.combined.df = stocks.combined.df[, -1]

I = which(diff(UTC.time, lag = 1L) <= 1)

p = ncol(stocks.combined.df)
n = length(I)

dlog.S = matrix(0.0, nrow = n, ncol = p)

for (i in 1:p) {
  dlog.S[, i] = diff(log(stocks.combined.df[, i]), lag = 1L)[I]
}

# Historic time horizon: TRAINING
UTC.time.train = UTC.time[which(UTC.time < as.POSIXlt("2019-07-01 07:00:00", tz = "UTC"))]
I = which(diff(UTC.time.train, lag = 1L) <= 1)

#time.cut = 37424

m = 5L
maxit = 5L

# methods = c("pmm", "midastouch", "sample", "cart", "rf", "mean", "norm", "norm.boot", "norm.predict", 
#             "2l.norm", "2l.lmer", "2l.pan", "2lonly.mean", "2lonly.norm", "2lonly.pmm")

methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")

#methods = c("rf")

for (method in methods) {
  cat(method, "\n")
  
  imp.historic = mice::mice(data = dlog.S[I, ], m = m, maxit = maxit, method = method, 
                            remove.collinear = FALSE, remove.constant = FALSE, printFlag = FALSE)
  
  save(file = paste("../imp.historic.", method, ".RData", sep = ""), imp.historic)
  
  print(imp.historic$loggedEvents)
  cat("\n\n")
}

# Sigma.hat.historic = cov(mice::complete(imp.historic))
# lambda = eigen(Sigma.hat.histori  c)$values
# 
# lambda     = lambda[which(lambda > 0)]
# cum.lambda = cumsum(lambda)/sum(lambda)
# 
# p. = min(which(cum.lambda >= 0.99))
# 
# plot(cum.lambda)
# abline(h = cum.lambda[p.])
# 
# cond.num = lambda[p.]/lambda[1]
# cat("condition number of reduced covariance = ", cond.num, "\n", sep = "")
# cat("reduced dimension: ", p., " out of ", p, "\n", sep = "")
# 
# save(file = "../imp.historic.RData", imp.historic)


# Sigma.hat = cov(dlog.S, use = "pairwise.complete.obs") # na.rm = TRUE)
# 
# lambda = eigen(Sigma.hat)$values
# 
# lambda     = lambda[which(lambda > 0)]
# cum.lambda = cumsum(lambda)/sum(lambda)
# 
# p. = min(which(cum.lambda >= 0.99))
# 
# plot(cum.lambda)
# abline(h = cum.lambda[p.])
# 
# cond.num = lambda[p.]/lambda[1]
# cat("condition number of reduced covariance = ", cond.num, "\n", sep = "")
# 
# res = mice::mice(dlog.S)
