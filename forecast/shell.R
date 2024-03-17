# (C) Michael Pokojovy (2024)

setwd(???)

set.seed(1)

## load stocks and legend
load(file = "../stocks.combined.RData")
stocks.legend = read.csv("legend.csv", header= TRUE, stringsAsFactors = FALSE)

UTC.time = stocks.combined.df$UTC.time
stocks.combined.df  = stocks.combined.df[, -1]
stocks.combined.mat = as.matrix(stocks.combined.df)

## load imputed log-differences

methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")
method = methods[3]

load(file = paste("../imp.historic.", method, ".RData", sep = ""))

m = imp.historic$m
p = ncol(imp.historic$data)
n = nrow(imp.historic$data)

#dlog.S.historic = sapply(1:m, function(i) mice::complete(imp.historic, i), simplify = "list")
#dlog.S.historic = list(a = mice::complete(imp.historic, 1), b = mice::complete(imp.historic, 2))

dlog.S.historic = list()
for (i in 1:m) {
   dlog.S.historic[[i]] = mice::complete(imp.historic, i)
}

rm(imp.historic)

## training and prediction time interval

UTC.time.train = UTC.time[which(UTC.time < as.POSIXlt("2019-07-01 07:00:00", tz = "UTC"))]

time.cut = length(UTC.time.train) # 2019-06-28 19:59:00 UTC

UTC.time.train = UTC.time.train[which(diff(UTC.time.train, lag = 1L) <= 1)]
UTC.time.pred  = seq(from = UTC.time[time.cut + 1], to = UTC.time[length(UTC.time)], by = "min")

#difftime(UTC.time[length(UTC.time)], UTC.time[time.cut + 1], units = "mins")

## key-to-legend convertion
KeyToLegend <- function(Key) {
  ind = which(stocks.legend$Key == Key)
  return(if (length(ind) < 1) c(NA, NA, NA) else stocks.legend[ind[1], ])
}

## compute prediction region
pred.region <- function(n.time.pred = 1440, alpha = 0.05, nrep = 100L) {
  n.pred.na = rep(0L, p)

  for (i in 1:p) {
    n.pred.na[i] = time.cut - max(which(!is.na(stocks.combined.mat[1:time.cut, i]))) + 1L
  }

  n.na = max(n.pred.na)

  UTC.last.not.na = UTC.time[time.cut - n.pred.na + 1L]

  UTC.time.na = seq(from = UTC.time[time.cut - n.na + 1L], to = UTC.time[time.cut], by = "min")

  len.pred.na = length(UTC.time.na)

  if (n.na > 1L) {
    lb.pred.na = matrix(as.numeric(NA), nrow = len.pred.na, ncol = p)
    ub.pred.na = matrix(as.numeric(NA), nrow = len.pred.na, ncol = p)
  } else {
    lb.pred.na = as.numeric(stocks.combined.mat[time.cut, ])
    ub.pred.na = as.numeric(stocks.combined.mat[time.cut, ])
  }

  if (n.na > 1L) {
    ## i = 1
    S = matrix(sapply(1:p, function(i) stocks.combined.mat[time.cut - n.pred.na[i] + 1L, i], simplify = TRUE),
               nrow = nrep, ncol = p, byrow = TRUE)

    lb.pred.na[1, ] = as.numeric(stocks.combined.mat[time.cut - n.na + 1L, ])
    ub.pred.na[1, ] = as.numeric(stocks.combined.mat[time.cut - n.na + 1L, ])

    ## i > 1
    for (i in 2:length(UTC.time.na)) {
      t = UTC.time.na[i]

      ind     = grep(t, UTC.time[1:time.cut])
      ind.imp = grep(t, UTC.time.train)

      I0  = which(UTC.last.not.na == t)
      I1  = which(UTC.last.not.na >= t)
      not.I1 = setdiff(1:p, I1)

      if (length(ind.imp) < 1L) {
        lb.pred.na[i, I1] = NA
        ub.pred.na[i, I1] = NA

        S[, not.I1] = S[, not.I1]*exp(t(replicate(nrep, as.numeric(dlog.S.historic[[sample.int(m, size = 1L)]][sample.int(length(UTC.time.train), size = 1L, replace = TRUE), not.I1]),
                                                  simplify = "matrix")))

        lb.pred.na[i, not.I1] = sapply(not.I1, FUN = function(dim) quantile(S[, dim], prob =     0.5*alpha, names = FALSE), simplify = "vector")
        ub.pred.na[i, not.I1] = sapply(not.I1, FUN = function(dim) quantile(S[, dim], prob = 1 - 0.5*alpha, names = FALSE), simplify = "vector")
      } else {
        lb.pred.na[i, I1] = as.numeric(stocks.combined.df[ind, I1])
        ub.pred.na[i, I1] = as.numeric(stocks.combined.df[ind, I1])

        S[, I0] = matrix(stocks.combined.mat[ind, I0], nrow = nrep, ncol = length(I0), byrow = TRUE)

        S[, not.I1] = S[, not.I1]*
                      exp(t(replicate(nrep, as.numeric(dlog.S.historic[[sample.int(m, size = 1L)]][ind.imp, not.I1]),
                                      simplify = "matrix")))

        lb.pred.na[i, not.I1] = sapply(not.I1, FUN = function(dim) quantile(S[, dim], prob =     0.5*alpha, names = FALSE), simplify = "vector")
        ub.pred.na[i, not.I1] = sapply(not.I1, FUN = function(dim) quantile(S[, dim], prob = 1 - 0.5*alpha, names = FALSE), simplify = "vector")
      }
    }

    UTC.time.pred = seq(from = UTC.time[time.cut] + 60, by = "min", length.out = n.time.pred)

    lb.pred = matrix(0.0, nrow = n.time.pred, ncol = p)
    ub.pred = matrix(0.0, nrow = n.time.pred, ncol = p)

    for (i in 1:n.time.pred) {
      
      S = S*exp(t(replicate(nrep, as.numeric(dlog.S.historic[[sample.int(m, size = 1L)]][sample.int(length(UTC.time.train), size = 1L, replace = TRUE), ]),
                            simplify = "matrix")))

      lb.pred[i, ] = sapply(1:p, FUN = function(dim) quantile(S[, dim], prob =     0.5*alpha, names = FALSE), simplify = "vector")
      ub.pred[i, ] = sapply(1:p, FUN = function(dim) quantile(S[, dim], prob = 1 - 0.5*alpha, names = FALSE), simplify = "vector")
    }
  }

  return(list(UTC.time.retro = UTC.time.na,   lb.retro = lb.pred.na, ub.retro = ub.pred.na,
              UTC.time.pred  = UTC.time.pred, lb.pred  = lb.pred,    ub.pred  = ub.pred))
}

#nrep = 2000L # 32.94361 hrs

nrep = 5000L

#n.time.pred = 1440*7
n.time.pred = as.numeric(difftime(UTC.time[length(UTC.time)], UTC.time[time.cut] + 60L, units = "mins"))
  
ptm = proc.time()
alpha = 0.05
reg = pred.region(n.time.pred = n.time.pred, alpha = alpha, nrep = nrep)
print(proc.time() - ptm)

## do plotting

plot.step = 60L

UTC.time.pred.plot = c(reg$UTC.time.retro, reg$UTC.time.pred)

for (i in 1:ncol(stocks.combined.df)) {
#for (i in 1:5) {
  key = colnames(stocks.combined.df)[i]
  leg = KeyToLegend(key)
  
  file.name = paste("fig/", key, ".pred.pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 4)
  
  par(mfcol = c(1, 1))
  par(mar = c(4.0, 4.0, 4.0, 0.5))
  
  stock.txt = paste("Stock ", key, ": ", leg$Symbol, " (", leg$Name, ")", sep = "")
  
  # Stock prices: prediction
  ymin = (1.0/1.05)*min(reg$lb.retro[, i], reg$lb.pred[, i], stocks.combined.mat[, i], na.rm = TRUE)
  ymax = (1.0*1.05)*max(reg$ub.retro[, i], reg$ub.pred[, i], stocks.combined.mat[, i], na.rm = TRUE) 
  
  plot(c(min(UTC.time), max(UTC.time)), c(NA, NA), ylim = c(ymin, ymax), 
       xlab = "Time", ylab = "Stock price", main = c(stock.txt, paste(100*(1 - alpha), "% prediction region", sep = "")), pch = NA)
  
  I = seq(from = 1L, to = length(UTC.time.pred.plot), by = plot.step)
  x = UTC.time.pred.plot; x = x[I]
  y1 = c(reg$lb.retro[, i], reg$lb.pred[, i]); y1 = y1[I]
  y2 = c(reg$ub.retro[, i], reg$ub.pred[, i]); y2 = y2[I]
  polygon(c(x, rev(x)), c(y1, rev(y2)), 
          col = "slategray1", border = NA) #"slateblue4"
  
  lines(x, y1, col = "slateblue4")
  lines(x, y2, col = "slateblue4")
  
  I = seq(from = 1L, to = nrow(stocks.combined.mat), by = plot.step/4)
  lines(UTC.time[I], stocks.combined.mat[I, i], lwd = 1)
  
  abline(v = UTC.time[time.cut], col = "red", lwd = 2)
  
  grDevices::dev.off()
}

try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)
