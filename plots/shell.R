# (C) Michael Pokojovy (2024)

setwd(???)

## load stocks and legend
load(file = "../stocks.combined.RData")
stocks.legend = read.csv("legend.csv", header= TRUE, stringsAsFactors = FALSE)

UTC.time = stocks.combined.df$UTC.time
stocks.combined.df  = stocks.combined.df[, -1]

UTC.time.train = UTC.time[which(UTC.time < as.POSIXlt("2019-07-01 07:00:00", tz = "UTC"))]

I.dlog       = which(diff(UTC.time,       lag = 1L) <= 1)
I.dlog.train = which(diff(UTC.time.train, lag = 1L) <= 1)

time.cut = length(UTC.time.train) # 2019-06-28 19:59:00 UTC

## load imputed log-differences
methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")
method = methods[3]

load(file = paste("../imp.historic.", method, ".RData", sep = ""))

dlog.S.historic = mice::complete(imp.historic, 1)

## key-to-legend convertion
KeyToLegend <- function(Key) {
  ind = which(stocks.legend$Key == Key)
  return(if (length(ind) < 1) c(NA, NA, NA) else stocks.legend[ind[1], ])
}

## do plotting

plot.step = 60L

for (i in 1:ncol(stocks.combined.df)) {
#for (i in 1:5) {
  key = colnames(stocks.combined.df)[i]
  leg = KeyToLegend(key)
  
  file.name = paste("fig/", key, ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 12)
  
  par(mfcol = c(3, 2))
  par(mar = c(4.0, 4.0, 4.0, 0.5))
  
  T.grid = UTC.time
  S      = stocks.combined.df[, i]
  dlogS  = diff(log(S), 1L)
  
  #theta.hat = robustbase::s_Qn(na.omit(dlogS), mu.too = TRUE) # mu and sigma estimates
  theta.hat = c(mean(dlogS[I.dlog.train], na.rm = TRUE), sd(dlogS[I.dlog.train], na.rm = TRUE)) # mu and sigma estimates
  
  stock.txt = paste("Stock ", key, ": ", leg$Symbol, " (", leg$Name, ")", sep = "")
  
  # Stock prices
  par(mfg = c(1L, 1L))
  plot(T.grid, S, pch = NA, 
       xlab = "Time", ylab = "Stock price", main = c(paste("(a)", stock.txt), "Raw stock prices"))
  lines(UTC.time, stocks.combined.df[, i], pch = NA)
  
  abline(v = T.grid[time.cut], col = "red", lwd = 2)
  
  # Stock price log-differences
  par(mfg = c(1L, 2L))
  
  x = T.grid[I.dlog]
  I = seq(from = 1L, to = length(x), by = plot.step)
  
  plot(x[I], dlogS[I.dlog[I]], pch = NA, 
       xlab = "Time", ylab = "Stock price log-differences", main = c(paste("(b)", stock.txt), "(Selected) stock price log-differences"))
  
  points(x[I], dlogS[I.dlog[I]], col = "black")
  
  I.imp = which(is.na(dlogS[I.dlog.train]))
  I.imp = I.imp[seq(from = 1, to = length(I.imp), by = plot.step)]
  
  points(x[I.dlog.train[I.imp]], dlog.S.historic[I.imp, i], col = "grey")
  
  abline(h = -3.0*theta.hat[2], col = "blue")
  abline(h =  3.0*theta.hat[2], col = "blue")
  
  abline(v = T.grid[time.cut], col = "red", lwd = 2)
  
  # Stock price log-differences pdf
  par(mfg = c(2L, 1L))
  
  #I.na = which(is.na(dlogS[I]))
  I     = I.dlog.train[seq(from = 1, to = length(I.dlog.train), by = plot.step)]
  I.imp = seq(from = 1, to = nrow(dlog.S.historic), by = plot.step)
  
  plot(density(na.omit(dlogS[I])), xlab = "Stock price log-difference", ylab = "Empirical pdf", 
       main = c(paste("(c)", stock.txt), "Stock price log-difference EPDF"), lty = 1L)
  lines(density(dlog.S.historic[I.imp, i]), lty = 2L, col = "grey")
  x.range = range(na.omit(dlogS[I]))
  x.grid = seq(from = x.range[1], to = x.range[2], length.out = 500)
  lines(x.grid, dnorm(x.grid, mean = theta.hat[1], sd = theta.hat[2]), col = "blue", lwd = 2L)
  
  legend("topright", legend = c("pdf w/o imputation", "pdf w/ imputation", "fitted Gaussian pdf"),
         pch = c(NA, NA, NA),
         lty = c(1L, 2L, 1L),
         col = c("black", "black", "blue"), bg = "white", pt.cex = 1, cex = 1.0)
  
  # Stock price log-differences cdf
  par(mfg = c(2L, 2L))
  
  f1 = ecdf(na.omit(dlogS[I]))
  f2 = ecdf(dlog.S.historic[I.imp, i])
  
  x.range = range(na.omit(dlogS[I]))
  x.grid = seq(from = x.range[1], to = x.range[2], length.out = 500)
  
  plot(x.grid, f1(x.grid), xlab = "Stock price log-difference", ylab = "Empirical cdf",
       main = c(paste("(d)", stock.txt), "Historic stock price log-difference ECDF"), pch = NA, lty = 1L)
  lines(x.grid, f1(x.grid), lty = 1L)
  lines(x.grid, f2(x.grid), lty = 2L, col = "grey")
  
  lines(x.grid, pnorm(x.grid, mean = theta.hat[1], sd = theta.hat[2]), col = "blue", lwd = 2L)

  legend("bottomright", legend = c("cdf w/o imputation", "cdf w/ imputation", "fitted Gaussian pdf"),
         pch = c(NA, NA, NA),
         lty = c(1L, 2L, 1L),
         col = c("black", "black", "blue"), bg = "white", pt.cex = 1, cex = 1.0)
  
  # Stock price log-differences acf
  par(mfg = c(3L, 1L))
  plot(acf(na.omit(dlogS[I]), plot = FALSE, ci = 0.99), main = " ")
  title(main = c(paste("(e)", stock.txt), "Historic stock price log-difference ACF"))
  
  # Stock price log-differences pacf
  par(mfg = c(3L, 2L))
  plot(pacf(na.omit(dlogS[I]), plot = FALSE, ci = 0.99), main = " ")
  title(main = c(paste("(f)", stock.txt), "Historic stock price log-difference PACF"))
  
  grDevices::dev.off()
}

try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)
