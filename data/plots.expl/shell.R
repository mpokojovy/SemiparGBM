# (C) Michael Pokojovy (2024)

setwd(???)

## load stocks and legend
load(file = "../stocks.combined.RData")
stocks.legend = read.csv("legend.csv", header= TRUE, stringsAsFactors = FALSE)

time.cut = 37424

KeyToLegend <- function(Key) {
  ind = which(stocks.legend$Key == Key)
  return(if (length(ind) < 1) c(NA, NA, NA) else stocks.legend[ind[1], ])
}

for (i in 2:ncol(stocks.combined.df)) {
  key = colnames(stocks.combined.df)[i]
  leg = KeyToLegend(key)
  
  file.name = paste("fig/", key, ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  
  par(mfcol = c(2, 2))
  par(mar = c(4.0, 4.0, 2.0, 0.5))
  
  T.grid = stocks.combined.df$UTC.time
  S      = stocks.combined.df[, i]
  dlogS  = diff(log(S), 1L)
  
  #theta.hat = robustbase::s_Qn(na.omit(dlogS), mu.too = TRUE) # mu and sigma estimates
  theta.hat = c(mean(dlogS, na.rm = TRUE), sd(dlogS, na.rm = TRUE)) # mu and sigma estimates
  
  stock.txt = paste(key, ": ", leg$Symbol, " (", leg$Name, ")", sep = "")
  
  # Stock prices
  par(mfg = c(1L, 1L))
  plot(T.grid, S, pch = NA, 
       xlab = "Time", ylab = "Stock price", main = c(stock.txt, "Raw stock prices"))
  lines(stocks.combined.df$UTC.time, stocks.combined.df[, i], pch = NA)
  
  abline(v = T.grid[time.cut], col = "red", lwd = 2)
  
  # Stock price log-differences
  par(mfg = c(1L, 2L))
  plot(T.grid[-1], dlogS, pch = NA, 
       xlab = "Time", ylab = "Stock price log-differences", main = c(stock.txt, "Stock price log-differences"))
  points(T.grid[-1], dlogS)
  abline(h = -3.0*theta.hat[2], col = "blue")
  abline(h =  3.0*theta.hat[2], col = "blue")
  
  abline(v = T.grid[time.cut], col = "red", lwd = 2)
  
  # Stock price log-differences pdf
  par(mfg = c(2L, 1L))
  plot(density(na.omit(dlogS)), xlab = "Time", ylab = "Empirical pdf", main = c(stock.txt, "Stock price log-difference epdf"))
  x.range = range(na.omit(dlogS))
  x.grid = seq(from = x.range[1], to = x.range[2], length.out = 1000)
  lines(x.grid, dnorm(x.grid, mean = theta.hat[1], sd = theta.hat[2]), col = "blue")
  
  # Stock price log-differences acf
  par(mfg = c(2L, 2L))
  plot(acf(na.omit(dlogS), plot = FALSE, ci = 0.99), main = " ")
  title(main = c(stock.txt, "Stock price log-difference ACF"))
  
  grDevices::dev.off()
}

try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)
