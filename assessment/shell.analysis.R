# (C) Andrews T. Anum (2024)

setwd(???)

source("distanceMat.R")

load(file = "../stocks.combined.RData")

stocks.combined.df1 = stocks.combined.df[-c(1)]
m = nrow(stocks.combined.df1)
p = ncol(stocks.combined.df1)

dlog.S.historic = matrix(0, nrow = (m-1), ncol = p)
for (i in 1:p) {
  dlog.S.historic[,i] = log(stocks.combined.df1[2:m, i]/stocks.combined.df1[1:(m-1), i] )
}
colnames(dlog.S.historic) = colnames(stocks.combined.df1)


####################################
##US/UK density plots
####################################

UK.log.diffs = dlog.S.historic[, 1:78]
US.log.diffs = dlog.S.historic[, 79:p]

ukx.range = range(UK.log.diffs, na.rm = T)
usx.range = range(US.log.diffs, na.rm = T)

all.x = c(usx.range, ukx.range)
all.xrange = range(all.x)

xden.uk = 60*24*252*colMeans(UK.log.diffs, na.rm = T)
xden.us = 60*24*252*colMeans(US.log.diffs, na.rm = T)

uky.range = density(xden.uk, from = -5.5, to = 5.5)
usy.range = density(xden.us, from = -5.5, to = 5.5)

all.y = c(usy.range$y, uky.range$y)
all.yrange = range(all.y)

pdf("usukdensities.pdf", width = 5, height = 4)
plot(x = NULL, xlim = c(-5.5, 5.5), ylim = all.yrange, xlab= expression("return"~mu~"(in %)"), ylab = "density")
points(uky.range$x, uky.range$y,  col= "red", type = "l", lwd = 2, lty = 1)
points(usy.range$x, usy.range$y, col = "blue", type = "l", lwd = 2, lty = 2)
legend("topleft", c("US", "UK"), col = c("red", "blue"),
       text.col = c("black", "black"), lty = c(1, 2),
       merge = TRUE, bg = "white", trace=TRUE)
dev.off()  


######################################
####tsne with distance matrix
#####################################

#methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")
#method = methods[3]
#load(file = paste("imp.historic.", method, ".RData", sep = ""))

#m = imp.historic$m
#p = ncol(imp.historic$data)
#n = nrow(imp.historic$data)

#dlog.S.historic = list()
#for (i in 1:m) {
#dlog.S.historic[[i]] = mice::complete(imp.historic, i)
#}

#ind = sample.int(m, size = 1, replace = TRUE)

#stock.dist.covmiss <- distance.mat(dlog.S.historic[[ind]], use.OGK = TRUE)
#save(stock.dist.covmiss, file = "stock.dist.covmiss.Rda")
load(file = "stock.dist.covmiss.Rda")

stock.dist.covmiss.conv = as.dist(stock.dist.covmiss)

portfolio.data = read.csv("newportfolio.csv", header= TRUE, stringsAsFactors = FALSE)

set.seed(42)
tsne_out = Rtsne::Rtsne(stock.dist.covmiss.conv, dims = 2, perplexity = 30)

pdf("stocktsneviz.pdf", width = 5, height = 4)
plot(tsne_out$Y, t='p', xlab = "", ylab = "", ylim = (1.2/1.0)*range(tsne_out$Y[,2]), cex = 0.3, pch = c(rep(0,78),rep(1, 246)), 
     col = c(rep("red",78),rep("blue", 246)))
text(tsne_out$Y, labels= portfolio.data$ticker, cex=.3, pos = 1, col = c(rep("red",78),rep("blue", 246)) )
legend("topleft", c("US", "UK"), col = c("blue", "red"), pch = c(1, 0),
       text.col = c("blue", "red"), cex = 0.5,
       merge = FALSE, bg = "white", trace=TRUE)
dev.off()

