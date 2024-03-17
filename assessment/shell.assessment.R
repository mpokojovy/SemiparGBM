# (C) Andrews T. Anum (2024) 

setwd(???)

load(file = "../stocks.combined.RData")
stocks.comb = stocks.combined.df

methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")
method = methods[3]
load(file = paste("../imp.historic.", method, ".RData", sep = ""))

portfolio.data = read.csv("newportfolio.csv", header= TRUE, stringsAsFactors = FALSE)

source("assessment.semipar.R")
source("assessment.gauss.R")
source("error.measures.R")

set.seed(20)
#for semiparametric
ptm = proc.time()
reg = assessment.semipar(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 5L)
print(proc.time() - ptm)

set.seed(20)
#for normal
ptm = proc.time()
reg.norm = assessment.gauss(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 5L)
print(proc.time() - ptm)

#for semiparametric
load(file = "MSEs.mat.Rdata")
load(file = "MAPEs.mat.Rdata")

#for normal
load(file = "MSEs.mat.normal.Rdata")
load(file = "MAPEs.mat.normal.Rdata")

dir.name = "fig"

if(!dir.exists(dir.name)){
  dir.create(dir.name)
}

#for semiparametric

file.name = paste("fig/", "MSE.semipar", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg$UTC.time.rem[-c(1)], MSEs.mat[-c(length(MSEs.mat))], 
     ylab = "MSE", xlab = "Time", main = c(paste("MISE:", round(sum(MSEs.mat, na.rm = TRUE),4), sep = "")) )

grDevices::dev.off()


file.name = paste("fig/", "MAPE.semipar", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg$UTC.time.rem[-c(1)], MAPEs.mat[-c(length(MAPEs.mat))], 
     ylab = "MAPE (%)", xlab = "Time", main = c(paste("MIAPE:", round(sum(MAPEs.mat, na.rm = TRUE),4), sep = "")) )

grDevices::dev.off()


#for normal

file.name = paste("fig/", "MSE.gauss", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.norm$UTC.time.rem[-c(1)], MSEs.mat.normal[-c(length(MSEs.mat.normal))], 
     ylab = "MSE", xlab = "Time", main = c(paste("MISE:", round(sum(MSEs.mat.normal, na.rm = T),4), sep = "")) )

grDevices::dev.off()


file.name = paste("fig/", "MAPE.gauss", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.norm$UTC.time.rem[-c(1)], MAPEs.mat.normal[-c(length(MAPEs.mat.normal))], 
     ylab = "MAPE (%)", xlab = "Time", main = c(paste("MIAPE:", round(sum(MAPEs.mat.normal, na.rm = T),4), sep = "")) )

grDevices::dev.off()


#daily max errors
source("daily.maxima.R")

daily.maxima(err1 = MSEs.mat, err2 = MSEs.mat.normal, err.typ = "MSE", predtimescale = reg$UTC.time.rem)

daily.maxima(err1 = MAPEs.mat, err2 = MAPEs.mat.normal, err.typ = "MAPE", predtimescale = reg$UTC.time.rem)

