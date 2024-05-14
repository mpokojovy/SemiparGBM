# (C) Andrews T. Anum (2024)

#setwd(???)

load(file = "../stocks.combined.RData")
stocks.comb = stocks.combined.df

methods = c("pmm", "sample", "cart", "mean", "norm", "ri", "norm.predict")
method = methods[3]
load(file = paste("../imp.historic.", method, ".RData", sep = ""))

portfolio.data = read.csv("newportfolio.csv", header= TRUE, stringsAsFactors = FALSE)

source("assessment.semipar.R")
source("assessment.gauss.R")
source("error.measures.R")

source("assessment.student.R")
source("student.fit.R")
source("regul.stats.R")

source("assessment.Laplace.R")

#for semiparametric
set.seed(20)
ptm = proc.time()
reg = assessment.semipar(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 500L)
print(proc.time() - ptm)


#for normal
set.seed(20)
ptm = proc.time()
reg.norm = assessment.gauss(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 500L)
print(proc.time() - ptm)


#for student
set.seed(20)
ptm = proc.time()
reg.stu = assessment.student(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 500L)
print(proc.time() - ptm)


#for Laplace
set.seed(20)
ptm = proc.time()
reg.lap = assessment.Laplace(stock.data = stocks.comb, imp.logdiff = imp.historic, portfolio = portfolio.data, nrep = 500L)
print(proc.time() - ptm)

#for semiparametric
load(file = "MSEs.mat.Rdata")
load(file = "MAPEs.mat.Rdata")

#for normal
load(file = "MSEs.mat.normal.Rdata")
load(file = "MAPEs.mat.normal.Rdata")

#for student
load(file = "MSEs.mat.stu.Rdata")
load(file = "MAPEs.mat.stu.Rdata")

#for Laplace
load(file = "MSEs.mat.laplace.Rdata")
load(file = "MAPEs.mat.laplace.Rdata")



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

plot(reg$UTC.time.rem[-c(1)], MAPEs.mat[-c(length(MAPEs.mat))]*100.0, 
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

plot(reg.norm$UTC.time.rem[-c(1)], MAPEs.mat.normal[-c(length(MAPEs.mat.normal))]*100, 
     ylab = "MAPE (%)", xlab = "Time", main = c(paste("MIAPE:", round(sum(MAPEs.mat.normal, na.rm = T),4), sep = "")) )

grDevices::dev.off()

#for student

file.name = paste("fig/", "MSE.stu", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.stu$UTC.time.rem[-c(1)], MSEs.mat.stu[-c(length(MSEs.mat.stu))], 
     ylab = "MSE", xlab = "Time", main = c(paste("MISE:", round(sum(MSEs.mat.stu, na.rm = T),4), sep = "")) )

grDevices::dev.off()


file.name = paste("fig/", "MAPE.stu", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.stu$UTC.time.rem[-c(1)], MAPEs.mat.stu[-c(length(MAPEs.mat.stu))]*100, 
     ylab = "MAPE (%)", xlab = "Time", main = c(paste("MIAPE:", round(sum(MAPEs.mat.stu, na.rm = T),4), sep = "")) )

grDevices::dev.off()



#for Laplace

file.name = paste("fig/", "MSE.laplace", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.lap$UTC.time.rem[-c(1)], MSEs.mat.laplace[-c(length(MSEs.mat.laplace))], 
     ylab = "MSE", xlab = "Time", main = c(paste("MISE:", round(sum(MSEs.mat.laplace, na.rm = T),4), sep = "")) )

grDevices::dev.off()


file.name = paste("fig/", "MAPE.laplace", ".pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 4)

plot(reg.lap$UTC.time.rem[-c(1)], MAPEs.mat.laplace[-c(length(MAPEs.mat.laplace))]*100, 
     ylab = "MAPE (%)", xlab = "Time", main = c(paste("MIAPE:", round(sum(MAPEs.mat.laplace, na.rm = T),4), sep = "")) )

grDevices::dev.off()


#daily max errors
source("daily.maxima.R")

daily.maxima(err1 = MSEs.mat, err2 = MSEs.mat.normal, err3 = MSEs.mat.stu, err4 = MSEs.mat.laplace, err.typ = "MSE", predtimescale = UTC.time.rem)

daily.maxima(err1 = 100.0*MAPEs.mat, err2 = 100.0*MAPEs.mat.normal, err3 = 100.0*MAPEs.mat.stu, err4 = 100.0*MAPEs.mat.laplace, err.typ = "MAPE", predtimescale = UTC.time.rem)
