# (C) Andrews T. Anum (2024)

daily.maxima <- function(err1, err2, err3, err4, err.typ = c("MSE", "MAPE"), predtimescale){
  
  
  start.time = predtimescale[1]
  current.day = stringr::str_split_i(start.time, " ", 1)
  end.time = paste(current.day, "19:59:00", "UTC", sep = " ")
  end.time.ind = which( as.POSIXlt(stringr::str_split_i(end.time, "UTC", 1), tz = "UTC" ) == predtimescale )
  
  first.fd = predtimescale[end.time.ind + 1]
  
  all.rem.day = seq(from = first.fd , to = predtimescale[length(predtimescale)], by = "1 day")
  
  day.maxima.ind = numeric(length = length(all.rem.day) + 2)
  day.maxima.ind[1] = 1; day.maxima.ind[length(day.maxima.ind)] = length(predtimescale)
  
  for (i in 1:length(all.rem.day) ) {
    predTimeSaved.ind = which(as.POSIXlt(stringr::str_split_i(all.rem.day[i], "UTC", 1), tz = "UTC" ) == predtimescale)
    ident = identical(predTimeSaved.ind, integer(0))
    
    if(!ident){
      day.maxima.ind[i+1] = predTimeSaved.ind
    }
    
  }
  
  day.maxima.ind = day.maxima.ind[which(day.maxima.ind > 0)]
  day.maxima.ind = unique(day.maxima.ind)
  
  gbm = numeric(length = length(day.maxima.ind)-1 )
  normal = numeric(length = length(day.maxima.ind)-1)
  stu = numeric(length = length(day.maxima.ind)-1)
  laplace = numeric(length = length(day.maxima.ind)-1) 
  
  for (j in 1:(length(day.maxima.ind) - 1)) {
    gbm[j]     = max( err1[day.maxima.ind[j]:day.maxima.ind[j+1]], na.rm = TRUE)
    normal[j]  = max( err2[day.maxima.ind[j]:day.maxima.ind[j+1]], na.rm = TRUE)
    stu[j]     = max( err3[day.maxima.ind[j]:day.maxima.ind[j+1]], na.rm = TRUE)
    laplace[j] = max( err4[day.maxima.ind[j]:day.maxima.ind[j+1]], na.rm = TRUE)
  }
  
  #x.axis = c(predtimescale[1], all.rem.day) #with weekends# does not capture beyond 07:00:00 UTC on the last day
  x.axis = predtimescale[day.maxima.ind] #withoout weekends
  
  gbm.inf = which(gbm == -Inf | gbm == Inf) #when all NA's, lines 37:40 return -Inf
  #normal.inf = which(normal == -Inf | gbm == Inf)
  
  inf.ind = identical(gbm.inf, integer(0))
  
  if(!inf.ind){
    gbm     = gbm[-gbm.inf]
    normal  = normal[-gbm.inf]
    stu     = stu[-gbm.inf]
    laplace = laplace[-gbm.inf]
    x.axis = x.axis[-gbm.inf]
  }
  
  
  ymin = (1.0/1.05)*min(c(gbm, normal, stu), na.rm = T)
  ymax = (1.0*1.05)*max(c(gbm, normal, stu), na.rm = T) 

  
  switch (err.typ,
    "MSE" = {plotname = "dailymax.MSE"
    file.name = paste("fig4/", plotname, ".pdf", sep = "")
    grDevices::pdf(file.name, width = 6, height = 6)
    
    plot(c(predtimescale[1], max(predtimescale)), c(NA, NA), ylim = c(ymin, ymax), 
         xlab = "Time", ylab = "MSE", main = "Daily maximum MSE", pch = NA)
    
    lines(x.axis[-c(length(x.axis))], gbm, type = "b", col = "black",  lty = 1, pch = 8)
    lines(x.axis[-c(length(x.axis))], normal, type = "b", col = "#8B7355",  lty = 2, pch = 5)
    lines(x.axis[-c(length(x.axis))], stu, type = "b", col = 	"#A2142F",  lty = 5, pch = 2)
    lines(x.axis[-c(length(x.axis))], laplace, type = "b", col = "#0072BD",  lty = 4, pch = 13)
    legend("topleft", c("Semiparametric", "Gaussian", "Student", "Laplace"), col = c("black", "#8B7355", 	"#A2142F", "#0072BD"),
           text.col = c("black", "black", "black", "black"), lty = c(1, 2, 5, 4), pch = c(8,5,2,13),
           merge = FALSE, bg = "white", trace=TRUE)
    
    grDevices::dev.off()},
    "MAPE" = {plotname = "dailymax.MAPE"
    file.name = paste("fig4/", plotname, ".pdf", sep = "")
    grDevices::pdf(file.name, width = 6, height = 6)
    
    plot(c(predtimescale[1], max(predtimescale)), c(NA, NA), ylim = c(ymin, ymax), 
         xlab = "Time", ylab = "MAPE (%)", main = "Daily maximum MAPE", pch = NA)
    
    lines(x.axis[-c(length(x.axis))], gbm, type = "b", col = "black",  lty = 1, pch = 8)
    lines(x.axis[-c(length(x.axis))], normal, type = "b", col = "#8B7355",  lty = 2, pch = 5)
    lines(x.axis[-c(length(x.axis))], stu, type = "b", col = 	"#A2142F",  lty = 5, pch = 2)
    lines(x.axis[-c(length(x.axis))], laplace, type = "b", col = "#0072BD",  lty = 4, pch = 13)
    legend("topleft", c("Semiparametric", "Gaussian", "Student", "Laplace"), col = c("black", "#8B7355", 	"#A2142F", "#0072BD"),
           text.col = c("black", "black", "black", "black"), lty = c(1, 2, 5, 4), pch = c(8,5,2,13),
           merge = FALSE, bg = "white", trace=TRUE)
    
    grDevices::dev.off()}
  )
}
