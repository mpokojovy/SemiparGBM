########## Install Packages#############
library(tidyr)
library(dplyr)
library(ggplot2)
library(crosstalk)
#library(deplyr)
library(tidyverse)
library(lubridate)  ######## Great for changing date time groups in a data frame
library(reshape2)
# search()
# theme_set(theme_minimal())

setwd(???)

## load legend
stocks.legend = read.csv("legend.csv", header= TRUE, stringsAsFactors = FALSE)

################## Import NYSE Unzip file Build Data frame #############################
filename="NYSE.zip"          # The name of the zipfile you want to read
columnname="Close"          # The column name you want to save
timecolumn="Gmt.time"       # The common row in all columns

dirname = paste("./",strsplit(filename,"[.]")[[1]][1],sep="")
#if (!(dir.exists(dirname))){
#  unzip(filename,exdir=dirname)
#}
files <- list.files(dirname)
NYSE = data.frame()
for(csv in files){
  filepath <- paste(dirname,"/",csv,sep="")
  if(nrow(NYSE)==0){
    NYSE <- read.csv(file=filepath, header=TRUE, sep=",")[c(timecolumn,columnname)]
    current_columnname = paste(strsplit(csv,"[.]")[[1]][1],columnname,sep="_")
    names(NYSE)[names(NYSE) == columnname] <- current_columnname
  } else {
    NYSE_csv <- read.csv(file=filepath, header=TRUE, sep=",")[c(timecolumn,columnname)]
    current_columnname = paste(strsplit(csv,"[.]")[[1]][1],columnname,sep="_")
    names(NYSE_csv)[names(NYSE_csv) == columnname] <- current_columnname
    NYSE <- merge(NYSE, NYSE_csv, by.x=timecolumn, by.y=timecolumn, all=T)
  }
}
#write.csv(NYSE, file = "./NYSEoutput.csv")

I = NULL

for (var in colnames(NYSE)[-1]) {
  ind = as.numeric(sub("_Close", "", sub("US", "", var)))
  I = c(I, ind)
}

NYSE[, 2:(length(I) + 1)] = NYSE[, order(I) + 1L]

#colnames(NYSE)[2:(length(I) + 1)] = paste("US", sprintf("%03d", 1:length(I)), "_Close", sep = "")
colnames(NYSE)[2:(length(I) + 1)] = paste("US", sort(I), sep = "")

#print(colnames(NYSE))

NYSE$Gmt.time <- dmy_hms(NYSE$Gmt.time)


############## Import LSE Unzip file Build Data Frame ###############
filename="LSE.zip"          # The name of the zipfile you want to read
columnname="Close"          # The column name you want to save
timecolumn="Gmt.time"       # The common row in all columns

dirname = paste("./",strsplit(filename,"[.]")[[1]][1],sep="")
#if (!(dir.exists(dirname))){
#  unzip(filename,exdir=dirname)
#}
files <- list.files(dirname)
LSE = data.frame()
for(csv in files){
  filepath <- paste(dirname,"/",csv,sep="")
  if(nrow(LSE)==0){
    LSE <- read.csv(file=filepath, header=TRUE, sep=",")[c(timecolumn,columnname)]
    current_columnname = paste(strsplit(csv,"[.]")[[1]][1],columnname,sep="_")
    names(LSE)[names(LSE) == columnname] <- current_columnname
  } else {
    LSE_csv <- read.csv(file=filepath, header=TRUE, sep=",")[c(timecolumn,columnname)]
    current_columnname = paste(strsplit(csv,"[.]")[[1]][1],columnname,sep="_")
    names(LSE_csv)[names(LSE_csv) == columnname] <- current_columnname
    LSE <- merge(LSE, LSE_csv, by.x=timecolumn, by.y=timecolumn, all=T)
  }
}
#write.csv(LSE, file = "./LSEoutput.csv")

colnames(LSE)[2:ncol(LSE)] = paste("UK", 1:(ncol(LSE) - 1L), sep = "")

LSE$Gmt.time <- dmy_hms(LSE$Gmt.time)

####################### Combined NYSE and LSE Stock Data Frame, Adjust time maintaining NA's ##############

combined_df <- merge(LSE, NYSE, by.x=timecolumn, by.y=timecolumn, all=T)
combined_df$Gmt.time = as.POSIXct(combined_df$Gmt.time,"2013-07-24 23:55:26")
colnames(combined_df)[1] = "UTC.time"

# Remove stocks w/o legend
I = NULL

for (ind in 2:ncol(combined_df)) {
  if (length(which(stocks.legend$Key == colnames(combined_df)[ind])) < 1) {
    I = c(I, ind)
  }
}

stocks.combined.df = combined_df[, -I]

save(file = "../stocks.combined.RData", stocks.combined.df)

stocks.combined.df

############# Build/Define Multiplot Function ###############################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
############### MultiPlot Combined Data Frame ################
############# Stock Plot Group 1 #############
df <- combined_df %>%
  select(Gmt.time, 2:79) %>% ### add stocks individually
  gather(key = "variable", value = "Close", -Gmt.time)

x1 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(80)) +
  ggtitle("London Stock Exchange")+ ## color codes
  theme_minimal()

df <- combined_df %>%
  select(Gmt.time, 80:158) %>%
  gather(key = "variable", value = "Close", -Gmt.time)
x2 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(80)) +
  ggtitle("New York Stock Exchange")+
  theme_minimal()
multiplot(x1, x2, cols=1)

########## Stock Plot Group 2 ################
df <- combined_df %>%
  select(Gmt.time, 2:79) %>% ### add stocks individually
  gather(key = "variable", value = "Close", -Gmt.time)

x1 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(80)) +
  ggtitle("London Stock Exchange")+ ## color codes
  theme_minimal()

df <- combined_df %>%
  select(Gmt.time, 158:236) %>%
  gather(key = "variable", value = "Close", -Gmt.time)
x2 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(100)) +
  ggtitle("New York Stock Exchange")+
  theme_minimal()
multiplot(x1, x2, cols=1)

######## Stock Plot Group 3 ##############
df <- combined_df %>%
  select(Gmt.time, 2:79) %>% ### add stocks individually
  gather(key = "variable", value = "Close", -Gmt.time)

x1 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(80)) +
  ggtitle("London Stock Exchange")+ ## color codes
  theme_minimal()

df <- combined_df %>%
  select(Gmt.time, 171:249) %>%
  gather(key = "variable", value = "Close", -Gmt.time)
x2 <- ggplot(df, aes(x = Gmt.time, y = Close)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = rainbow(100)) +
  ggtitle("New York Stock Exchange")+
  theme_minimal()
multiplot(x1, x2, cols=1)


