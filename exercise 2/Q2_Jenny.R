library(mosaic)
library(dplyr)
library(readr)

setwd('C:/Users/jenni/OneDrive/Documents/GitHub/STA380/R')

airport <- read.csv("ABIA.csv",header=T, na.strings=c("",NA))
airlinecodes <- read.csv("airline_codes.csv")
abia <- airport %>% left_join(airlinecodes,by="UniqueCarrier")


d3 = abia  %>% filter(DepDelay >=30) %>%
  group_by(CRSDepTime) %>%
  summarize(Frequency=n())


qplot(d3$CRSDepTime,
      geom="histogram",
      binwidth = 50,  
      main = "Delay Frequency Based on Time of Day", 
      xlab = "Time of Day (hhmm)", 
      ylab = "# of Delays",
      fill=I('blue'),
      col=I("red"),
      alpha=I(.5))






