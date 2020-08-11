library(magrittr)
library("dplyr")  
library(ggplot2)
abia <- read.csv('ABIA.csv')

#line plot month vs count of delayed
  #plot avg delay time per month per airline
military.expected <- abia$CRSArrTime
abia$ExpArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M ')
military.actual <- abia$ArrTime
abia$ActArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M ')
#QUESTION - what time zone is this in 
#length of delay
abia$delaytime <- abia$ActualElapsedTime - abia$CRSElapsedTime

AvgArrDelay <- abia %>%
  group_by(Month, UniqueCarrier) %>%
  summarize(avgArrDelay = mean(ArrDelay, na.rm = TRUE))
ggplot(data=AvgArrDelay, aes(x=Month, y=avgArrDelay, group=UniqueCarrier)) +
  geom_line()+
  geom_point()

