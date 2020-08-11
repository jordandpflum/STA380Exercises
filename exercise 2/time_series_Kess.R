library(magrittr)
library("dplyr")  
library(ggplot2)
abia_orig <- read.csv('ABIA.csv')
airlinecodes <- read.csv('airline_codes.csv')
abia <- abia_orig %>% left_join(airlinecodes,by="UniqueCarrier")


#MILITARY TIME TO REGULAR AM/PM TIME FORMAT
military.expected <- abia$CRSArrTime
abia$ExpArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M ')
military.actual <- abia$ArrTime
abia$ActArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M ')
#QUESTION - what time zone is this in 
#length of delay
abia$delaytime <- abia$ActualElapsedTime - abia$CRSElapsedTime


#LINE PLOT MONTH VS AVERAGE ARRIVAL DELAY TIME BY AIRLINE -----------------------------DONE 
AvgArrDelay <- abia %>%
  group_by(Month, UniqueCarrier) %>%
  summarize(avgArrDelay = mean(ArrDelay, na.rm = TRUE))
#maybe subset for popular airlines
ggplot(AvgArrDelay, aes(x=Month, y=avgArrDelay, group=UniqueCarrier)) +
  geom_line(aes(color=UniqueCarrier))+
  geom_point(aes(color=UniqueCarrier))
#---------------------------------------------------------------------------------------END

#most popular airlines
count(abia, vars = UniqueCarrier)




