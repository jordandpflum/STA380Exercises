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
  group_by(Month, Airline) %>%
  summarize(avgArrDelay = mean(ArrDelay, na.rm = TRUE))
#maybe subset for popular airlines
ggplot(AvgArrDelay, aes(x=Month, y=avgArrDelay, group=Airline)) +
  geom_line(aes(color=Airline))+
  geom_point(aes(color=Airline))
#---------------------------------------------------------------------------------------END

#most popular airlines (above median)
commonair <- data.frame(count(abia, vars = Airline)) #df of airlines and frequency in abia data frame
airline.counts <- table(abia$Airline) #table of counts similar to commonair
barplot(airline.counts, main="Frequency of Airlines",
        xlab="Airline")

airline.median <- median(commonair$n) #median frequency of airlines in abia
x.sub <- commonair %>%
  filter(n >= airline.median) #data frame of list of airlines who's frequency is > median
airlines <- x.sub$vars #list of popular airlines

popular.abia <- subset(abia, Airline %in% airlines) #data frame with info for each flight for popular airlines

#LINE PLOT MONTH VS AVERAGE ARRIVAL DELAY TIME BY AIRLINE FOR POPULAR AIRLINES -----------------------------DONE 
AvgArrDelay <- popular.abia %>%
  group_by(Month, Airline) %>%
  summarize(avgArrDelay = mean(ArrDelay, na.rm = TRUE))
ggplot(AvgArrDelay, aes(x=Month, y=avgArrDelay, group=Airline))+ 
  scale_fill_brewer(palette="Paired") +
  geom_line(aes(color=Airline))+
  geom_point(aes(color=Airline))+ 
  ggtitle("Average Arrival Delay Over Time (Year:2018)") +
  xlab("Airline") + 
  ylab("Average Arrival Delay")
#---------------------------------------------------------------------------------------END

#LINE PLOT MONTH VS AVERAGE DEPARTURE DELAY TIME BY AIRLINE FOR POPULAR AIRLINES -----------------------------DONE 
AvgDepDelay <- popular.abia %>%
  group_by(Month, Airline) %>%
  summarize(avgDepDelay = mean(DepDelay, na.rm = TRUE))
ggplot(AvgDepDelay, aes(x=Month, y=avgDepDelay, group=Airline))+ 
  scale_fill_brewer(palette="Paired") +
  geom_line(aes(color=Airline))+
  geom_point(aes(color=Airline))+ 
  ggtitle("Average Departure Delay Over Time (Year:2018)") +
  xlab("Airline") + 
  ylab("Average Departure Delay")
#---------------------------------------------------------------------------------------END

#Airline with the big peak
#most popular airlines (above median)
expjet <- abia[ which(abia$Airline=='ExpressJet'), ]

    #DEPARTURE DELAY------------------------------------------------
eAvgDepDelay <- expjet %>%
  group_by(Month, Airline) %>%
  summarize(eavgDepDelay = mean(DepDelay, na.rm = TRUE))
ggplot(eAvgDepDelay, aes(x=Month, y=eavgDepDelay, group=Airline))+ 
  geom_line(color="olivedrab4")+
  geom_point(color="olivedrab4")+ 
  ggtitle("Average Departure Delay Over Time (Year:2018)") +
  xlab("Airline") + 
  ylab("Average Departure Delay")

    #ARRIVAL DELAY------------------------------------------------
eAvgArrDelay <- expjet %>%
  group_by(Month, Airline) %>%
  summarize(eavgArrDelay = mean(ArrDelay, na.rm = TRUE))
ggplot(eAvgArrDelay, aes(x=Month, y=eavgArrDelay, group=Airline))+ 
  scale_fill_brewer(palette="Paired") +
  geom_line(color="olivedrab4")+
  geom_point(color="olivedrab4")+ 
  ggtitle("Average Arrival Delay Over Time (Year:2018)") +
  xlab("Airline") + 
  ylab("Average Arrival Delay")
#---------------------------------------------------------------------------------------END
