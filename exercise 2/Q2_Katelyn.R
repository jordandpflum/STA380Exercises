#########################################################
# initial setup
#########################################################

library(ggmosaic)
library(ggplot2)
library(varhandle)
library(lubridate)
library(dplyr)

# add column for airline
airport <- read.csv("C:/Users/katel/OneDrive/Desktop/R/ABIA.csv",header=T, na.strings=c("",NA))
airlinecodes <- read.csv("C:/Users/katel/OneDrive/Desktop/R/airline_codes.csv")
abia <- airport %>% left_join(airlinecodes,by="UniqueCarrier")

# replace n/a cells with zero
abia[is.na(abia)] <- 0
abia[abia=='NULL'] <- 0

# recode cancellation code
abia$CancellationCode <- recode(abia$CancellationCode, 
                         "A"="Carrier",
                         "B"="Weather",
                         "C"="NAS")

abia$CancellationCode <- factor(abia$CancellationCode, 
                         levels= c("Carrier",
                                   "Weather",
                                   "NAS"))

# recode days of week
abia$DayOfWeek <- recode(abia$DayOfWeek, 
                         "7"="Sunday",
                         "1"="Monday",
                         "2"="Tuesday",
                         "3"="Wednesday",
                         "4"="Thursday",
                         "5"="Friday",
                         "6"="Saturday")

abia$DayOfWeek <- factor(abia$DayOfWeek, 
                         levels= c("Monday", 
                                   "Tuesday",
                                   "Wednesday", 
                                   "Thursday",
                                   "Friday", 
                                   "Saturday", 
                                   "Sunday"))

# recode months
abia$MonthName <- recode(abia$Month, 
                     "1"="Jan",
                     "2"="Feb",
                     "3"="Mar",
                     "4"="April",
                     "5"="May",
                     "6"="June",
                     "7"="July",
                     "8"="Aug",
                     "9"="Sept",
                     "10"="Oct",
                     "11"="Nov",
                     "12"="Dec")

abia$MonthName <- factor(abia$MonthName, 
                     levels= c("Jan",
                               "Feb",
                               "Mar",
                               "April",
                               "May",
                               "June",
                               "July",
                               "Aug",
                               "Sept",
                               "Oct",
                               "Nov",
                               "Dec"))

# create date column
abia$date <- as.Date(with(abia, paste(Year, Month, DayofMonth,sep="-")), "%Y-%m-%d")
# add column for weeknum
abia$weeknum <- isoweek(abia$date)

# set colorblind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#########################################################
# plot cancellation code frequency by weekday
#########################################################

# aggregate the data set by more than one factor
d2 = abia  %>% filter(abia$CancellationCode != 0) %>%
  group_by(CancellationCode, DayOfWeek) %>%
  summarize(Frequency=n())

# plot
ggplot(data = d2) + 
  geom_bar(mapping = aes(x=DayOfWeek, y=Frequency, fill=CancellationCode),
           position="dodge", stat='identity') +
  labs(x = "Weekday", fill = "Cancellation Code") +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  ggtitle("Cancellation Code Frequency by Weekday") +
  # center plot title
  theme(plot.title = element_text(hjust = 0.5))

#########################################################
# plot cancellation code frequency by month
#########################################################

# aggregate the data set by more than one factor
d3 = abia %>% filter(abia$CancellationCode != 0) %>%
  group_by(CancellationCode, MonthName) %>%
  summarize(Frequency=n())

# plot
ggplot(data = d3) + 
  geom_bar(mapping = aes(x=MonthName, y=Frequency, fill=CancellationCode),
           position="dodge", stat='identity') +
  labs(x = "Month", fill = "Cancellation Code") +
  scale_fill_manual(values=cbPalette) +
  theme_bw() +
  ggtitle("Cancellation Code Frequency by Month") +
  # center plot title
  theme(plot.title = element_text(hjust = 0.5))

#########################################################
# plot avg delay in min. over 2008 by the delay types
#########################################################

# group by weeknum, avg delays
d4 = abia %>%
  group_by(weeknum) %>%
  summarize(avgweatherdelay = mean(WeatherDelay),
            avgcarrierdelay = mean(CarrierDelay),
            avgNASdelay = mean(NASDelay),
            avgsecuritydelay = mean(SecurityDelay),
            avgaircraftdelay = mean(LateAircraftDelay))
d4

# set up month breaks in plot
month <- seq(as.Date("2008-01-01"), 
             as.Date("2008-12-01"), 
             by = "1 month")
month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

# plot
ggplot() + 
  # plot individual lines
  geom_line(data=d4,aes(y=avgweatherdelay,x= weeknum,color="#CC79A7"))+
  geom_line(data=d4,aes(y=avgcarrierdelay,x= weeknum,color="#E69F00"))+
  geom_line(data=d4,aes(y=avgNASdelay,x= weeknum,color="#009E73"))+
  geom_line(data=d4,aes(y=avgsecuritydelay,x= weeknum,color="#0072B2"))+
  geom_line(data=d4,aes(y=avgaircraftdelay,x= weeknum,color="#56B4E9"))+
  # add ticks for months
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label) +
  # get rid of grey background
  theme_bw() +
  # add title and center
  ggtitle("Average Delay (in min.) by Delay Type in 2008") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # add labels
  labs(x = "2008",y = "Average Delay (min.)") +
  # add legend
  scale_color_discrete(name = "Delay Type", 
                       labels = c("Security",
                                  "NAS",
                                  "Late Aircraft",
                                  "Weather",
                                  "Carrier"))

#######################################################################
# plot cancellations by type for top 10 airlines (# total flights)
#######################################################################

d6 = abia %>%
  group_by(Airline) %>%
  summarize(count=n(),.groups = 'drop') %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(n=10)

d6 <- as.data.frame(d6)

d7 = abia %>%
  filter(Airline %in% d6$Airline) %>%
  filter(CancellationCode!= "NA") %>%
  group_by(Airline, CancellationCode) %>%
  summarize(cancelcount = sum(Cancelled))
d7

ggplot(data = d7, aes(x = reorder(Airline,desc(cancelcount)), y = cancelcount, fill = CancellationCode)) + 
  geom_bar(stat='identity') +
  # get rid of grey background
  theme_bw() +
  # add title and center
  ggtitle("Cancellations for Top 10 Airlines by Type in 2008") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # add labels
  labs(x = "Airline",y = "Total Cancelled Flights",fill="Cancellation Code") 

#######################################################################
# plot avg carrier delay (in min.) for top 5 airlines (# total flights)
#######################################################################

d9 = abia %>%
  group_by(Airline) %>%
  summarize(count=n(),.groups = 'drop') %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(n=5)

d9 <- as.data.frame(d9)

# group by weeknum, avg delays
d8 = abia %>%
  filter(Airline %in% d9$Airline) %>%
  group_by(Airline,weeknum) %>%
  summarize(avgcarrdelay = mean(CarrierDelay))
d8

# set up month breaks in plot
month <- seq(as.Date("2008-01-01"), 
             as.Date("2008-12-01"), 
             by = "1 month")
month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

# plot
ggplot() + 
  # plot individual lines
  geom_line(data=d8,aes(y=avgcarrdelay,x= weeknum,color=Airline))+
 # add ticks for months
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label) +
  # get rid of grey background
  theme_bw() +
  # add title and center
  ggtitle("Average Carrier Delay (in min.) for Top 5 Airlines in 2008") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # add labels
  labs(x = "2008",y = "Average Carrier Delay (min.)")
