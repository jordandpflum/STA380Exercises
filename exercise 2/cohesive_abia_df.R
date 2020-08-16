#COPY OF KATELYN'S CODE TO MAKE SYSNONYMOUS DATA FRAME EVERYONE USES

library(ggmosaic)
library(ggplot2)
library(varhandle)
library(lubridate)
library(dplyr)

# add column for airline
airport <- read.csv("ABIA.csv",header=T, na.strings=c("",NA))
airlinecodes <- read.csv("airline_codes.csv")
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
abia$weeknum <- isoweek(abia$date)

# set colorblind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

