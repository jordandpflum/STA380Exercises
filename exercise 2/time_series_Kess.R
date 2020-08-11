abia <- read.csv('ABIA.csv')

#line plot month vs count of delayed
  #plot avg delay time per month per airline
military.expected <- abia$CRSArrTime
abia$ExpArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')
military.actual <- abia$ArrTime
abia$ActArrTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", military.expected), 
                                                     format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')
#QUESTION - what time zone is this in 
#length of delay
time1<- abia$ExpArrTime
time2<- abia$ActArrTime
abia$arrival.delay <- difftime(time1, time2)

abia$arrival.delay <- difftime(time2, time1, units="mins")
