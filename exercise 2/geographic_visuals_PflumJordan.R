library(splitstackshape)
library(dplyr)
library(ggmap)

# Load Data
abiaData <- read.csv('ABIA.csv')
airportCodes <- read.csv('airportCodes.csv')

# Add Logitude and Latitude to ABIA Dataframe

# Rename Airport Codes iata_code and get unique codes
abiaDataDestination <- abiaData %>% select(Dest) %>% rename('iata_code' = Dest) %>% distinct()
abiaDataOrgin <- abiaData %>% select(Origin) %>% rename('iata_code' = Origin) %>% distinct()

# Get destination codes
abiaDataDestination <- merge(abiaDataDestination, airportCodes, by='iata_code') %>% 
  distinct(iata_code, .keep_all= TRUE) %>% 
  select(iata_code, coordinates) 

# Get Orgin Codes
abiaDataOrgin <- merge(abiaDataOrgin, airportCodes, by='iata_code') %>% 
  distinct(iata_code, .keep_all= TRUE) %>% 
  select(iata_code, coordinates)

# Split into Longitude and Latitude (Destination)
abiaDataDestination <- cSplit(abiaDataDestination, 'coordinates', sep=", ", type.convert=FALSE) %>% 
  rename('latitude' = coordinates_1,
         'longitude' = coordinates_2
  )
abiaDataDestination$latitude <- as.numeric(abiaDataDestination$latitude)
abiaDataDestination$longitude <- as.numeric(abiaDataDestination$longitude)

# Split into Longitude and Latitude (Orgin)
abiaDataOrgin <- cSplit(abiaDataOrgin, 'coordinates', sep=", ", type.convert=FALSE) %>% 
  rename('latitude' = coordinates_1,
         'longitude' = coordinates_2
  )
abiaDataOrgin$latitude <- as.numeric(abiaDataOrgin$latitude)
abiaDataOrgin$longitude <- as.numeric(abiaDataOrgin$longitude)


# Merge Dataframe back to original dataframe (Desitnation)
abiaDataDestination <- abiaDataDestination %>% rename('Dest' = iata_code)
abiaData <- merge(abiaData, abiaDataDestination, by='Dest') %>% rename('Dest_Longitude' = longitude,
                                                                       'Dest_Latitude' = latitude)

# Merge Dataframe back to original dataframe (Orgina)
abiaDataOrgin <- abiaDataOrgin %>% rename('Origin' = iata_code)
abiaData <- merge(abiaData, abiaDataOrgin, by='Origin') %>% rename('Origin_Longitude' = longitude,
                                                                   'Origin_Latitude' = latitude)
# Get Density of Departures
densityOfDepatrues <- abiaData %>% count(Dest)
abiaData <- merge(abiaData, densityOfDepatrues, by='Dest') %>% rename('Density_Destination' = n)

# Get Density of Arrivals
densityOfArrivals <- abiaData %>% count(Origin)
abiaData <- merge(abiaData, densityOfArrivals, by='Origin') %>% rename('Density_Arrivals' = n)





# Mapping
register_google(key = "AIzaSyDjF7I4JjprxlUM4ZFB_0AScyEME5HsI-s")

usa <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(bbox = usa, zoom = 4, maptype = "toner-lite")


# Number of Depatures per Airport
numDepatures <- abiaData %>% filter(Dest != 'AUS') %>% 
  select(Dest, Dest_Longitude, Dest_Latitude, Density_Destination) %>% 
  group_by(Dest) %>% 
  summarize('longitude' = mean(Dest_Longitude, na.rm = TRUE),
            'latitude' = mean(Dest_Latitude, na.rm = TRUE),
            'Number_Flight_Depatures' = mean(Density_Destination, na.rm = TRUE)
  )
ggmap(map) + geom_point(data=numDepatures, aes(x=longitude, y=latitude, color=Number_Flight_Depatures, size=Number_Flight_Depatures))


# Number of Arrivals per Airport
numArrivals <- abiaData %>% filter(Origin != 'AUS') %>% 
  select(Origin, Origin_Longitude, Origin_Latitude, Density_Arrivals) %>% 
  group_by(Origin) %>% 
  summarize('longitude' = mean(Origin_Longitude, na.rm = TRUE),
            'latitude' = mean(Origin_Latitude, na.rm = TRUE),
            'Number_Flight_Arrivals' = mean(Density_Arrivals, na.rm = TRUE)
  )
ggmap(map) + geom_point(data=numArrivals, aes(x=longitude, y=latitude, color=Number_Flight_Arrivals, size=Number_Flight_Arrivals))


# Average Arrival Delays per Airport
arrivalDealysPerAirport <- abiaData %>% filter(Origin != 'AUS') %>% 
  select(Origin, Origin_Longitude, Origin_Latitude, ArrDelay, Density_Arrivals) %>% 
  group_by(Origin) %>% 
  summarize('longitude' = mean(Origin_Longitude, na.rm = TRUE),
            'latitude' = mean(Origin_Latitude, na.rm = TRUE),
            'Arrival_Delay' = mean(ArrDelay, na.rm = TRUE),
            'Number_of_Flights' = mean(Density_Arrivals, na.rm = TRUE)
  )
ggmap(map) + geom_point(data=arrivalDealysPerAirport, aes(x=longitude, y=latitude, color=Arrival_Delay, size=Number_of_Flights))

# Average Departure Delays per Airport
depatureDealysPerAirport <- abiaData %>% filter(Dest != 'AUS') %>% 
  select(Dest, Dest_Longitude, Dest_Latitude, DepDelay, Density_Destination) %>% 
  group_by(Dest) %>% 
  summarize('longitude' = mean(Dest_Longitude, na.rm = TRUE),
            'latitude' = mean(Dest_Latitude, na.rm = TRUE),
            'Departure_Delay' = mean(DepDelay, na.rm = TRUE),
            'Number_of_Flights' = mean(Density_Destination, na.rm = TRUE)
  )
ggmap(map) + geom_point(data=depatureDealysPerAirport, aes(x=longitude, y=latitude, color=Departure_Delay, size=Number_of_Flights))

