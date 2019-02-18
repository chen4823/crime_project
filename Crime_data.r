
library(ggplot2)
library(ggmap)
getwd()
setwd('/Users/AngelinaChen/Downloads')
crime_data <- read.csv('Crime_data.csv')
crime_data
attach(crime_data)
geocoded <- data.frame(stringsAsFactors = FALSE)
crime_data[,"longitude"] <- NA
crime_data[,"latitude"] <- NA
crime_data["geoAddress"] <- NA
register_google('AIzaSyAvrl0VdbEWEBCTPHsPWMTgwjLTPN4kAj0')
crime_data$Address <- as.character(crime_data$Address) 
#loop through data sets 4029 data
for(i in 4001:4029)
{
  # Print("Working...")
  result <- geocode(paste(crime_data$Address[i],'Minneapolis','st.paul'), output = "latlona", source = "google")
  crime_data$longitude[i] <- as.numeric(result[1])
  crime_data$latitude[i] <- as.numeric(result[2])
  crime_data$geoAddress[i] <- as.character(result[3])
}

twin_cities<- get_map(location = c(lon = -93.236,lat = 44.977),zoom = 15)
university <- ggmap(twin_cities,legend = "topleft")
university + stat_density_2d(aes(x=longitude,y=latitude,alpha = ..level..,colour=..level..),size = 2, bins = 4, data = crime_data,geom='polygon')

browser()

#i <- 3518
#result <- geocode(paste(crime_data$Address[i],'Minneapolis','st.paul'), output = "latlona", source = "google")
#crime_data$longitude[i] <- as.numeric(result[1])
#crime_data$latitude[i] <- as.numeric(result[2])
#crime_data$geoAddress[i] <- as.character(result[3])
write.csv(crime_data,file = 'Geocoded crime data.csv')
crime_data[,'rdate'] <- ''
for(i in 1:4029){
  rdate[i] <- as.Date(crime_data$Date[i],format = '%m/%d/%Y')
}
crime_data$rdate <- rdate
crime_data <- read.csv('Geocoded crime data.csv')
crime_data$Date <- as.character(crime_data$Date)
crime_data$weekdays <- weekdays(crime_data$rdate)
serious <- read.csv('serious.csv')
serious <- as.character(serious$ï..SeriousOffense)
PrimaryOffense <- as.character(crime_data$PrimaryOffense)

for(i in 1:4029){
  if (PrimaryOffense[i] %in% serious){
    crime_data$Offensetype[i] <- 'emergency'}
  else{
    crime_data$Offensetype[i] <- 'normal'
  }
}

for (i in 1:4029){
  if ((crime_data$Time[i] >= '7:30') && (crime_data$Time[i] <= '15:00')){
    crime_data$Shift[i] <- 'day'
  }
  else if ((crime_data$Time[i] >= '15:00') && (crime_data$Time[i] <= '17:00')){
    crime_data$Shift[i] <- 'day and mid'
  }
  else if ((crime_data$Time[i] >= '17:00') && (crime_data$Time[i] <= '21:30')){
    crime_data$Shift[i] <- 'mid'
  }
  else if ((crime_data$Time[i] >= '21:30') || (crime_data$Time[i] <= '1:00')){
    crime_data$Shift[i] <- 'mid and night'
  }
  else if ((crime_data$Time[i] >= '1:00') && (crime_data$Time[i] <= '7:00')){
    crime_data$Shift[i] <- 'night'
  }
  else{
    crime_data$Shift[i] <- 'night and day'
  }
}