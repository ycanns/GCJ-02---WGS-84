# define required libraries
if (!require('geosphere')) install.packages("geosphere")
if (!require('dplyr')) install.packages("dplyr")
if (!require('leaflet')) install.packages("leaflet")
if (!require('rgdal')) install.packages("rgdal")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('gridExtra')) install.packages("gridExtra")
if (!require('data.table')) install.packages("data.table")
if (!require('geosphere')) install.packages("geosphere")

# define working directory
dir <- getwd()
setwd(dir)




# Coordinate transform: GCJ-02 -> WGS-84
earthR = 6378137.0
center_lat=(34.241 + 34.234)/2
center_lng=(108.943 + 108.9415)/2


transform <- function(x, y){
	xy = x * y
	absX = sqrt(abs(x))
	xPi = x * pi
	yPi = y * pi
	d = 20.0 * sin(6.0*xPi) + 20.0 * sin(2.0*xPi)

	lat = d
	lng = d

	lat = lat + (20.0*sin(yPi) + 40.0*sin(yPi/3.0))
	lng = lng + (20.0*sin(xPi) + 40.0*sin(xPi/3.0))

	lat = lat + (160.0*sin(yPi/12.0) + 320*sin(yPi/30.0))
	lng = lng + (150.0*sin(xPi/12.0) + 300.0*sin(xPi/30.0))

	lat = lat * (2.0 / 3.0)
	lng = lng * (2.0 / 3.0)

	lat = lat + (-100.0 + 2.0*x + 3.0*y + 0.2*y*y + 0.1*xy + 0.2*absX)
	lng = lng + (300.0 + x + 2.0*y + 0.1*x*x + 0.1*xy + 0.1*absX)

	latlong <- list(lat,lng)
	return (latlong)
}



delta <- function (lat, lng){
    ee = 0.00669342162296594323
    latlong = transform(lng-105.0, lat-35.0)
    dLat<-latlong[[1]]
    dLng<-latlong[[2]]
    radLat = lat / 180.0 * pi
    magic = sin(radLat)
    magic = 1 - ee * magic * magic
    sqrtMagic = sqrt(magic)
    dLat = (dLat * 180.0) / ((earthR * (1 - ee)) / (magic * sqrtMagic) * pi)
    dLng = (dLng * 180.0) / (earthR / sqrtMagic * cos(radLat) * pi)

    latlong <- list(dLat,dLng)
	return (latlong)
}




gcj2wgs <- function (gcjLat, gcjLng){
    latlong = delta(gcjLat, gcjLng)
    latlong[[1]] <- gcjLat - latlong[[1]]
    latlong[[2]] <- gcjLng - latlong[[2]]
    return (latlong)
}


distance <- function (latA, lngA, latB, lngB){
    pi180 = pi / 180
    arcLatA = latA * pi180
    arcLatB = latB * pi180
    x = (cos(arcLatA) * cos(arcLatB) *
         cos((lngA - lngB) * pi180))
    y = sin(arcLatA) * sin(arcLatB)
    s = x + y
    if (s > 1){
        s = 1
    }
    if (s < -1){
        s = -1
    }
    alpha = acos(s)
    distance = alpha * earthR
    return (distance)
} # Unit: meter
#____ Transform end


cal_speed <- function(n){
  Observation.Subset <- subset(Observation.new, NewID==Driver_List.new$DriverID[n])
  Observation.Subset <- mutate(Observation.Subset,
    TimeStamp_unit=(as.numeric(substr(Observation.Subset$TimeStamp,1,2))*3600)+
            (as.numeric(substr(Observation.Subset$TimeStamp,4,5))*60)+
            (as.numeric(substr(Observation.Subset$TimeStamp,7,9))))

  if (nrow(Observation.Subset)>1){
    # order data by time
    Observation.Subset <- Observation.Subset[order(Observation.Subset$TimeStamp_unit),]

    Observation.Subset$from_time <- NA
    Observation.Subset$from_Long <- NA
    Observation.Subset$from_Lati <- NA
    Observation.Subset$Stream <- NA
    Observation.Subset$Location <- NA
    
    Observation.Subset$from_time[2:nrow(Observation.Subset)] <- Observation.Subset$TimeStamp_unit[1:(nrow(Observation.Subset)-1)]
    Observation.Subset$from_Long[2:nrow(Observation.Subset)] <- Observation.Subset$Longitude[1:(nrow(Observation.Subset)-1)]
    Observation.Subset$from_Lati[2:nrow(Observation.Subset)] <- Observation.Subset$Latitude[1:(nrow(Observation.Subset)-1)]
    Observation.Subset <- Observation.Subset[2:nrow(Observation.Subset),]
    Observation.Subset <- mutate(Observation.Subset,
      Speed_kph=((distance(Observation.Subset$from_Lati, Observation.Subset$from_Long, 
                                            Observation.Subset$Latitude,Observation.Subset$Longitude)/
                                  (Observation.Subset$TimeStamp_unit - Observation.Subset$from_time)) * 3.6),
      Stream=ifelse((distance(Observation.Subset$from_Lati, Observation.Subset$from_Long, 
                                            center_lat,center_lng) > distance(Observation.Subset$Latitude, Observation.Subset$Longitude, 
                                            center_lat,center_lng)), "Up", 
      (ifelse((distance(Observation.Subset$from_Lati, Observation.Subset$from_Long, 
                                            center_lat,center_lng) < distance(Observation.Subset$Latitude, Observation.Subset$Longitude, 
                                            center_lat,center_lng)), "Down", "St"))),
      Bearing=ifelse((any((Observation.Subset$from_Lati/center_lat)>1) & any((Observation.Subset$from_Long/center_lng)>=1)),"NorthEast",
        (ifelse((any((Observation.Subset$from_Lati/center_lat)>1) & any((Observation.Subset$from_Long/center_lng)<=1)), "NorthWest",
          (ifelse((any((Observation.Subset$from_Lati/center_lat)<1) & any((Observation.Subset$from_Long/center_lng)<=1)), "SouthWest", 
            (ifelse((any((Observation.Subset$from_Lati/center_lat)<1) & any((Observation.Subset$from_Long/center_lng)>=1)),"SouthEast", "Center")))))))
      ) 


    Observation.Subset <- Observation.Subset[,c("NewID","TimeStamp","Longitude","Latitude", "Speed_kph", "Stream", "Bearing")]

  }
  return(Observation.Subset)
}
