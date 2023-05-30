################Assignment 1########################333

################preparations########################
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stationsutf8.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 100000000# These three values are up to the students
h_date <- 100
h_time <-2
# a=latitude  b= longitude
a <- 58.4274 # The point to predict (up to the students) 
b <- 14.826

datestar <- "2013-11-04" # The date to predict (up to the students)

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))
# Students’ code here

library(dplyr)
tempsfiltered <- filter(temps, date < datestar)

###############task1##################################
#install.packages("geosphere")
library(geosphere)
#plot(temp, type="o")


StarStation= c(a,b)


######### kernel for distance ############

GKerneldistance <- function(station2) { 
  coordinatesStation2=c(station2$latitude,station2$longitude)
  result = exp((-(distHaversine(StarStation,coordinatesStation2, r=6378137))^2)/(2*h_distance)^2)
  return(result)
} 

list=list()

for(i in 1 : 812){
  
  list[i]= GKerneldistance(stations[i,])
  print(GKerneldistance(stations[i,]))
}
plot(x=c(1:812),y=list, type="o")

Y=list

GBell = data.frame(Y = list, X = stations$station_number)

matplot(x=stations$station_number,y=list)


#Plot
library(ggplot2)
ggplot(GBell, aes(x = X, y = Y))
       +  geom_line()


############ kernel for Day ################

StarDay=as.Date(date)
GKernelDay <- function(Day2) { 
  
  DateDay2=as.Date(Day2)
  
  result = exp((- (as.numeric(difftime(DateDay2,StarDay, units = "days")))^2)/(2*h_date)^2)

  return(result)
} 
daylist=list()

for(i in 1 : nrow(temps)){
  
  if(temps[i,]$date < StarDay )
    {
     daylist[i]= GKernelDay(temps[i,]$date)
     
     print(GKernelDay(temps[i,]$date))
    
  }
}

plot(x=c(1:length(unlist(daylist))),y=as.numeric(unlist(daylist)), type="o")


############ kernel for Hours ################

#library(hms)

StarHour=c(times)
GKernelHours <- function(Hour2) {
  first <- as.POSIXct(paste("2022-01-01 ",StarHour))
  second <- as.POSIXct(paste("2022-01-01 ",Hour2))
  # result = exp((- (as.numeric(difftime(Hour2,StarHour, units = "hours")))^2)/(2*h_time)^2)
  result = exp((- (as.numeric( difftime(first, second,units = "hours")))^2)/(2*h_time)^2)
  return(result)
} 

timelist=list() #may want to change to amtrix to be able to store 11 lists

#Gkernel hours for all of our inputs by comparing it with starhour
#for (k in 1:length(times)) #k 1 to 11
#{
  StarHour=times[1] 
  for(i in 1 : nrow(temps)){# each 11 times
    if(temps[i,]$time < StarHour ){
      timelist[i]= GKernelHours(temps[i,]$time)
    }
  }
#}
plot(x=c(1:length(unlist(timelist)),y=as.numeric(unlist(timelist)), type="o"))


#Data

#Question what to do to see a bell curve
#Question how to filter instead of if statement. 


#We have implemented three gaussian kernels and begun an attempt to filter out posterior dates on line 107. 

#sum up gaussian kernels

#multiply gaussian kernels

#forecast for sum and mult

#plot forecasts and compare results
