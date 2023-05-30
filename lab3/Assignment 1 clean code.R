################Assignment 1########################333
#install.packages("geosphere")
library(geosphere)
#install.packages("dplyr")
library(dplyr)
################preparations########################
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stationsutf8.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 100000
h_date <- 10
h_time <-2

##The point to predict (up to the students)- Stockholm
# a=latitude  b= longitude
a <- 59.335 
b <- 18.063
# date
date <- "2013-08-04" # The date to predict (up to the students)
# times of interest
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
# Students’ code here


##################function to calculate Gaussian kernel for distance ############
#########takes a row from st table as an input 
StarStation= c(a,b)
GKerneldistance <- function(station2) { 
  coordinatesStation2=c(station2$latitude,station2$longitude)
  distance <- distHaversine(StarStation,coordinatesStation2)
  result = exp(- (distance/h_distance)^2 )
  return(result)
} 

############picking nice smoothing factor h_distance###########
distanceList=c()
list<-c()
for(i in 1 : nrow(st)){
  distanceList <- append(distanceList,distHaversine(StarStation,c(st[i,]$latitude,st[i,]$longitude)))
  list[i]= GKerneldistance(st[i,])
}
#plotting the dependency of distance kernels values on physical distance
plot(x=distanceList,y=list)

##################function for getting vector of distance kernels for given data########
##returns a vector
filldist <-function(data)
{
  list=c()
  for(i in 1 : nrow(data)){
    list[i]= GKerneldistance(data[i,])
  }
  return(list)
}

####################################################Gaussian kernel for Day ################
StarDay=as.Date(date)
GKernelDay <- function(Day2) { 
  DateDay2=as.Date(Day2)
  daysdiff=((as.numeric(difftime(DateDay2,StarDay, units = "days")))%%365)
  result = exp((- (daysdiff)^2)/(h_date)^2)
  return(result)
} 

############picking nice smoothing factor h_day###########
dayDistList=c()
daylist<-c()
for(i in 1 : nrow(st)){
  dayDistList <- append(dayDistList,(as.numeric(difftime(as.Date(st[i,]$date),StarDay, units = "days"))%%365))
  daylist<-append(daylist,GKernelDay(st[i,]$date))
}
#plotting the dependency of day kernels values on the distance between days
plot(x=dayDistList,y=daylist)
##########################################

###############function for getting vector of day kernels for given data and day
##returns a vector
fillDay <-function(data)
{
  daylist=c()
  for(i in 1 : nrow(data)){
    daylist[i]= GKernelDay(data[i,]$date)
  }
  return(daylist)
}
##################


############Gaussian kernel for Hours ################
GKernelHours <- function(Hour2,StarHour) {
  first <- as.POSIXct(paste("2022-01-01 ",StarHour))
  second <- as.POSIXct(paste("2022-01-01 ",Hour2))
  result = exp((- (as.numeric( difftime(first, second,units = "hours")))^2)/(h_time)^2)
  return(result)
} 

##################This code to pick smoothing factor for hours#########
############picking nice smoothing factor h_day###########
hoursDistList=c()
hourKernellistForH<-c()
StarHourH=times[3]
StarHourHTime <- as.POSIXct(paste("2022-01-01 ",StarHourH))

for(i in 1 : nrow(st)){
  currentTime <- as.POSIXct(paste("2022-01-01 ",st[i,]$time))
  hoursDistList <- append(hoursDistList,(as.numeric( abs(difftime(currentTime, StarHourHTime,units = "hours")))))
  hourKernellistForH<-append(hourKernellistForH,GKernelHours(st[i,]$time,StarHourH))
}
#plotting the dependency of day kernels values on the distance between days
plot(x=hoursDistList,y=hourKernellistForH)
######################################

####################Forecast for Sum and Multiply ####################

finalsum= c() #sum of kernels
mult1= list() #multiplication of kernels
forecastsum<-c()
forecastmult<-c()
daylist<-c()

for(i in 1 : length(times)){
  StarHour=times[i] 
  #filtering
  stfiltered <- filter(st, date < StarDay & time<StarHour)
  daylist=fillDay(stfiltered)
  distlist=filldist(stfiltered)
  timelist=c(1:nrow(stfiltered))
  for(k in 1 : nrow(stfiltered)){
    timelist[k]= GKernelHours(stfiltered[k,]$time,StarHour)
  }

  finalsum<-distlist+timelist+daylist
  mult1=distlist*timelist*daylist
  forecastsum[i]=sum(finalsum*stfiltered$air_temperature)/sum(finalsum)
  forecastmult[i]=sum(mult1*stfiltered$air_temperature)/sum(mult1)
}
print(forecastsum)
print(forecastmult)

plot(forecastsum, type="o", xlab = "Time", ylab = "Temperature forecast", xaxt="n",ylim=c(0,25),col="blue")
axis(1, at=1:length(times), labels = times)
points(forecastmult, type="o", xlab = "Time", xaxt="n",col="green")
legend(5, 5, c("sum", "mult"), col = c(4,3), text.col = "red", pch = c(3, 4))
