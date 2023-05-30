################Assignment 1########################333

################preparations########################
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stationsutf8.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 0# These three values are up to the students
h_date <- 0
h_time <- 0

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826

date <- "2013-11-04" # The date to predict (up to the students)

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))
# Students’ code here


###############task1##################################
#install.packages("geosphere")
#library(geosphere)
plot(temp, type="o")
