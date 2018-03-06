# Packages-------------------------------------------------------
install.packages("mxnet")
install.packages("tidyverse")
library(mxnet)
library(tidyverse)




city1 <- "toronto"
city2 <- "vancouver"
sample2 <- GET(paste("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=",city1,"&destinations=",city2,"&key=",gmap_API,sep=""))
sample2


