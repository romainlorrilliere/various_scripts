library(data.table) ## to easily manipulate data
library(readxl) ## to import data at xlsx format
library(lubridate) ## to manage date


## weather data importation
## data file is in a repertory named "data"
dw  <-  readxl::read_xlsx("data/weather_data.xlsx")

## transfom the data to a data.table object
setDT(dw)

## check data
dim(dw)
head(dw)

## changing the column names
colnames(dw) <- c("year","month","day","temp_min","temp_max","temp_mean","precipitation")

## add columns : date and julian_day
dw[,date := as.Date(paste0(year,"-",sprintf("%02d", month),"-",sprintf("%02d", day)))]
dw[,julian_day := yday(date)]

## assess the values of the temperature and precipitation references by day
dw[,`:=`(temp_ref = median(temp_mean),precipitation_ref = median(precipitation)),by=julian_day]

## assess the anomalies
dw[,`:=`(temp_anomly = temp_mean - temp_ref,precipitation_anomaly = precipitation - precipitation_ref)]

print(head(dw))


fwrite(dw,"data/weather_data_anomaly.csv")

