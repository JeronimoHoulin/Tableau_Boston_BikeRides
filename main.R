"Crisil Data Analitics Exam"
library("tidyverse")
library("tidyr")
#Rout of csv files
stations = read.csv("C:/Users/Usuario/Desktop/Master_vis_test/data/hubway_stations.csv") 

trips = read.csv("C:/Users/Usuario/Desktop/Master_vis_test/data/hubway_trips.csv")
colSums(is.na(trips))

trips$birth_date[is.na(trips$birth_date)] <- NULL

head(stations)
head(trips)
#check if df characters ar OK
str(stations)
str(trips)
#change start/end station names
names(trips)[6] = "start_id"
names(trips)[8] = "end_id"
#Separate trip Date $ Time into D&T

trips$start_time <- format(as.POSIXct(strptime(trips$start_date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
trips$start_day <- format(as.POSIXct(strptime(trips$start_date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
trips$end_time <- format(as.POSIXct(strptime(trips$end_date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
trips$end_day <- format(as.POSIXct(strptime(trips$end_date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%m/%d/%Y")

# trips <- trips[ -c(5,7) ]


#join the Lat / Lon coordinates for both starting and ending points

salida_latlong = left_join(trips, stations, by = c("start_id" = "id"))
llegada_latlong = left_join(trips, stations, by = c("end_id" = "id"))

#append it to our trips df
trips$end_lat = llegada_latlong$lat
trips$end_lon= llegada_latlong$lng
trips$start_lat = salida_latlong$lat
trips$start_lon = salida_latlong$lng 

#making trip duraiton positive
trips$duration[trips$duration <0] <- mean(trips$duration)

#seeing outliers
boxplot(trips$duration)
which(trips$duration > 900000)
#delete outliers
trips[-c(14272  , 87179  , 96543 , 101639 , 144187),]

mean(trips$duration)

write.csv(trips,"C:/Users/Usuario/Desktop/Master_vis_test/data/data_sync.csv", row.names = FALSE)




#see amount per generations
# Gen Z, iGen, or Centennials: Born 1996 - 2015
# Millennials or Gen Y: Born 1977 - 1995
# Generation X: Born 1965 - 1976
# Baby Boomers: Born 1946 - 1964
# Traditionalists or Silent Generation: Born 1945 and before
trad = nrow(subset(trips,birth_date < 1946)) #Traditionalists
bby = nrow(subset(trips,birth_date >= 1946 & birth_date < 1965)) #Baby boomers
genx = nrow(subset(trips,birth_date >= 1965 & birth_date < 1976)) #Gen X
geny = nrow(subset(trips,birth_date >= 1976 & birth_date < 1995)) #Gen y 
genz = nrow(subset(trips,birth_date >= 1995 & birth_date < 2015)) #Gen z 
above = nrow(subset(trips,birth_date >= 2015 )) #Above

sum = trad + bby + genx + genz + geny + genz+ above

#amount of genders
men = nrow(subset(trips, gender == 'Male'))
women = nrow(subset(trips, gender == 'Female'))
men_prct = men/(men + women)
wmn_prct = women/(men+women)


mean(trips$duration)

#percentage of trips originated in each municipality
length(grep("Boston",salida_latlong$municipal)) / length(salida_latlong$municipal)
length(grep("Somerville",salida_latlong$municipal)) / length(salida_latlong$municipal)
length(grep("Cambridge",salida_latlong$municipal)) / length(salida_latlong$municipal)
length(grep("Brookline",salida_latlong$municipal)) / length(salida_latlong$municipal)




#most frequent duration
getmode <- function(df) {
  uniq <- unique(df)
  uniq[which.max(tabulate(match(df, uniq)))]
}

getmode(trips$duration)






#ARMIA MODEL for forecasting
library(forecast)
fit <- auto.arima(trips$start_id)
summary(fit)

library(lmtest)
library(stargazer)


#linear model wtih birth date
linear <- lm(trips$duration ~trips$birth_date)
coeftest(linear) 
summary(linear)

#linear model wtih gender
linear <- lm(trips$duration ~trips$gender)
coeftest(linear) 
summary(linear)

#linear model wtih gender
linear <- lm(trips$duration ~trips$start_id)
coeftest(linear) 
summary(linear)
