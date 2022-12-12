#set working directory
setwd("~/R Scripts/Google_Capstone_Project_CyclisticBikes")

#prepare packages for use
library(tidyverse)

#import last 12 months of Cyclistic rider data
nov2021<- read_csv('Project_Data/Cyclistic_tripdata_202111.csv')
dec2021<- read_csv('Project_Data/Cyclistic_tripdata_202112.csv')
jan2022<- read_csv('Project_Data/Cyclistic_tripdata_202201.csv')
feb2022<- read_csv('Project_Data/Cyclistic_tripdata_202202.csv')
mar2022<- read_csv('Project_Data/Cyclistic_tripdata_202203.csv')
apr2022<- read_csv('Project_Data/Cyclistic_tripdata_202204.csv')
may2022<- read_csv('Project_Data/Cyclistic_tripdata_202205.csv')
jun2022<- read_csv('Project_Data/Cyclistic_tripdata_202206.csv')
jul2022<- read_csv('Project_Data/Cyclistic_tripdata_202207.csv')
aug2022<- read_csv('Project_Data/Cyclistic_tripdata_202208.csv')
sep2022<- read_csv('Project_Data/Cyclistic_tripdata_202209.csv')
oct2022<- read_csv('Project_Data/Cyclistic_tripdata_202210.csv')

#inspecting columns and checking for inconsistencies
colname(nov2021)
colname(dec2021)
colname(jan2022)
colname(feb2022)
colname(mar2022)
colname(apr2022)
colname(may2022)
colname(jun2022)
colname(jul2022)
colname(aug2022)
colname(sep2022)
colname(oct2022)

#adding column to show the length of each ride
nov2021$ride_length <- nov2021$ended_at-nov2021$started_at
dec2021$ride_length <- dec2021$ended_at-dec2021$started_at
jan2022$ride_length <- jan2022$ended_at-jan2022$started_at
feb2022$ride_length <- feb2022$ended_at-feb2022$started_at
mar2022$ride_length <- mar2022$ended_at-mar2022$started_at
apr2022$ride_length <- apr2022$ended_at-apr2022$started_at
may2022$ride_length <- may2022$ended_at-may2022$started_at
jun2022$ride_length <- jun2022$ended_at-jun2022$started_at
jul2022$ride_length <- jul2022$ended_at-jul2022$started_at
aug2022$ride_length <- aug2022$ended_at-aug2022$started_at
sep2022$ride_length <- sep2022$ended_at-sep2022$started_at
oct2022$ride_length <- oct2022$ended_at-oct2022$started_at

#adding column to show which day of the week each ride was on
nov2021$day_of_week <- as.Date(nov2021$started_at)
dec2021$day_of_week <- as.Date(dec2021$started_at)
jan2022$day_of_week <- as.Date(jan2022$started_at)
feb2022$day_of_week <- as.Date(feb2022$started_at)
mar2022$day_of_week <- as.Date(mar2022$started_at)
apr2022$day_of_week <- as.Date(apr2022$started_at)
may2022$day_of_week <- as.Date(may2022$started_at)
jun2022$day_of_week <- as.Date(jun2022$started_at)
jul2022$day_of_week <- as.Date(jul2022$started_at)
aug2022$day_of_week <- as.Date(aug2022$started_at)
sep2022$day_of_week <- as.Date(sep2022$started_at)
oct2022$day_of_week <- as.Date(oct2022$started_at)

#formatting 'day of the week' column to suit our needs
nov2021$day_of_week <- format(as.Date(nov2021$day_of_week),'%A')
dec2021$day_of_week <- format(as.Date(dec2021$day_of_week),'%A')
jan2022$day_of_week <- format(as.Date(jan2022$day_of_week),'%A')
feb2022$day_of_week <- format(as.Date(feb2022$day_of_week),'%A')
mar2022$day_of_week <- format(as.Date(mar2022$day_of_week),'%A')
apr2022$day_of_week <- format(as.Date(apr2022$day_of_week),'%A')
may2022$day_of_week <- format(as.Date(may2022$day_of_week),'%A')
jun2022$day_of_week <- format(as.Date(jun2022$day_of_week),'%A')
jul2022$day_of_week <- format(as.Date(jul2022$day_of_week),'%A')
aug2022$day_of_week <- format(as.Date(aug2022$day_of_week),'%A')
sep2022$day_of_week <- format(as.Date(sep2022$day_of_week),'%A')
oct2022$day_of_week <- format(as.Date(oct2022$day_of_week),'%A')

#inspecting each month's data frame for irregularities or inconsistencies
str(nov2021)
str(dec2021)
str(jan2022)
str(feb2022)
str(mar2022)
str(apr2022)
str(may2022)
str(jun2022)
str(jul2022)
str(aug2022)
str(sep2022)
str(oct2022)

#removing unnecessary columns
nov2021 <- subset(nov2021, select = -c(start_lat, start_lng, end_lat, end_lng))
dec2021 <- subset(dec2021, select = -c(start_lat, start_lng, end_lat, end_lng))
jan2022 <- subset(jan2022, select = -c(start_lat, start_lng, end_lat, end_lng))
feb2022 <- subset(feb2022, select = -c(start_lat, start_lng, end_lat, end_lng))
mar2022 <- subset(mar2022, select = -c(start_lat, start_lng, end_lat, end_lng))
apr2022 <- subset(apr2022, select = -c(start_lat, start_lng, end_lat, end_lng))
may2022 <- subset(may2022, select = -c(start_lat, start_lng, end_lat, end_lng))
jun2022 <- subset(jun2022, select = -c(start_lat, start_lng, end_lat, end_lng))
jul2022 <- subset(jul2022, select = -c(start_lat, start_lng, end_lat, end_lng))
aug2022 <- subset(aug2022, select = -c(start_lat, start_lng, end_lat, end_lng))
sep2022 <- subset(sep2022, select = -c(start_lat, start_lng, end_lat, end_lng))
oct2022 <- subset(oct2022, select = -c(start_lat, start_lng, end_lat, end_lng))

#combining months into quarters for seasonal comparisons
q1 <- rbind(nov2021,dec2021,jan2022)
q2 <- rbind(feb2022,mar2022,apr2022)
q3 <- rbind(may2022,jun2022,jul2022)
q4 <- rbind(aug2022,sep2022,oct2022)

#inspecting quarters for irregularities or inconsistencies
str(q1)
str(q2)
str(q3)
str(q4)

#combining quarters into a 12-month data frame to analyze data as a whole
alltrips <- rbind(q1,q2,q3,q4)

#inspecting alltrips data frame for irregularities or inconsistencies
str(alltrips)

#check how many observations there are between members and casual riders
table(alltrips$member_casual)
#Note: there are 1,049,628 more member observations than casual riders

#inspecting rideable_type dispersion
table(alltrips$rideable_type)

#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
median(alltrips$ride_length) #midpoint of all ride lengths
max(alltrips$ride_length) #longest ride
min(alltrips$ride_length) #shortest ride
#Note: some rides appear to be in the negative

#duplicating alltrips dataframe but removing negative time rides
alltripsV2 <- alltrips[!(alltrips$ride_length < 0)]