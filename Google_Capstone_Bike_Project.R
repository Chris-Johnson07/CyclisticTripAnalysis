#set working directory
setwd("~/R Scripts/Google_Capstone_Project_CyclisticBikes")

#prepare packages for use
library(tidyverse)
library(lubridate)

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
colnames(nov2021)
colnames(dec2021)
colnames(jan2022)
colnames(feb2022)
colnames(mar2022)
colnames(apr2022)
colnames(may2022)
colnames(jun2022)
colnames(jul2022)
colnames(aug2022)
colnames(sep2022)
colnames(oct2022)

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

#combining months into a 12-month data frame to analyze data as a whole
alltrips <- rbind(nov2021,
                  dec2021,
                  jan2022,
                  feb2022,
                  mar2022,
                  apr2022,
                  may2022,
                  jun2022,
                  jul2022,
                  aug2022,
                  sep2022,
                  oct2022)

#inspecting alltrips data frame for irregularities or inconsistencies
str(alltrips)

#check how many observations there are between members and casual riders
table(alltrips$member_casual)
#Note: there are 1,049,628 more member observations than casual riders

#inspecting rideable_type dispersion
table(alltrips$rideable_type)

#now inspecting rideable_type dispersion by member/casual rider
table(alltrips$member_casual,alltrips$rideable_type)

#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
median(alltrips$ride_length) #midpoint of all ride lengths
max(alltrips$ride_length) #longest ride
min(alltrips$ride_length) #shortest ride
#Note: some rides appear to be in the negative

#duplicating alltrips dataframe but removing ride lengths less than 0 secs
alltripsV2 <- alltrips[!(alltrips$ride_length < 0),]

#inspecting ride_length column from new dataframe
mean(alltripsV2$ride_length) #average ride length
median(alltripsV2$ride_length) #midpoint of all ride lengths
max(alltripsV2$ride_length) #longest ride
min(alltripsV2$ride_length) #shortest ride

#now inspecting characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)

#order day of week column
alltripsV2$day_of_week <- ordered(alltripsV2$day_of_week, levels = 
                                    c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#inspecting number of rides by day of week
table(alltripsV2$day_of_week)

#inspecting number of rides by member/casual then by day of week
table(alltripsV2$member_casual, alltripsV2$day_of_week)

#average ride length by member/casual rider then by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)

#average ride by member/casual rider by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)

#number of rides and average ride length grouped by member/casual rider and day of the week
alltripsV2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#visualization of number of rides by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')
ggsave('number_of_rides.png')

#visualization of average ride length by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Average Duration in Minutes')
ggsave("average_duration.png")

#visualization of the type of bike used by member/casual riders
alltripsV2[alltripsV2$member_casual == 'member',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'member',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Members')

alltripsV2[alltripsV2$member_casual == 'casual',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'casual',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Casual Riders')

#now let's inspect the hours of the day riders start their trips at
alltripsV2$start_hour <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)

alltripsV2$start_hour <- format(alltripsV2$start_hour, format = '%H')
str(alltripsV2)

table(alltripsV2$start_hour)

#let's visualize the number of rides started for each hour of the day
alltripsV2 %>% group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, start_hour) %>% 
  ggplot(aes(x = start_hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Hour of the Day', y = 'Number of Rides')
ggsave("rides_by_the_hour")

#let's visualize number of rides for each month by member/casual riders
alltripsV2$month <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)

alltripsV2$month <- format(alltripsV2$month, format = '%b')
str(alltripsV2)

alltripsV2$month <- ordered(alltripsV2$month, levels = 
                              c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

alltripsV2 %>% group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Month', y = 'Number of Rides')