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

#adding column to show the length of each ride
alltrips$ride_length <- alltrips$ended_at-alltrips$started_at

#adding column to show which day of the week each ride was on
alltrips$day_of_week <- as.Date(alltrips$started_at)

#formatting 'day of the week' column to suit our needs
alltrips$day_of_week <- format(as.Date(alltrips$day_of_week),'%A')

#removing unnecessary columns
alltrips <- subset(alltrips, select = -c(start_lat, start_lng, end_lat, end_lng))

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