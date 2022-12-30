Cyclistic Bike Share Analysis
================
Chris Johnson
2022-12-28

## Overview

This is a capstone project from Google’s Data Analytics Certification on
Coursera. In the following presentation we will go over the six steps in
data analysis: Ask, Prepare, Process, Analyze, Share, and Act. This
project is based on the following practice scenario.

**From the scenario**

*“You are a junior data analyst working in the marketing analyst team at
Cyclistic, a bike-share company in Chicago. The director of marketing
believes the company’s future success depends on maximizing the number
of annual memberships. Therefore, your team wants to understand how
casual riders and annual members use Cyclistic bikes differently. From
these insights, your team will design a new marketing strategy to
convert casual riders into annual members. But first, Cyclistic
executives must approve your recommendations, so they must be backed up
with compelling data insights and professional data visualizations.”*

In 2016, Cyclistic launched a successful bike-share offering. Since
then, the program has grown to a fleet of 5,824 bicycles that are
geotracked and locked into a network of 692 stations across Chicago. The
bikes can be unlocked from one station and returned to any other station
in the system anytime. Until now, Cyclistic's marketing strategy relied
on building general awareness and appealing to broad consumer segments.
One approach that helped make these things possible was the flexibility
of its pricing plans: single-ride passes, full-day passes, and annual
memberships. Customers who purchase single-ride or full-day passes are
referred to as casual riders. Customers who purchase annual memberships
are Cyclistic members. Cyclistic's finance analysts have concluded that
annual members are much more profitable than casual riders. Although the
pricing flexibility helps Cyclistic attract more customers, Moreno
believes that maximizing the number of annual members will be key to
future growth. Rather than creating a marketing campaign that targets
all-new customers, Moreno believes there is a very good chance to
convert casual riders into members. She notes that casual riders are
already aware of the Cyclistic program and have chosen Cyclistic for
their mobility needs. Moreno has set a clear goal: Design marketing
strategies aimed at converting casual riders into annual members. In
order to do that, however, the marketing analyst team needs to better
understand how annual members and casual riders differ, why casual
riders would buy a membership, and how digital media could affect their
marketing tactics. Moreno and her team are interested in analyzing the
Cyclistic historical bike trip data to identify trends.

## Ask

In the **Ask** step we will look at the guiding questions for our
project and define what problem we are trying to solve.

**From the scenario**

Three questions will guide the future marketing program:

- How do annual members and casual riders use Cyclistic bikes
  differently?

- Why would casual riders buy Cyclistic annual memberships?

- How can Cyclistic use digital media to influence casual riders to
  become members?

Moreno has assigned you the first question to answer: How do annual
members and casual riders use Cyclistic bikes differently?

## Prepare

In the **Prepare** step we will identify the dataset provided and answer
some questions to put context behind it and determine its quality.

- *Where is your data located?* The data was provided by the company and
  I have stored it locally on my computer for better security.

- *How is the data organized?* The company has provided the most recent
  12 months of data, each month having its own data file to download.
  For each of these files I used the naming convention “Company Name” +
  “Data Type” + “Year and Month”. For example:
  “Cyclistic_tripdata_202210.txt”. Additionally, I structured the folder
  names as Cyclistic Bike Share Project/Raw Data for the initial raw
  data I downloaded and a separate folder “Project Data” for processed
  data files.

- *Are there issues with bias or credibility in this data? Does your
  data ROCCC?* This data is first party data and is ROCCC (Reliable,
  Original, Comprehensive, Current, and Cited)

- *How are you addressing licensing, privacy, security, and
  accessibility?* The company has provided a license for the data, the
  data does not provide personal information from any of the users, and
  the data is being stored locally for analysis.

- *How does it help you answer your question?* The credibility of this
  data helps strengthen the validity of any findings made during
  analysis

## Process

In the **Process** step we look at how the data is structured and
identify any needs for cleaning.

First we will setup our code for analysis

``` r
#set working directory
setwd("~/R Scripts/Google_Capstone_Project_CyclisticBikes")

#prepare packages for use
library(tidyverse)
library(lubridate)
```

Next we will load in our data

``` r
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
```

Now we will start looking at the data structure and correct any issues
we come across

``` r
#inspecting columns and checking for inconsistencies
colnames(nov2021)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(dec2021)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(jan2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(feb2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(mar2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(apr2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(may2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(jun2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(jul2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(aug2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(sep2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
colnames(oct2022)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

``` r
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
```

``` r
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
```

``` r
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
```

    ## tibble [5,755,694 × 11] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5755694] "7C00A93E10556E47" "90854840DFD508BA" "0A7D10CDD144061C" "2F3BE33085BCFF02" ...
    ##  $ rideable_type     : chr [1:5755694] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5755694], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...
    ##  $ ended_at          : POSIXct[1:5755694], format: "2021-11-27 13:46:38" "2021-11-27 13:56:10" ...
    ##  $ start_station_name: chr [1:5755694] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:5755694] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:5755694] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:5755694] NA NA NA NA ...
    ##  $ member_casual     : chr [1:5755694] "casual" "casual" "casual" "casual" ...
    ##  $ ride_length       : 'difftime' num [1:5755694] 1140 1065 142 301 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : chr [1:5755694] "Saturday" "Saturday" "Friday" "Saturday" ...

## Analyze

In the **Analyze** step we will perform calculations and aggregations to
seek out trends or discoveries.

``` r
#check how many observations there are between members and casual riders
table(alltrips$member_casual)
```

    ## 
    ##  casual  member 
    ## 2353033 3402661

``` r
#Note: there are 1,049,628 more member observations than casual riders
```

``` r
#inspecting rideable_type dispersion
table(alltrips$rideable_type)
```

    ## 
    ##  classic_bike   docked_bike electric_bike 
    ##       2637937        182205       2935552

``` r
#now inspecting rideable_type dispersion by member/casual rider
table(alltrips$member_casual,alltrips$rideable_type)
```

    ##         
    ##          classic_bike docked_bike electric_bike
    ##   casual       897427      182205       1273401
    ##   member      1740510           0       1662151

``` r
#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
```

    ## Time difference of 1166.389 secs

``` r
median(alltrips$ride_length) #midpoint of all ride lengths
```

    ## Time difference of 620 secs

``` r
max(alltrips$ride_length) #longest ride
```

    ## Time difference of 2483235 secs

``` r
min(alltrips$ride_length) #shortest ride
```

    ## Time difference of -621201 secs

``` r
#Note: some ride lengths appear to be in the negative
```

``` r
#duplicating alltrips dataframe but removing ride lengths less than 0 secs
alltripsV2 <- alltrips[!(alltrips$ride_length < 0),]
#Note: 112 rows removed leaving 5,755,782 rows
```

``` r
#inspecting ride_length column from new dataframe
mean(alltripsV2$ride_length) #average ride length
```

    ## Time difference of 1166.556 secs

``` r
median(alltripsV2$ride_length) #midpoint of all ride lengths
```

    ## Time difference of 620 secs

``` r
max(alltripsV2$ride_length) #longest ride
```

    ## Time difference of 2483235 secs

``` r
min(alltripsV2$ride_length) #shortest ride
```

    ## Time difference of 0 secs

``` r
#now inspecting characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
```

    ##   Group.1              x
    ## 1  casual 1750.3054 secs
    ## 2  member  762.8815 secs

``` r
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
```

    ##   Group.1        x
    ## 1  casual 790 secs
    ## 2  member 530 secs

``` r
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
```

    ##   Group.1            x
    ## 1  casual 2483235 secs
    ## 2  member   93594 secs

``` r
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)
```

    ##   Group.1      x
    ## 1  casual 0 secs
    ## 2  member 0 secs

``` r
#order day of week column
alltripsV2$day_of_week <- ordered(alltripsV2$day_of_week, levels = 
                                    c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#inspecting number of rides by day of week
table(alltripsV2$day_of_week)
```

    ## 
    ##    Sunday    Monday   Tuesday Wednesday  Thursday    Friday  Saturday 
    ##    792877    774650    788087    805049    839611    815649    939659

``` r
#inspecting number of rides by member/casual then by day of week
table(alltripsV2$member_casual, alltripsV2$day_of_week)
```

    ##         
    ##          Sunday Monday Tuesday Wednesday Thursday Friday Saturday
    ##   casual 397048 284967  264384    275394   306947 338956   485276
    ##   member 395829 489683  523703    529655   532664 476693   454383

``` r
#average ride length by member/casual rider then by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)
```

    ##    alltripsV2$member_casual alltripsV2$day_of_week alltripsV2$ride_length
    ## 1                    casual                 Sunday         2031.1183 secs
    ## 2                    member                 Sunday          844.7337 secs
    ## 3                    casual                 Monday         1757.6043 secs
    ## 4                    member                 Monday          737.7097 secs
    ## 5                    casual                Tuesday         1561.1835 secs
    ## 6                    member                Tuesday          728.1012 secs
    ## 7                    casual              Wednesday         1494.3808 secs
    ## 8                    member              Wednesday          724.7810 secs
    ## 9                    casual               Thursday         1528.2472 secs
    ## 10                   member               Thursday          734.8682 secs
    ## 11                   casual                 Friday         1674.3550 secs
    ## 12                   member                 Friday          748.6522 secs
    ## 13                   casual               Saturday         1958.0404 secs
    ## 14                   member               Saturday          850.9703 secs

``` r
#number of rides and average ride length grouped by member/casual rider and day of the week
alltripsV2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int> <drtn>          
    ##  1 casual        Sun              397048 2031.1183 secs  
    ##  2 casual        Mon              284967 1757.6043 secs  
    ##  3 casual        Tue              264384 1561.1835 secs  
    ##  4 casual        Wed              275394 1494.3808 secs  
    ##  5 casual        Thu              306947 1528.2472 secs  
    ##  6 casual        Fri              338956 1674.3550 secs  
    ##  7 casual        Sat              485276 1958.0404 secs  
    ##  8 member        Sun              395829  844.7337 secs  
    ##  9 member        Mon              489683  737.7097 secs  
    ## 10 member        Tue              523703  728.1012 secs  
    ## 11 member        Wed              529655  724.7810 secs  
    ## 12 member        Thu              532664  734.8682 secs  
    ## 13 member        Fri              476693  748.6522 secs  
    ## 14 member        Sat              454383  850.9703 secs

``` r
#visualization of number of rides by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge')
```

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
![unnamed-chunk-18-1](https://user-images.githubusercontent.com/52151771/210039884-7517bca4-a78f-4246-bd03-ed4790c322d3.png)

``` r
#visualization of average ride length by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) +
  geom_col(position = 'dodge')
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.
    ## Don't know how to automatically pick scale for object of type <difftime>.
    ## Defaulting to continuous.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
![unnamed-chunk-19-1](https://user-images.githubusercontent.com/52151771/210039880-44b15ed8-56ce-417c-b107-838711656a42.png)

``` r
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
```

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
![unnamed-chunk-20-1](https://user-images.githubusercontent.com/52151771/210039863-3cf25bbc-0113-44d2-99f4-9b8e03888fd5.png)

``` r
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
```

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->
![unnamed-chunk-20-2](https://user-images.githubusercontent.com/52151771/210039856-40fb33cf-a2c0-4a34-b1d3-b58b0014db96.png)

``` r
#now let's inspect the hours of the day riders start their trips at
alltripsV2$start_hour <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)
```

    ## tibble [5,755,582 × 12] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5755582] "7C00A93E10556E47" "90854840DFD508BA" "0A7D10CDD144061C" "2F3BE33085BCFF02" ...
    ##  $ rideable_type     : chr [1:5755582] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...
    ##  $ ended_at          : POSIXct[1:5755582], format: "2021-11-27 13:46:38" "2021-11-27 13:56:10" ...
    ##  $ start_station_name: chr [1:5755582] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:5755582] NA NA NA NA ...
    ##  $ member_casual     : chr [1:5755582] "casual" "casual" "casual" "casual" ...
    ##  $ ride_length       : 'difftime' num [1:5755582] 1140 1065 142 301 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : Ord.factor w/ 7 levels "Sunday"<"Monday"<..: 7 7 6 7 6 6 7 7 7 7 ...
    ##  $ start_hour        : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...

``` r
alltripsV2$start_hour <- format(alltripsV2$start_hour, format = '%H')
str(alltripsV2)
```

    ## tibble [5,755,582 × 12] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5755582] "7C00A93E10556E47" "90854840DFD508BA" "0A7D10CDD144061C" "2F3BE33085BCFF02" ...
    ##  $ rideable_type     : chr [1:5755582] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...
    ##  $ ended_at          : POSIXct[1:5755582], format: "2021-11-27 13:46:38" "2021-11-27 13:56:10" ...
    ##  $ start_station_name: chr [1:5755582] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:5755582] NA NA NA NA ...
    ##  $ member_casual     : chr [1:5755582] "casual" "casual" "casual" "casual" ...
    ##  $ ride_length       : 'difftime' num [1:5755582] 1140 1065 142 301 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : Ord.factor w/ 7 levels "Sunday"<"Monday"<..: 7 7 6 7 6 6 7 7 7 7 ...
    ##  $ start_hour        : chr [1:5755582] "13" "13" "22" "09" ...

``` r
table(alltripsV2$start_hour)
```

    ## 
    ##     00     01     02     03     04     05     06     07     08     09     10 
    ##  83524  52959  32146  19396  16878  45929 120911 225965 275774 219938 234144 
    ##     11     12     13     14     15     16     17     18     19     20     21 
    ## 290410 339732 346230 353726 406576 494896 574922 488597 362110 260519 211190 
    ##     22     23 
    ## 175884 123226

``` r
#let's visualize the number of rides started for each hour of the day
alltripsV2 %>% group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, start_hour) %>% 
  ggplot(aes(x = start_hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual))
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->
![unnamed-chunk-22-1](https://user-images.githubusercontent.com/52151771/210039839-348cc603-8e6f-46db-838f-2ee9023bb170.png)

``` r
#let's visualize number of rides for each month by member/casual riders
alltripsV2$month <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)
```

    ## tibble [5,755,582 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5755582] "7C00A93E10556E47" "90854840DFD508BA" "0A7D10CDD144061C" "2F3BE33085BCFF02" ...
    ##  $ rideable_type     : chr [1:5755582] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...
    ##  $ ended_at          : POSIXct[1:5755582], format: "2021-11-27 13:46:38" "2021-11-27 13:56:10" ...
    ##  $ start_station_name: chr [1:5755582] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:5755582] NA NA NA NA ...
    ##  $ member_casual     : chr [1:5755582] "casual" "casual" "casual" "casual" ...
    ##  $ ride_length       : 'difftime' num [1:5755582] 1140 1065 142 301 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : Ord.factor w/ 7 levels "Sunday"<"Monday"<..: 7 7 6 7 6 6 7 7 7 7 ...
    ##  $ start_hour        : chr [1:5755582] "13" "13" "22" "09" ...
    ##  $ month             : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...

``` r
alltripsV2$month <- format(alltripsV2$month, format = '%b')
str(alltripsV2)
```

    ## tibble [5,755,582 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5755582] "7C00A93E10556E47" "90854840DFD508BA" "0A7D10CDD144061C" "2F3BE33085BCFF02" ...
    ##  $ rideable_type     : chr [1:5755582] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5755582], format: "2021-11-27 13:27:38" "2021-11-27 13:38:25" ...
    ##  $ ended_at          : POSIXct[1:5755582], format: "2021-11-27 13:46:38" "2021-11-27 13:56:10" ...
    ##  $ start_station_name: chr [1:5755582] NA NA NA NA ...
    ##  $ start_station_id  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_name  : chr [1:5755582] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:5755582] NA NA NA NA ...
    ##  $ member_casual     : chr [1:5755582] "casual" "casual" "casual" "casual" ...
    ##  $ ride_length       : 'difftime' num [1:5755582] 1140 1065 142 301 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week       : Ord.factor w/ 7 levels "Sunday"<"Monday"<..: 7 7 6 7 6 6 7 7 7 7 ...
    ##  $ start_hour        : chr [1:5755582] "13" "13" "22" "09" ...
    ##  $ month             : chr [1:5755582] "Nov" "Nov" "Nov" "Nov" ...

``` r
alltripsV2$month <- ordered(alltripsV2$month, levels = 
                              c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

alltripsV2 %>% group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual))
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
![unnamed-chunk-23-1](https://user-images.githubusercontent.com/52151771/210039824-5aa0ac03-03aa-4426-8cef-8037efd92f88.png)

## Share

In the **Share** step we communicate with our stakeholders our findings
in presentation form whether that is on Powerpoint or some other
presentation platform. Below you’ll see a basic outline of a
presentation with the findings made from the analysis and answers to the
original project questions.

**Overview**. The questions we want to answer are *“how do members and
casual riders differ in their use of the company’s bike share service”*
and *“what are our recommendations based on our analysis.”* In order to
sufficiently answer these questions we first wanted to establish the
validity and integrity of the data provided, we then want to comb
through the dataset for any abnormalities, after which we will then show
how the analysis process took place and finally explain what the
analysis process found.

**Presentation.** During a real presentation I would go over a lot of
what has already been presented but also include the following analysis.

**Analysis.** Our analysis found some note-worthy behaviors in both
members and casual riders that can help us understand our customers as
well as help us specifically target casual riders.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
![unnamed-chunk-24-1](https://user-images.githubusercontent.com/52151771/210039807-2b4da315-c6bf-41f8-a7d2-af38c9db6693.png)

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
![unnamed-chunk-25-1](https://user-images.githubusercontent.com/52151771/210039800-955e3249-5154-460b-a0d2-5dd47a1f0736.png)

In these two histograms, we see that:

- number of rides for casual riders peaks during the weekend, favoring
  Saturday the most

- casual riders more than double their average ride length when compared
  to the average ride length of members

- casual riders prefer longer rides during the weekends.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
![unnamed-chunk-26-1](https://user-images.githubusercontent.com/52151771/210039788-77ff082d-9c58-4c72-a0ea-60b9da53b9a1.png)

In this line graph we see that, like members, number of rides for casual
riders peaks at 5pm.

![](RMarkdownCyclisticBikeShare_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
![unnamed-chunk-27-1](https://user-images.githubusercontent.com/52151771/210037603-721edf96-0ee1-4c45-9923-d19160f97c0b.png)

In this line graph we see that number of rides for members steadily
increases during summer months and remains high until after October.
Although casual riders show a relatively similar trend, the number of
rides for casual riders sharply increases during summer months but
quickly decreases after August.

## Act

This data has allowed us to identify a few key insights into some
aspects of how members and casual riders use Cyclistic’s bikes but it
does not give us insight into how they became members, why they chose
Cyclistic Bike-Share, or for what purpose they used Cyclistic
Bike-Share. For this reason our recommendations, with the business task
in mind, can only help us identify which casual riders our marketing
team should focus on. It is clear from our findings that the top 3
groups of casual riders we should focus on is:

- Casual riders who bike-share during the summer

- Casual riders who bike-share on the weekends

- Casual riders who bike-share around 5pm
