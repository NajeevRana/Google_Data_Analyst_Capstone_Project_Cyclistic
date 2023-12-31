library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(anytime)
library(stringr)
library(janitor)

## Load data into R for analysis

df1<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202207-divvy-tripdata.csv")
df2<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202208-divvy-tripdata.csv")
df3<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202209-divvy-tripdata.csv")
df4<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202210-divvy-tripdata.csv")
df5<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202211-divvy-tripdata.csv")
df6<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202212-divvy-tripdata.csv")
df7<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202301-divvy-tripdata.csv")
df8<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202302-divvy-tripdata.csv")
df9<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202303-divvy-tripdata.csv")
df10<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202304-divvy-tripdata.csv")
df11<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202305-divvy-tripdata.csv")
df12<- read.csv("C:/Users/Najeev/google-data-analytics-capstone-cyclistic-data/202306-divvy-tripdata.csv")

#compare datasets columns
compare_df_cols(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

##Bind Datasets
trek<- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

## Removing "start_station_id" and "end_station_id" columns because both character and Numeric data enter in those columns
trek<- trek %>%
  select(-c(start_station_id, end_station_id))

##Change the stated_at and ended_at datatype to date-time format.
trek$started_at <- anytime(trek$started_at, tz="America/Chicago")
trek$ended_at <- anytime(trek$ended_at, tz="America/Chicago")


##added new column week_of_days
trek<- mutate(trek,week_of_days = weekdays(started_at))

##more column added "date","day","month",year","trip_duration"
trek<- mutate(trek,date = as.Date(started_at))
trek<-mutate(trek,day = format(started_at,"%d"))
trek<- mutate(trek,month= format(started_at,"%m"))
trek<- mutate(trek,year= format(started_at,"%y"))
trek<- mutate(trek,trip_durattion= difftime(ended_at,started_at,units = "mins"))

##verify the column added
colnames(trek)

## Remove the column started_at and ended_at
trek<- subset(trek, select = -c(started_at,ended_at))

##understanding of data
str(trek)

##changing trip_duration to numeric
trek$trip_durattion <- as.numeric(as.character(trek$trip_durattion))

## confirming trip_duration is numeric
is.numeric(trek$trip_durattion)

## Data cleaning 
## Remove unnecessary white space
trek %>% mutate(across(where(is.character), str_squish))

##check NA for data set 
sum(is.na(trek))

##Remove rows with NA
trek <- na.omit(trek)

##Verify removal of rows with NA
sum(is.na(trek))

## check duplicate ride_id
sum(duplicated(trek$ride_id))

##check for rows where trip_durattion is less than 0
filter(trek,trip_durattion<0)

##remove rows where trip_durattion is less than 0
trek<-subset(trek,trip_durattion>0)

##check the data for better understanding
glimpse(trek)

##Analyze the data
## statistical summary of data
summary(trek$trip_durattion)

##Arrange week of days in order
trek$week_of_days<- ordered(trek$week_of_days,levels=c("Sunday","Monday","Tuesday","Wednesday","Thrusday","Friday","Saturday"))

##Compare mean,median,max ride duration in minutes for members and casual users 

aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN = mean)
aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN= median)
aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN = max)

##Analyzed ridership data by user type and weekday - average duration in minutes 

trek %>% mutate(week_of_days = wday(date,label = TRUE)) %>% 
  group_by(member_casual, week_of_days) %>%
  summarise (number_of_rides = n()
             ,average_duration = mean(trip_durattion)) %>%
  arrange(member_casual,week_of_days)

##Generate seasonal trip chart by rider classification

trek %>%
  ggplot(aes(x = date,fill=member_casual,color=member_casual)) +
  geom_area(stat = 'bin', position = 'identity',alpha = 0.5) + 
  ggtitle("Seasonal Usage By Rider Classification") +
  xlab('Date')+
  ylab('Number of Rides') +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_bw()

##Generate day of week trip view and user type by rider classification

trek%>%
  ggplot(aes(x= wday(date,label=TRUE),fill = member_casual))+
  geom_bar(position= 'dodge')+
  ggtitle('Days of Week Usage by Rider and user type') +
  geom_text(stat = 'count',
            aes(label = scales::comma(..count..)), 
            position = position_dodge(width = 0.85),
            hjust = 1.25,
            angle = 90) +
  xlab('Day of Week') + 
  ylab('Number of Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  theme_bw()


##Visualized the number of rides by month and user type.

trek %>% 
  mutate(month) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  ggtitle(label = "Number of Rides by Month and User Type") +
  xlab('Months') + 
  ylab('Number of Rides') +
  scale_x_discrete(labels = month.abb, expand = expansion(add = c(0,0))) +
  scale_y_continuous(label = scales::label_comma())


##Visualized average duration by weekday and user type.

trek %>% 
  mutate(week_of_days = wday(date, label = TRUE)) %>% 
  group_by(member_casual, week_of_days) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, week_of_days)  %>% 
  ggplot(aes(x = week_of_days, y = average_duration, fill = member_casual)) +
  ggtitle(label = "Average Duration by Weekday and User Type") +
  geom_col(position = "dodge") +
  xlab('Day Of Week') + 
  ylab('Average Duration') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0)))

## visualized the number of rides by year and user type

trek %>% 
  mutate(year = "2023") %>% 
  group_by(member_casual, year) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, year)  %>% 
  ggplot(aes(x = year, y = number_of_rides, fill = member_casual)) +
  ggtitle(label = "Number Of Rides by Year and User Type") +
  geom_col(position = "dodge") +
  xlab('Year') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0))) +
  geom_text(aes(label = number_of_rides), hjust = .5, vjust = 0, position = position_dodge(.9))

## visualized the number of rides by year and bike type

trek %>% 
  mutate(year = "2023") %>% 
  group_by(rideable_type, year) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(rideable_type, year)  %>% 
  ggplot(aes(x = year, y = number_of_rides, fill = rideable_type)) +
  ggtitle(label = "Number of Rides by Year and Bike Type") +
  geom_col(position = "dodge") +
  xlab('Year') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0))) +
  geom_text(aes(label = number_of_rides), hjust = .5, vjust = -.1, position = position_dodge(.9))

##Visualized the number of rides by weekday and bike type

trek %>% 
  mutate(week_of_days = wday(date, label = TRUE)) %>% 
  group_by(rideable_type, week_of_days) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(rideable_type, week_of_days)  %>% 
  ggplot(aes(x = week_of_days, y = number_of_rides, fill = rideable_type)) +
  ggtitle(label = "Number of Rides by Week of Day and Bike Type") +
  geom_col(position = "dodge") +
  xlab('Week Of Day') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0)))


